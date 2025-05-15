library(move2)
library(terra)
library(tidyterra)
library(sf)
library(ggplot2)
library(elevatr)
library(tidyverse)
library(purrr)
library(data.table)
library(broom)
library(tools)
library(ggpubr)


POPULATION <- "population"
INDIVIDUAL <- "individual"


get_raster_data <- function(locations, move_data, rasters, user_provided_rasters) {
  logger.info("Getting raster data...")
  col_names <- unlist(lapply(rasters, names))


  # Create empty data.table
  dt <- data.table(matrix(NA, nrow = nrow(locations), ncol = length(col_names)))
  setnames(dt, col_names)

  for (i in 1:length(rasters)) {
    current_raster <- rasters[[i]]

    # Extract values for all layers at once if possible
    # First check if we need special handling for any layer
    layer_names <- names(current_raster)
    special_layers <- layer_names %in% c("tree_canopy_cover", "LC")

    if (!any(special_layers) | user_provided_rasters) {
      # Standard extraction for all layers at once
      extracted_values <- terra::extract(current_raster, locations)

      # Add each column to the result
      extracted_col_names <- colnames(extracted_values)
      for (j in 1:length(extracted_col_names)) {
        lyr_name <- extracted_col_names[j]
        dt[[lyr_name]] <- extracted_values[[lyr_name]]
      }
    } else {
      # Handle special layers individually
      for (j in 1:nlyr(current_raster)) {
        lyr_name <- names(current_raster)[j]

        if (lyr_name == "tree_canopy_cover") {
          # Extract with bilinear method for continuous data
          values <- terra::extract(current_raster[[j]], locations, method = "bilinear")
          dt[[lyr_name]] <- as.numeric(base::scale(values[, 1]))
        } else if (lyr_name == "LC") {
          # Handle categorical data
          values <- terra::extract(current_raster[[j]], locations)
          dt[[lyr_name]] <- as.factor(values[, 1])
        } else {
          # Standard extraction
          values <- terra::extract(current_raster[[j]], locations)
          dt[[lyr_name]] <- values[, 1]
        }
      }
    }
  }

  return(list(
    raster_data = dt,
    columns = col_names
  ))
}




get_projection_methods <- function(rast_obj,
                                   categorical_method = "near",
                                   continuous_methods = c("bilinear", "cubic"),
                                   max_unique_values = 20,
                                   integer_threshold = 0.99,
                                   sample_size = 1000) {
  # Ensure input is a SpatRaster
  if (!inherits(rast_obj, "SpatRaster")) {
    stop("Input must be a terra SpatRaster object")
  }

  # Get number of layers
  n_layers <- nlyr(rast_obj)

  # Initialize output lists
  layer_types <- vector("character", n_layers)
  projection_methods <- vector("character", n_layers)
  reasons <- vector("list", n_layers)

  # Process each layer
  for (i in 1:n_layers) {
    layer_name <- names(rast_obj)[i]
    if (is.null(layer_name) || layer_name == "") {
      layer_name <- paste("Layer", i)
    }

    # Use a single layer subset
    current_layer <- rast_obj[[i]]

    # Check if it has category information
    cat_info <- cats(current_layer)
    has_cats <- !is.null(cat_info) && !is.null(cat_info[[1]])

    if (has_cats) {
      layer_types[i] <- "categorical"
      projection_methods[i] <- categorical_method
      reasons[[i]] <- "Has explicit category table"
      next # Skip to next layer if categorical is confirmed
    }

    # Get data type
    dt <- datatype(current_layer)

    # Sample values to avoid memory issues with large rasters
    if (ncell(current_layer) > sample_size) {
      # Create a sample of cell indices
      set.seed(123 + i) # Different seed for each layer for variety
      cell_sample <- sample(1:ncell(current_layer), size = sample_size)
      # Extract values manually using cell numbers
      layer_values <- values(current_layer)[cell_sample]
    } else {
      layer_values <- values(current_layer)
    }

    # Remove NA values for analysis
    layer_values <- layer_values[!is.na(layer_values)]

    # If we have too few values after NA removal, assume continuous to be safe
    if (length(layer_values) < 10) {
      layer_types[i] <- "continuous"
      projection_methods[i] <- continuous_methods[1]
      reasons[[i]] <- "Too few non-NA values to make a determination, assuming continuous"
      next
    }

    # Count unique values
    unique_vals <- unique(layer_values)
    n_unique <- length(unique_vals)

    # Check if all values are integers
    integers_proportion <- sum(layer_values == floor(layer_values)) / length(layer_values)
    all_integers <- integers_proportion >= integer_threshold

    # Check for common categorical data types
    is_integer_type <- grepl("INT", dt)

    # Make the determination based on multiple factors
    is_categorical <- FALSE
    reason_parts <- character(0)

    if (n_unique <= max_unique_values) {
      is_categorical <- TRUE
      reason_parts <- c(reason_parts, paste("Few unique values:", n_unique, "≤", max_unique_values))
    }

    if (all_integers) {
      reason_parts <- c(reason_parts, paste("Values are integers:", round(integers_proportion * 100, 1), "% integer values"))

      if (!is_categorical) {
        is_categorical <- all_integers
      }
    }

    if (is_integer_type) {
      reason_parts <- c(reason_parts, paste("Integer data type:", dt))

      if (!is_categorical) {
        is_categorical <- is_integer_type
      }
    }

    # Set the layer type and method based on determination
    if (is_categorical) {
      layer_types[i] <- "categorical"
      projection_methods[i] <- categorical_method
    } else {
      layer_types[i] <- "continuous"
      projection_methods[i] <- continuous_methods[1] # Default to first continuous method
      reason_parts <- c(reason_parts, paste(
        "Likely continuous data:",
        "unique values =", n_unique,
        ", integer proportion =", round(integers_proportion, 2)
      ))
    }

    reasons[[i]] <- paste(reason_parts, collapse = "; ")
  }

  # Compile results into a data frame
  results <- data.frame(
    layer = 1:n_layers,
    name = names(rast_obj),
    type = layer_types,
    recommended_method = projection_methods,
    reason = unlist(lapply(reasons, function(x) paste(x, collapse = ", ")))
  )

  # Also return as a named vector for direct use with project()
  method_vector <- projection_methods
  names(method_vector) <- names(rast_obj)

  return(list(
    results_table = results,
    method_vector = method_vector
  ))
}


get_projected_rasters <- function(extent, raster_list, move_data) {
  rast_list_cropped <- lapply(raster_list, function(rast) {
    if (!same.crs(move_data, rast)) {
      logger.info(str_interp(
        "rast ${names(rast)} has crs ${crs(rast)}, projecting to move data crs ${crs(move_data)}"
      ))

      proj_ext <- terra::project(extent, from = crs(move_data), to = crs(rast))
      cropped_rast <- terra::crop(rast, proj_ext)

      methods_info <- get_projection_methods(cropped_rast)

      layers <- list()
      for (i in 1:nlyr(cropped_rast)) {
        layer <- cropped_rast[[i]]
        method <- methods_info$method_vector[i]
        layers[[i]] <- terra::project(layer, crs(move_data), method = method)
      }

      proj_rast <- do.call(c, layers)

      return(proj_rast)
    }


    cropped_rast <- terra::crop(rast, extent)
    return(cropped_rast)
  })

  return(rast_list_cropped)
}


get_rasters <- function(extent, raster_file, raster_cat_file, move_data) {
  logger.info("Getting rasters...")
  
  rast1_path <- getAuxiliaryFilePath("raster_file", fallbackToProvidedFiles = FALSE)
  rast2_path <- getAuxiliaryFilePath("raster_cat_file", fallbackToProvidedFiles = FALSE)


  if (!is.null(rast1_path) | !is.null(rast2_path)) {
    # user-provided files
    user_provided_rasters <- TRUE

    rast_list <- list()
    if (!is.null(rast1_path)) {
      if (file_ext(rast1_path) != "tif") {
        stop("Raster with extension .tif not found, please make sure you uploaded a tif file.")
      }

      rast1 <- terra::rast(rast1_path)
      rast_list[[length(rast_list) + 1]] <- rast1
    }

    if (!is.null(rast2_path)) {
      if (file_ext(rast2_path) != "tif") {
        stop("Raster with extension .tif not found, please make sure you uploaded a tif file.")
      }

      rast2 <- terra::rast(rast2_path)
      rast_list[[length(rast_list) + 1]] <- rast2
    }
  } else {
    rast_lc_tree_canopy <- terra::rast(getAuxiliaryFilePath("raster_lc_tcc_file"), "raster.tif")
    rast_ghm <- terra::rast(getAuxiliaryFilePath("raster_hm_file"), "raster_hm.tif")

    rast_list <- list(rast_lc_tree_canopy, rast_ghm)
    user_provided_rasters <- FALSE
  }



  rast_list_proj <- get_projected_rasters(
    extent = extent,
    raster_list = rast_list,
    move_data = move_data
  )

  return(list(
    user_provided_rasters = user_provided_rasters,
    rasters = rast_list_proj
  ))
}



fit_model <- function(model_df, model_variables, user_provided_rasters = FALSE) {
  logger.info("Fitting model...")
  custom_vars <- paste0(model_variables, collapse = " + ")
  formula_str <- stringr::str_interp(
    "case ~ delx + dely + distxy + ${custom_vars}"
  )

  model_args <- list(
    formula = as.formula(formula_str),
    data = model_df,
    family = binomial(link = "logit")
  )

  model <- do.call(glm, args = model_args)

  return(model)
}


plot_rasters <- function(rast_list, move_data, scale, track_id_var) {
  logger.info("Plotting rasters...")
  plot_list <- list()
  move_vector <- as_spatvector(move_data) |>
    filter(
      case == 1
    )
  
  
  # Create plots without individual legends for track_id
  for (i in 1:length(rast_list)) {
    raster <- rast_list[[i]]
    for (j in 1:nlyr(raster)) {
      layer <- raster[[j]]
      plot <- ggplot() +
        geom_spatraster(data = terra::crop(layer, move_vector)) +
        theme_bw() +
        scale_fill_hypso_c() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        coord_sf(expand = TRUE, datum = sf::st_crs(raster)) +
        ggtitle(names(layer)[1])
      
      if (scale == INDIVIDUAL) {
        # Add vectors but hide their legend
        plot <- plot +
          geom_spatvector(data = move_vector,
                          aes(color = get(track_id_var)),
                          show.legend = FALSE)
      } else {
        plot <- plot +
          geom_spatvector(data = move_vector,
                          show.legend = FALSE)
      }
      plot_list[[length(plot_list) + 1]] <- plot
    }
  }
  
  plots_arranged <- ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = 2,
    nrow = ceiling(length(plot_list)/2)
  )

  if(scale == INDIVIDUAL) {
    # Create legend with better formatting
    legend_only <- ggplot() +
      geom_spatvector(data = move_vector, aes(color = get(track_id_var))) +
      labs(color = track_id_var) +
      theme_minimal()
    
    # Get the legend
    common_legend <- ggpubr::get_legend(legend_only)
    
    # Arrange plots with the legend at the bottom
    final_arrangement <- ggpubr::ggarrange(
      plots_arranged,
      common_legend,
      ncol = 1,
      nrow = 2,
      heights = c(0.9, 0.1) # 90% for plots, 10% for legend
    )
  } else {
    final_arrangement <- plots_arranged
  }
  
  return(final_arrangement)
}


plot_model <- function(model_plot_df, scale, track_id_var) {
  model_plot <- ggplot(model_plot_df) +
    geom_point(aes(y = term, x = estimate), col = "blue") +
    geom_linerange(aes(
      y = term,
      xmin = conf.low,
      xmax = conf.high
    )) +
    labs(y = "Variable", x = "Coefficient Estimate") +
    theme_bw()


  if (scale == INDIVIDUAL) {
    model_plot <- model_plot +
      facet_wrap(~ get(track_id_var), scales = "free")
  }

  return(model_plot)
}

# Optimized get_nonraster_data function with batching and progress bar
get_elevation_data <- function(locations, move_data) {
  logger.info("Getting elevation data...")
  
  # Convert to data.table directly
  mut_locations <- locations |>
    as.data.frame() |>
    rename(
      x = X,
      y = Y
    )


  # Create batches to avoid overwhelming the API
  batch_size <- 10000
  n_locations <- nrow(mut_locations)
  n_batches <- ceiling(n_locations / batch_size)

  # Pre-allocate the result vector - much faster than building a list
  all_elevations <- numeric(n_locations)

  for (i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_locations)

    logger.info(
      stringr::str_interp("Getting elevation batch ${i}/${n_batches} (rows ${start_idx}-${end_idx})")
    )

    # Extract the batch using data.table syntax for speed
    batch_locations <- mut_locations[start_idx:end_idx, ]

    # Get elevation for this batch
    batch_elev <- get_elev_point(
      batch_locations,
      prj = sf::st_crs(move_data),
      units = "meters",
      src = "aws"
    )$elevation

    # Directly assign to the pre-allocated vector instead of building a list
    all_elevations[start_idx:end_idx] <- batch_elev
  }

  # Scale once at the end
  elev_dat <- base::scale(all_elevations)[, 1]

  return(elev_dat)
}


convert_categorical_cols <- function(df,
                                     max_unique_values = 20,
                                     integer_threshold = 0.99,
                                     include_character = TRUE) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  # Create a copy of the data frame to avoid modifying the original
  result_df <- df

  # Process each column
  for (col_name in names(df)) {
    col_data <- df[[col_name]]

    # Skip columns that are already factors
    if (is.factor(col_data)) {
      next
    }

    # Skip columns that are not numeric or character
    if (!is.numeric(col_data) && !is.character(col_data)) {
      next
    }

    # For character columns, convert to factor if requested
    if (is.character(col_data)) {
      if (include_character) {
        # Check if the number of unique values is reasonable
        unique_vals <- unique(col_data)
        n_unique <- length(unique_vals)

        if (n_unique <= max_unique_values && n_unique > 1) {
          result_df[[col_name]] <- as.factor(col_data)
        }
      }
      next
    }

    # For numeric columns, apply more checks

    # Remove NA values for analysis
    non_na_data <- col_data[!is.na(col_data)]

    # If we have too few values after NA removal, skip
    if (length(non_na_data) < 10) {
      next
    }

    # Count unique values
    unique_vals <- unique(non_na_data)
    n_unique <- length(unique_vals)

    # Skip binary (0/1) columns
    is_binary <- n_unique <= 2 && all(unique_vals %in% c(0, 1))
    if (is_binary) {
      next
    }

    # Check if values are integers
    integers_proportion <- sum(non_na_data == floor(non_na_data)) / length(non_na_data)
    all_integers <- integers_proportion >= integer_threshold

    # Convert to factor if the column appears categorical
    if (n_unique <= max_unique_values && n_unique > 1 && all_integers) {
      result_df[[col_name]] <- as.factor(col_data)
    }
  }

  return(result_df)
}


get_model_data <- function(locations, move_data, rasters,
                           user_provided_rasters, track_id_var) {
  raster_data_result <- get_raster_data(
    locations = locations,
    move_data = move_data,
    rasters = rasters,
    user_provided_rasters = user_provided_rasters
  )

  model_data <- raster_data_result$raster_data |>
    mutate(
      location_x = locations[, 1],
      location_y = locations[, 2],
      delx = location_x - mean(location_x),
      dely = location_y - mean(location_y),
      distxy = sqrt((delx)^2 + (dely)^2),
      case = move_data$case,
      !!track_id_var := move_data[[track_id_var]]
    )

  model_variables <- raster_data_result$columns

  if (!user_provided_rasters) {
    # get default elevation data
    model_data$elevation <- get_elevation_data(locations = locations, move_data = move_data)
    model_variables <- c(model_variables, "elevation")
  }

  model_df <- convert_categorical_cols(model_data)

  return(list(
    model_df = model_df,
    model_variables = model_variables
  ))
}


rFunction <- function(data, raster_file = NULL, raster_cat_file = NULL,
                      scale) {
  track_id_var <- mt_track_id_column(data)
  rast_ext <- ext(as.vector(ext(data)) + c(-0.5, 0.5, -0.5, 0.5))

  locations <- sf::st_coordinates(data)
  raster_list_result <- get_rasters(
    extent = rast_ext,
    raster_file = raster_file,
    raster_cat_file = raster_cat_file,
    move_data = data
  )

  rasters <- raster_list_result$rasters


  user_provided_rasters <- raster_list_result$user_provided_rasters


  model_data <- get_model_data(
    locations = locations,
    move_data = data,
    rasters = rasters,
    user_provided_rasters = user_provided_rasters,
    track_id_var = track_id_var
  )


  if (scale == POPULATION) {
    model <- fit_model(
      model_df = model_data$model_df,
      model_variables = model_data$model_variables,
      user_provided_rasters = user_provided_rasters
    )

    model_plot_df <- broom::tidy(model, conf.int = TRUE)
  } else {
    model_plot_df <- model_data$model_df |>
      nest(nested_data = -!!sym(track_id_var)) |>
      mutate(results = purrr::map(nested_data, function(ind_data_df) {
        model <- fit_model(
          model_df = ind_data_df, model_variables = model_data$model_variables,
          user_provided_rasters = user_provided_rasters
        )
        coefs <- broom::tidy(model, conf.int = TRUE)
        return(coefs)
      })) |>
      unnest(results) |>
      dplyr::select(-nested_data)
  }


  if (!user_provided_rasters) {
    logger.info("Getting elevation raster...")
    rasters$elevation <- terra::rast(
      get_elev_raster(rasters[[1]],
        src = "aws",
        prj = st_crs(data),
        z = 4,
        clip = "bbox",
        override_size_check = TRUE
      )
    )
    
    names(rasters$elevation) <- c("elevation")
  }

  model_plot <- plot_model(
    model_plot_df = model_plot_df,
    scale = scale,
    track_id_var = track_id_var
  )

  raster_plots <- plot_rasters(
    rast_list = rasters,
    move_data = data,
    scale = scale,
    track_id_var = track_id_var
  )

  ggsave(model_plot,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "model_coeffcient_plot.jpeg"),
    width = 9, height = 6, units = "in", dpi = 300
  )

  ggsave(raster_plots,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "raster_plot.jpeg"),
    width = 12, height = 14
  )

  write.csv(model_plot_df,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "rsf_coefficient_output.csv"),
    row.names = FALSE
  )

  return(data)
}

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


get_raster_data <- function(locations, move_data, rasters,
                            include_percent_tree_cover, include_elevation) {
  logger.info("Extracting raster data...")

  if (is_empty(rasters)) {
    logger.info("No rasters were provided by the user nor did the user select any default environmental variables")

    # Create placeholder data.table
    dt <- data.table(case = move_data$case)
    col_names <- c()
  } else {
    col_names <- unlist(lapply(rasters, names))

    # Create empty data.table
    dt <- data.table(matrix(NA, nrow = nrow(locations), ncol = length(col_names)))

    for (i in 1:length(rasters)) {
      current_raster <- rasters[[i]]

      scale_data <- (("percent_tree_cover" %in% names(current_raster)) & include_percent_tree_cover) |
        (("elevation" %in% names(current_raster)) & include_elevation)

      if (scale_data) {
        name <- names(current_raster)[1]

        # Extract with bilinear method for continuous data
        values <- terra::extract(current_raster, locations, method = "bilinear")
        dt[[str_interp("${name}_scaled")]] <- as.numeric(base::scale(values[, 1]))

        col_names <- gsub(
          name,
          str_interp("${name}_scaled"),
          col_names
        )
      } else {
        # if not special case, then just extract same as all others
        extracted_values <- terra::extract(current_raster, locations)

        # Add each column to the result
        extracted_col_names <- colnames(extracted_values)
        for (j in 1:length(extracted_col_names)) {
          lyr_name <- extracted_col_names[j]
          dt[[lyr_name]] <- extracted_values[[lyr_name]]
        }
      }
    }
  }


  return(list(
    raster_data = dt,
    columns = col_names
  ))
}




is_categorical_layer <- function(layer,
                                 max_unique_values = 20,
                                 sample_size = 1000) {
  # Initialize result variables
  is_categorical <- FALSE
  reason_parts <- character(0)

  # Check if it has category information
  cat_info <- cats(layer)
  has_cats <- !is.null(cat_info) && !is.null(cat_info[[1]])

  if (has_cats) {
    is_categorical <- TRUE
    reason_parts <- "Has explicit category table"
    return(list(is_categorical = is_categorical, reasons = reason_parts))
  }

  # Get data type
  dt <- datatype(layer)

  # Sample values to avoid memory issues with large rasters
  if (ncell(layer) > sample_size) {
    # Create a sample of cell indices
    set.seed(123) # Use a fixed seed for reproducibility
    cell_sample <- sample(1:ncell(layer), size = sample_size)
    # Extract values manually using cell numbers
    layer_values <- values(layer)[cell_sample]
  } else {
    layer_values <- values(layer)
  }

  # Remove NA values for analysis
  layer_values <- layer_values[!is.na(layer_values)]

  # If we have too few values after NA removal, assume continuous to be safe
  if (length(layer_values) < 10) {
    reason_parts <- "Too few non-NA values to make a determination, assuming continuous"
    return(list(is_categorical = FALSE, reasons = reason_parts))
  }

  # Count unique values
  unique_vals <- unique(layer_values)
  n_unique <- length(unique_vals)

  # Check for common categorical data types
  is_integer_type <- grepl("INT", dt)

  # Make the determination based on multiple factors
  if (n_unique <= max_unique_values) {
    is_categorical <- TRUE
    reason_parts <- c(reason_parts, paste("Few unique values:", n_unique, "≤", max_unique_values))
  }


  if (is_integer_type) {
    reason_parts <- c(reason_parts, paste("Integer data type:", dt))
    if (!is_categorical) {
      is_categorical <- is_integer_type
    }
  }

  if (!is_categorical) {
    reason_parts <- c(reason_parts, paste(
      "Likely continuous data:",
      "unique values =", n_unique
    ))
  }

  return(list(is_categorical = is_categorical, reasons = paste(reason_parts, collapse = "; ")))
}

get_projection_methods <- function(rast_obj,
                                   categorical_method = "near",
                                   continuous_methods = c("bilinear", "cubic"),
                                   max_unique_values = 20,
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

    # Use the helper function to determine if the layer is categorical
    result <- is_categorical_layer(
      current_layer,
      max_unique_values = max_unique_values,
      sample_size = sample_size
    )

    if (result$is_categorical) {
      layer_types[i] <- "categorical"
      projection_methods[i] <- categorical_method
    } else {
      layer_types[i] <- "continuous"
      projection_methods[i] <- continuous_methods[1] # Default to first continuous method
    }

    reasons[[i]] <- result$reasons
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
        layer_type <- methods_info$results_table$type[i]
        current_layer <- terra::project(layer, crs(move_data), method = method)

        if (layer_type == "categorical") {
          layers[[i]] <- as.factor(current_layer)
        } else {
          layers[[i]] <- current_layer
        }
      }

      proj_rast <- do.call(c, layers)

      return(proj_rast)
    } else {
      cropped_rast <- terra::crop(rast, extent)
      layers <- list()

      for (i in 1:nlyr(cropped_rast)) {
        layer <- cropped_rast[[i]]
        if (is_categorical_layer(layer)$is_categorical) {
          layers[[i]] <- as.factor(layer)
        } else {
          layers[[i]] <- layer
        }
      }

      cropped_rasters <- do.call(c, layers)
      return(cropped_rasters)
    }
  })

  return(rast_list_cropped)
}


get_rasters <- function(extent, move_data, include_percent_tree_cover,
                        include_land_cover_type, include_global_human_modification,
                        include_elevation) {
  logger.info("Getting rasters...")

  rast1_path <- getAuxiliaryFilePath("user_raster_file_1", fallbackToProvidedFiles = FALSE)
  rast2_path <- getAuxiliaryFilePath("user_raster_file_2", fallbackToProvidedFiles = FALSE)

  rast_list <- list()

  if (!is.null(rast1_path) | !is.null(rast2_path)) {
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
  }

  if (include_percent_tree_cover) {
    rast_tree_cover <- terra::rast(getAuxiliaryFilePath("percent_tree_cover"))
    rast_list[[length(rast_list) + 1]] <- rast_tree_cover
  }

  if (include_land_cover_type) {
    rast_landcover_type <- terra::rast(getAuxiliaryFilePath("land_cover_type"))
    rast_list[[length(rast_list) + 1]] <- rast_landcover_type
  }


  if (include_global_human_modification) {
    rast_ghm <- terra::rast(getAuxiliaryFilePath("global_human_modification"))
    rast_list[[length(rast_list) + 1]] <- rast_ghm
  }

  if (include_elevation) {
    logger.info("Getting elevation raster from AWS...")
    ext_df <- data.frame(
      x = c(extent[1], extent[2]),
      y = c(extent[3], extent[4])
    )

    rast_elev <- terra::rast(get_elev_raster(ext_df,
      src = "aws",
      prj = crs(move_data),
      z = 7,
      clip = "bbox",
      override_size_check = TRUE
    ))

    names(rast_elev) <- c("elevation")
    rast_list[[length(rast_list) + 1]] <- rast_elev
  }

  rast_list_proj <- get_projected_rasters(
    extent = extent,
    raster_list = rast_list,
    move_data = move_data
  )

  return(list(
    rasters = rast_list_proj
  ))
}



fit_model <- function(model_df, model_variables) {
  logger.info("Fitting model...")
  custom_vars <- paste0(model_variables, collapse = " + ")

  custom_vars_str <- ifelse(custom_vars == "",
    "",
    stringr::str_interp(" + ${custom_vars}")
  )

  formula_str <- stringr::str_interp(
    "case ~ delx + dely + distxy${custom_vars_str}"
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

  if (is_empty(rast_list)) {
    logger.info("No rasters to plot")
    return(NULL)
  }

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
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        coord_sf(expand = TRUE, datum = sf::st_crs(raster)) +
        ggtitle(names(layer)[1])


      if (scale == INDIVIDUAL) {
        # Add vectors but hide their legend
        plot <- plot +
          geom_spatvector(
            data = move_vector,
            aes(color = get(track_id_var)),
            show.legend = FALSE
          )
      } else {
        plot <- plot +
          geom_spatvector(
            data = move_vector,
            show.legend = FALSE
          )
      }

      if (!is.null(cats(layer)[[1]])) {
        plot <- plot +
          scale_fill_discrete(na.value = "transparent")
      } else {
        plot <- plot +
          scale_fill_continuous(na.value = "transparent")
      }
      plot_list[[length(plot_list) + 1]] <- plot
    }
  }

  plots_arranged <- ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = 2,
    nrow = ceiling(length(plot_list) / 2)
  )

  if (scale == INDIVIDUAL) {
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
    geom_linerange(aes(
      y = term,
      xmin = conf.low,
      xmax = conf.high
    )) +
    geom_point(aes(y = term, x = estimate), col = "blue") +
    labs(y = "Variable", x = "Coefficient Estimate") +
    theme_bw()


  if (scale == INDIVIDUAL) {
    model_plot <- model_plot +
      facet_wrap(~ get(track_id_var), scales = "free")
  }

  return(model_plot)
}

# Optimized get_nonraster_data function with batching and progress bar
# get_elevation_data <- function(locations, move_data) {
#   logger.info("Getting elevation data...")
#
#   # Convert to data.table directly
#   mut_locations <- locations |>
#     as.data.frame() |>
#     rename(
#       x = X,
#       y = Y
#     )
#
#
#   # Create batches to avoid overwhelming the API
#   batch_size <- 20000
#   n_locations <- nrow(mut_locations)
#   n_batches <- ceiling(n_locations / batch_size)
#
#   # Pre-allocate the result vector - much faster than building a list
#   all_elevations <- numeric(n_locations)
#
#   for (i in 1:n_batches) {
#     start_idx <- (i - 1) * batch_size + 1
#     end_idx <- min(i * batch_size, n_locations)
#
#     logger.info(
#       stringr::str_interp("Getting elevation batch ${i}/${n_batches} (rows ${start_idx}-${end_idx})")
#     )
#
#     # Extract the batch using data.table syntax for speed
#     batch_locations <- mut_locations[start_idx:end_idx, ]
#
#     # Get elevation for this batch
#     batch_elev <- get_elev_point(
#       batch_locations,
#       prj = sf::st_crs(move_data),
#       units = "meters",
#       src = "aws"
#     )$elevation
#
#     # Directly assign to the pre-allocated vector instead of building a list
#     all_elevations[start_idx:end_idx] <- batch_elev
#   }
#
#   # Scale once at the end
#   elev_dat <- base::scale(all_elevations)[, 1]
#
#   return(elev_dat)
# }

get_model_data <- function(locations, move_data, rasters,
                           include_elevation, include_percent_tree_cover,
                           track_id_var) {
  raster_data_result <- get_raster_data(
    locations = locations,
    move_data = move_data,
    rasters = rasters,
    include_percent_tree_cover = include_percent_tree_cover,
    include_elevation = include_elevation
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

  # if (include_elevation) {
  #   model_data$elevation_scaled <- get_elevation_data(
  #     locations = locations,
  #     move_data = move_data
  #   )
  #   model_variables <- c(model_variables, "elevation_scaled")
  # }


  return(list(
    model_df = model_data,
    model_variables = model_variables
  ))
}


rFunction <- function(data, scale, user_raster_file_1 = NULL, user_raster_file_2 = NULL,
                      include_percent_tree_cover = FALSE,
                      include_land_cover_type = FALSE,
                      include_global_human_modification = FALSE,
                      include_elevation = FALSE) {
  track_id_var <- mt_track_id_column(data)
  rast_ext <- ext(as.vector(ext(data)) + c(-0.5, 0.5, -0.5, 0.5))

  locations <- sf::st_coordinates(data)

  raster_list_result <- get_rasters(
    extent = rast_ext,
    move_data = data,
    include_percent_tree_cover = include_percent_tree_cover,
    include_land_cover_type = include_land_cover_type,
    include_global_human_modification = include_global_human_modification,
    include_elevation = include_elevation
  )

  rasters <- raster_list_result$rasters


  model_data <- get_model_data(
    locations = locations,
    move_data = data,
    rasters = rasters,
    include_elevation = include_elevation,
    include_percent_tree_cover = include_percent_tree_cover,
    track_id_var = track_id_var
  )


  if (scale == POPULATION) {
    model <- fit_model(
      model_df = model_data$model_df,
      model_variables = model_data$model_variables
    )

    model_plot_df <- broom::tidy(model, conf.int = TRUE)
  } else {
    model_plot_df <- model_data$model_df |>
      nest(nested_data = -!!sym(track_id_var)) |>
      mutate(results = purrr::map(nested_data, function(ind_data_df) {
        model <- fit_model(
          model_df = ind_data_df,
          model_variables = model_data$model_variables
        )
        coefs <- broom::tidy(model, conf.int = TRUE)
        return(coefs)
      })) |>
      unnest(results) |>
      dplyr::select(-nested_data)
  }


  # if (include_elevation) {
  #   logger.info("Getting elevation raster at very low resolution...")
  #   rasters$elevation <- terra::rast(
  #     get_elev_raster(rasters[[1]],
  #       src = "aws",
  #       prj = st_crs(data),
  #       z = 3,
  #       clip = "bbox",
  #       override_size_check = TRUE
  #     )
  #   )
  #
  #   names(rasters$elevation) <- c("elevation")
  # }

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

  if (!is.null(raster_plots)) {
    ggsave(raster_plots,
      file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "raster_plot.jpeg"),
      width = 12, height = 14
    )
  }

  write.csv(model_plot_df,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "rsf_coefficient_output.csv"),
    row.names = FALSE
  )

  return(data)
}

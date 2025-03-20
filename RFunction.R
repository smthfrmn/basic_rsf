library(move2)
library(terra)
library(tidyterra)
library(sf)
library(ggplot2)
library(elevatr)
library(tidyverse)
library(progress)
library(cowplot)
library(purrr)


POPULATION <- "population"
INDIVIDUAL <- "individual"


get_raster_data <- function(move_data, rasters) {
  locations <- sf::st_coordinates(move_data)

  mutated_data <- move_data
  all_cols <- NULL
  for (i in 1:length(rasters)) {
    current_raster <- rasters[[i]]
    cols <- names(current_raster)

    if (is.null(all_cols)) {
      all_cols <- cols
    } else {
      all_cols <- c(all_cols, cols)
    }

    for (j in 1:nlyr(current_raster)) {
      lyr <- current_raster[[j]]
      new_data <- NULL
      if (names(lyr) == "tree_canopy_cover") {
        new_data <- as.numeric(scale(terra::extract(current_raster[[j]], locations,
          method = "bilinear"
        )))
      } else {
        new_data <- terra::extract(current_raster[[j]], locations)[,1]
      }

      mutated_data[[names(lyr)]] <- new_data

    }
  }

  return(list(
    raster_data = mutated_data,
    columns = all_cols
  ))
}


get_rasters <- function(extent, raster_file, raster_cat_file) {
  rast1_path <- getAppFilePath("raster_file", fallbackToProvidedFiles = FALSE)
  rast2_path <- getAppFilePath("raster_cat_file", fallbackToProvidedFiles = FALSE)

  if (!is.null(rast1_path) & !is.null(rast2_path)) {
    rast1 <- terra::rast(paste0(rast1_path, "raster.tif"), "raster.tif")
    rast2 <- terra::rast(paste0(rast2_path, "raster_cat.tif"))
    rast_list <- list(rast1, rast2)
    user_provided_rasters <- TRUE
  } else {
    rast_lc_tree_canopy <- terra::rast(paste0(getAppFilePath("raster_file"), "raster.tif"))

    rast_lc_tree_canopy$LC <- as.factor(default_raster$LC)

    rast_ghm <- rast(paste0(getAppFilePath("raster_file"), "raster_hm.tif"))

    rast_list <- list(rast_lc_tree_canopy, rast_ghm)
    user_provided_rasters <- FALSE
  }


  rast_list_cropped <- terra::crop(terra::sprc(rast_list), extent) |>
    as.list()


  return(list(
    user_provided_rasters = user_provided_rasters,
    rasters = rast_list_cropped
  ))
}



fit_model <- function(model_df, model_variables, user_provided_rasters = FALSE) {
  custom_vars <- paste0(model_variables, collapse = " + ")
  formula_str <- stringr::str_interp(
    "case ~ delx + dely + distxy + ${custom_vars}"
  )

  model_args <- list(
    formula = as.formula(formula_str),
    data = model_df,
    family = get_model_family(user_provided_rasters)
  )

  model <- do.call(glm, args = model_args)

  return(model)
}


plot_rasters <- function(rast_list) {
  plot_list <- list()
  for (i in 1:length(rast_list)) {
    raster <- rast_list[[i]]
    plot <- ggplot() +
      geom_spatraster(data = raster) +
      facet_wrap(~lyr) +
      theme_bw() +
      scale_fill_hypso_c() +
      # geom_sf(data = data_sf, size = 0.5) +
      theme(
        legend.position = "right",
        axis.text = element_text(size = 6)
      )

    plot_list[[i]] <- plot
  }

  plots <- cowplot::plot_grid(plotlist = plot_list)
  return(plots)
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
      facet_wrap(~get(track_id_var))
  }

  return(model_plot)
}


get_nonraster_data <- function(move_data) {
  locations <- sf::st_coordinates(move_data) |>
    as.data.frame() |>
    rename(
      x = X,
      y = Y
    )

  elev_dat <- get_elev_point(
    locations,
    prj = sf::st_crs(move_data), units = "meters", src = "aws"
  ) |>
    mutate(
      elevation = base::scale(elevation)
    )

  return(elev_dat)
}


get_model_data <- function(move_data, rasters, user_provided_rasters) {
  raster_data_result <- get_raster_data(
    move_data = move_data,
    rasters = rasters
  )

  locations <- sf::st_coordinates(move_data)
  model_data <- raster_data_result$raster_data |>
    mutate(
      location_x = locations[,1],
      location_y = locations[,2],
      delx = location_x - mean(location_x),
      dely = location_y - mean(location_y),
      distxy = sqrt((delx)^2 + (dely)^2)
    )

  model_variables <- raster_data_result$columns
  other_data <- NULL


  if (!user_provided_rasters) {
    other_data <- get_nonraster_data(move_data = move_data)
    model_data <- cbind(model_data, other_data)
    model_variables <- c(model_variables, colnames(other_data))
  }


  return(list(model_df = as.data.frame(model_data), model_variables = model_variables))
}


get_model_family <- function(user_provided_rasters) {
  if (!user_provided_rasters) {
    return(binomial(link = "logit"))
  }

  return(gaussian)
}


rFunction <- function(data, raster_file = NULL, raster_cat_file = NULL,
                      scale = "population", num_layers = 1) {

  track_id_var <- mt_track_id_column(data)
  rast_ext <- ext(data) + c(-0.5, 0.5, -0.5, 0.5)

  raster_list_result <- get_rasters(
    extent = rast_ext,
    raster_file = raster_file,
    raster_cat_file = raster_cat_file
  )

  rasters <- raster_list_result$rasters
  user_provided_rasters <- raster_list_result$user_provided_rasters


  model_data <- get_model_data(move_data = data, rasters = rasters, user_provided_rasters = user_provided_rasters)


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
    rasters$elevation <- terra::rast(
      get_elev_raster(rasters[[1]], src = "aws", prj = st_crs(4326), units = "meters", z = 9)
    )
  }


  model_plot <- plot_model(model_plot_df = model_plot_df,
                           scale = scale,
                           track_id_var = track_id_var)
  
  raster_plots <- plot_rasters(rast_list = rasters)

  ggsave(model_plot,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "model_coeffcient_plot.jpeg"),
    width = 9, height = 6, units = "in", dpi = 300
  )

  ggsave(raster_plots,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "raster_plot.jpeg"),
    width = 9, height = 6, units = "in", dpi = 300, bg = "white"
  )

  write.csv(model_plot_df,
    file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "rsf_coefficient_output.csv"),
    row.names = FALSE
  )

  return(data)
}

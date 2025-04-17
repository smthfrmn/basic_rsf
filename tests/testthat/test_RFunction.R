source(here("tests/testthat/helper.R"))

SCALES <- c("individual", "population")

test_that("function runs without error with user-provided rasters", {
  toggle_raster_dirs(hide = FALSE)
  

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))

    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      raster_file = "placeholder",
      raster_cat_file = "placeholder"
    ))
  }
  
  withr::defer(toggle_raster_dirs(hide = TRUE))
  
})



test_that("function runs without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)


  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))

    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    ))
  }

  withr::defer(toggle_raster_dirs(hide = TRUE))
  
})


test_that("function runs without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)
  

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    ))
  }
  
  withr::defer(toggle_raster_dirs(hide = FALSE))
  
})



test_that("function gives same output as old version without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)
  

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    
    result <- rFunction(
      data = sample_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    )
    
    new_output <- read_csv(file = here("data/output/rsf_coefficient_output.csv")) |>
      arrange(
        estimate
      )
    
    old_output <- readRDS(file = here(
      str_interp("tests/testthat/data/output/old_model_${scale}_df.rds"))) |>
      mutate(
        term = gsub("lulc\\$|ghm\\$", "", term),
        term = gsub("forest_cover", "tree_canopy_cover", term)
      ) |>
      arrange(
        estimate
      )
    
    expect_equal(new_output$term, old_output$term)
    expect_equal(new_output$estimate, old_output$estimate)
    expect_equal(new_output$p.value, old_output$p.value)

  }
  
  withr::defer(toggle_raster_dirs(hide = FALSE))
  
})


toggle_proj_raster <- function(hide) {
  
  if (!hide) {
    # make the projected raster visible
    file.rename(here("data/local_app_files/uploaded-app-files/raster_cat_file/raster_cat.tif"),
                here("data/local_app_files/uploaded-app-files/raster_cat_file/raster_cat_hide.tif"))
    file.rename(here("tests/testthat/data/raster_cat_proj.tif"),
                here("data/local_app_files/uploaded-app-files/raster_cat_file/raster_cat.tif"))
  } else {
    # hide the projected
    file.rename(here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/raster_cat.tif"),
                here("tests/testthat/data/raster_cat_proj.tif"))
    file.rename(here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/raster_cat_hide.tif"),
                here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/raster_cat.tif"))

  }
}



sample_data <- test_data(str_interp("input_population.rds"))
scale <- "population"
rFunction(
  data = test_data,
  scale = scale,
  raster_file = NULL,
  raster_cat_file = NULL
)


methods_info <- get_projection_methods(test_rast)

layers <- list()
for (i in 1:nlyr(test_rast)) {
  browser()
  layer <- test_rast[[i]]
  method <- methods_info$method_vector[i]
  layers[[i]] <- terra::project(layer, "EPSG:3857", method = method)
}

test_that("function projects crs for unmatching move data and raster", {
  toggle_raster_dirs(hide = TRUE)
  make_proj_rast_visible(hide = FALSE)

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = test_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    ))
  }
  
  withr::defer(make_proj_rast_visible(hide = TRUE))
})

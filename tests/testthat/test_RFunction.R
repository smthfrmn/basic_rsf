source(here("tests/testthat/helper.R"))


test_that("function runs without error with user-provided rasters", {
  toggle_raster_dirs(hide = FALSE)
  
  scales <- c("individual", "population")

  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))

    expect_no_error(rFunction(
      data = test_data,
      scale = scale,
      raster_file = "placeholder",
      raster_cat_file = "placeholder"
    ))
  }
  
  toggle_raster_dirs(hide = TRUE)
  
})



test_that("function runs without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)

  scales <- c("individual")

  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))

    expect_no_error(rFunction(
      data = test_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    ))
  }

  toggle_raster_dirs(hide = FALSE)
})


test_that("function runs without error without user-provided rasters", {
  # toggle_raster_dirs(hide = TRUE)
  
  scales <- c("individual", "population")
  
  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = test_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    ))
  }
  
  toggle_raster_dirs(hide = FALSE)
})



test_that("function gives same output as old version without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)
  
  scales <- c("individual", "population")
  
  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))
    
    result <- rFunction(
      data = test_data,
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
  
  toggle_raster_dirs(hide = FALSE)
})

source(here("tests/testthat/helper.R"))


test_that("function runs without error with user-provided rasters", {
  
  scales <- c("individual", "population")
  
  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = test_data,
      scale = scale,
      raster_file = "placeholder",
      raster_cat_file = "placeholder"))
  }
})


test_that("function runs without error without user-provided rasters", {
  
  # TODO: move the files away...
  scales <- c("individual", "population")
  
  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = test_data,
      scale = scale,
      raster_file = "placeholder",
      raster_cat_file = "placeholder"))
  }
})

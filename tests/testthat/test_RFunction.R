source(here("tests/testthat/helper.R"))

SCALES <- c("population", "individual")


test_that("function runs without error with user-provided rasters", {
  activate_raster_files(files_to_show = "user_provided")

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))

    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      user_raster_file_1 = "placeholder",
      user_raster_file_2 = "placeholder"
    ))
  }

})





test_that("function runs without error with user-provided rasters and default variables", {
  activate_raster_files(files_to_show = "user_provided")
  
  
  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      user_raster_file_1 = NULL,
      user_raster_file_2 = NULL,
      include_percent_tree_cover = TRUE,
      include_land_cover_type = TRUE,
      include_global_human_modification = TRUE,
      include_elevation = FALSE
    ))
  }
})


test_that("function runs without error without user-provided rasters and without default variables", {
  activate_raster_files(files_to_show = "fallback")
  
  
  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      user_raster_file_1 = NULL,
      user_raster_file_2 = NULL
    ))
  }
})


# test_that("function gives same output as old version without error without user-provided rasters", {
#  activate_raster_files(files_to_show = "fallback")
# 
# 
#   for (i in 1:length(SCALES)) {
#     scale <- SCALES[i]
#     sample_data <- test_data(str_interp("input_${scale}.rds"), thin = FALSE)
# 
#     result <- rFunction(
#       data = sample_data,
#       scale = scale,
#       user_raster_file_1 = NULL,
#       user_raster_file_2 = NULL,
#       include_percent_tree_cover = TRUE,
#       include_land_cover_type = TRUE,
#       include_global_human_modification = TRUE,
#       include_elevation = TRUE
#     )
# 
#     new_output <- read_csv(file = here("data/output/rsf_coefficient_output.csv")) |>
#       arrange(
#         estimate
#       )
# 
#     old_output <- readRDS(file = here(
#       str_interp("tests/testthat/data/old_model_${scale}_df.rds")
#     )) |>
#       mutate(
#         term = gsub("lulc", "land_cover_type", term),
#         term = gsub("ghm", "gHM", term),
#         term = gsub("forest_cover", "tree_canopy_cover", term)
#       ) |>
#       arrange(
#         estimate
#       )
#     
#     browser()
#     expect_equal(new_output$term, old_output$term)
#     expect_equal(new_output$estimate, old_output$estimate)
#     expect_equal(new_output$p.value, old_output$p.value)
#   }
# })



test_that("function projects crs for default move data crs and different crs raster", {
  activate_raster_files(files_to_show = "user_provided_projected")

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))

    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      user_raster_file_1 = NULL,
      user_raster_file_2 = NULL
    ))
  }

})


test_that("function projects crs for different move data crs and default crs raster", {
  activate_raster_files(files_to_show = "fallback")

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    sample_data_proj <- sf::st_transform(sample_data, "+init=epsg:3857")

    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      user_raster_file_1 = NULL,
      user_raster_file_2 = NULL,
      include_percent_tree_cover = TRUE
    ))
  }

})


test_that("function runs without error without user-provided rasters and default variables", {
  activate_raster_files(files_to_show = "fallback")
  
  
  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      user_raster_file_1 = NULL,
      user_raster_file_2 = NULL,
      include_percent_tree_cover = TRUE,
      include_land_cover_type = TRUE,
      include_global_human_modification = TRUE,
      include_elevation = TRUE
    ))
  }
})

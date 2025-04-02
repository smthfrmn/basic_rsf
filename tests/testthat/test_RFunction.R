source(here("tests/testthat/helper.R"))


test_that("function runs without error with user-provided rasters", {
  # toggle_raster_dirs(hide = FALSE)
  
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
  # toggle_raster_dirs(hide = TRUE)

  scales <- c("population")

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

  # toggle_raster_dirs(hide = FALSE)
})


test_that("OldFunction", {
  # toggle_raster_dirs(hide = TRUE)
  
  scales <- c("individual", "population")
  
  for (i in 1:length(scales)) {
    scale <- scales[i]
    test_data <- test_data(str_interp("input_${scale}.rds"))
    
    expect_no_error(rFunctionOld(
      data = test_data,
      type_ind = ifelse(scale == "individual", TRUE, FALSE),
      raster_file = NULL,
      raster_cat_file = NULL,
      num_layers = 1
    ))
  }
  
  # toggle_raster_dirs(hide = FALSE)
})

old_model_df <- readRDS(here("data/old_model_df.rds"))


mut <- old_model_df |>
  mutate(
    elevation = elevation[1,],
    LC = lulc$LC,
    gHM = ghm$gHM
  ) |>
  dplyr::select(-c(lulc, ghm))


write.csv(mut, file = here("data/old_model_df.csv"))
new_model_df <- readRDS(here("data/new_model_df.rds"))

write.csv(new_model_df, file = here("data/new_model_df.rds"))


View(old_model_df)

all(mut$LC == new_model_df$LC)


rast_ext_old <- readRDS(here("data/old_rast_ext.rds"))
rast_ext_new <- readRDS(here("data/new_rast_ext.rds"))

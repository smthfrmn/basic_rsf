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
})




test_that("function gives same output as old version without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)


  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"), thin = FALSE)

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
      str_interp("tests/testthat/data/old_model_${scale}_df.rds")
    )) |>
      mutate(
        term = gsub("lulc", "LC", term),
        term = gsub("ghm", "gHM", term),
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



test_that("function projects crs for default move data crs and different crs raster", {
  toggle_raster_dirs(hide = TRUE)
  make_proj_rast_visible(hide = FALSE)

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

  withr::defer(make_proj_rast_visible(hide = TRUE))
})


test_that("function projects crs for different move data crs and default crs raster", {
  toggle_raster_dirs(hide = TRUE)

  for (i in 1:length(SCALES)) {
    scale <- SCALES[i]
    sample_data <- test_data(str_interp("input_${scale}.rds"))
    sample_data_proj <- sf::st_transform(sample_data, "+init=epsg:3857")

    expect_no_error(rFunction(
      data = sample_data,
      scale = scale,
      raster_file = NULL,
      raster_cat_file = NULL
    ))
  }

  withr::defer(make_proj_rast_visible(hide = FALSE))
})

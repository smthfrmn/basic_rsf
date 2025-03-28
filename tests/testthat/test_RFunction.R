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
      raster_cat_file = "placeholder"
    ))
  }
})


toggle_raster_dirs <- function(hide) {
  if (hide) {
    rast1_path <- here("data/local_app_files/uploaded-app-files/raster_file/")
    move_dir(rast1_path, gsub("raster_file", "raster_file_hide", rast1_path, fixed = TRUE))

    rast2_path <- here("data/local_app_files/uploaded-app-files/raster_cat_file/")
    move_dir(rast2_path, gsub("raster_cat_file", "raster_cat_file_hide", rast2_path, fixed = TRUE))
  } else {
    rast1_path <- here("data/local_app_files/uploaded-app-files/raster_file_hide/")
    move_dir(rast1_path, gsub("raster_file_hide", "raster_file", rast1_path, fixed = TRUE))

    rast2_path <- here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/")
    move_dir(rast2_path, gsub("raster_cat_file_hide", "raster_cat_file", rast2_path, fixed = TRUE))
  }
}

move_dir <- function(source_dir, destination_dir) {
  # Normalize paths to ensure consistent format
  source_dir <- normalizePath(source_dir, mustWork = FALSE)
  destination_dir <- normalizePath(destination_dir, mustWork = FALSE)

  # Check if source directory exists
  if (!dir.exists(source_dir)) {
    stop("Source directory does not exist: ", source_dir)
  }

  # Check if source and destination are the same
  if (source_dir == destination_dir) {
    message("Source and destination are the same. No action needed.")
    return(TRUE)
  }

  # Check if destination is a subdirectory of source - this would cause problems
  if (grepl(
    paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", source_dir), "/"),
    paste0(destination_dir, "/")
  )) {
    stop("Destination is a subdirectory of source. This would cause infinite recursion.")
  }

  # Create destination directory if it doesn't exist
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
    message("Created destination directory: ", destination_dir)
  }

  # Try a simpler approach - first try file.rename on the whole directory
  success <- tryCatch(
    {
      file.rename(source_dir, destination_dir)
    },
    error = function(e) {
      return(FALSE)
    }
  )

  if (success) {
    message("Directory successfully moved from ", source_dir, " to ", destination_dir)
    return(TRUE)
  }

  # If simple rename failed, do the file-by-file copy
  # Get list of all files and subdirectories in source
  files <- list.files(source_dir, full.names = TRUE, recursive = TRUE, all.files = TRUE)

  if (length(files) == 0) {
    # Empty directory, just remove source and we're done
    unlink(source_dir, recursive = TRUE)
    message("Empty directory moved (removed source, destination already exists)")
    return(TRUE)
  }

  # Create corresponding paths in the destination
  dest_files <- file.path(
    destination_dir,
    sapply(files, function(f) {
      substr(f, nchar(source_dir) + 2, nchar(f))
    })
  )

  # Create necessary subdirectory structure in destination
  for (dir in unique(dirname(dest_files))) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }

  # Copy all files
  copy_success <- logical(length(files))
  for (i in seq_along(files)) {
    copy_success[i] <- file.copy(from = files[i], to = dest_files[i], overwrite = TRUE)
  }

  # Check if all files were copied successfully
  if (all(copy_success)) {
    # Remove the original directory if all copies succeeded
    unlink(source_dir, recursive = TRUE)
    message("Directory successfully moved from ", source_dir, " to ", destination_dir)
    return(TRUE)
  } else {
    failed_files <- files[!copy_success]
    warning("Failed to copy some files: ", paste(failed_files, collapse = ", "))
    return(FALSE)
  }
}


test_that("function runs without error without user-provided rasters", {
  toggle_raster_dirs(hide = TRUE)

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

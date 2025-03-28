test_data <- function(test_file) {
    test_data_root_dir <- test_path("data")
    readRDS(file = file.path(test_data_root_dir, test_file))
}


move_file <- function(source_path, destination_path) {
  # Check if source file exists
  if (!file.exists(source_path)) {
    stop("Source file does not exist: ", source_path)
  }
  
  # Check if destination directory exists
  dest_dir <- dirname(destination_path)
  if (!dir.exists(dest_dir)) {
    message("Creating destination directory: ", dest_dir)
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Move the file
  success <- file.rename(from = source_path, to = destination_path)
  
  if (success) {
    message("File successfully moved to: ", destination_path)
  } else {
    warning("Failed to move file from ", source_path, " to ", destination_path)
    
    # If file.rename fails (often happens across different drives), try copy and delete
    if (file.copy(from = source_path, to = destination_path)) {
      if (file.remove(source_path)) {
        message("File successfully copied and original deleted")
      } else {
        warning("File copied but failed to delete original at: ", source_path)
      }
    } else {
      stop("Failed to copy file")
    }
  }
  
  return(success)
}
test_data <- function(test_file, thin = TRUE) {
    test_data_root_dir <- test_path("data")
    sample_data <- readRDS(file = file.path(test_data_root_dir, test_file)) 
    
    if (thin) {
      sample_data <- sample_data |>
        filter_track_data(.track_id = c("Prinzesschen..deploy_id.1930120.",
                                      "Prinzesschen..deploy_id.1930121.",
                                      "Prinzesschen..deploy_id.1930122.",
                                      "Prinzesschen..deploy_id.1930123."))
    }
    
    return(sample_data) 
}


activate_raster_files <- function(files_to_show) {
  if (files_to_show == "fallback") {
    Sys.setenv(USER_APP_FILE_UPLOAD_DIR = "I/AM/A/FAKE/PATH/") 
  } else if (files_to_show == "user_provided") {
    Sys.setenv(USER_APP_FILE_UPLOAD_DIR = here("tests/testthat/data/local_app_files/uploaded-app-files/"))
  } else if (files_to_show == "user_provided_projected") {
    Sys.setenv(USER_APP_FILE_UPLOAD_DIR = here("tests/testthat/data/local_app_files/uploaded-app-files-projected/"))
  } else {
    stop(str_interp("Unknown files_to_show ${files_to_show}"))
  }
}
###################################################
## Title: Function file
## Purpose : script for project-specific functions
###################################################


########################
#### Pre-Processing ####
########################

# Function to install and load libraries
load_libraries <- function(packages) {
  
  # Check if required packages are installed. If not Install.
  installed <- packages %in% installed.packages()[, "Package"]
  
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
  
  # Load required packages
  invisible(lapply(packages, library, character.only = TRUE))
}



# Function to load raw data from csv files
load_csv_files <- function(file_path) {
  
  # List all csv files in the folder
  files <- list.files(path = file_path, pattern = "\\.csv$")
  
  # Initialize an empty list to store data frames
  data_list <- list()
  
  # Load all files and store them in the list
  for (i in files) {
    # Full file path
    filepath <- file.path(file_path, i)
    
    # Extract the file name without extension
    name <- substr(i, 1, nchar(i)-nchar(".csv"))
    
    # Read the CSV file and store it in the list
    data_list[[name]] <- read.csv(filepath, encoding = "UTF-8")
  }
  
  # Return the list of data frames
  return(data_list)
}


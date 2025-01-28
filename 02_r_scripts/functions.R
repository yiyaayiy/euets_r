###################################################
## Title: Function file
## Purpose : script for project-specific functions
###################################################


###########################
#### 1. Pre-Processing ####
###########################

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


##########################
#### 2. Visualization ####
##########################


# Function to draw bar plot
eda_bar <- function(data, 
                    x_col, 
                    y_col, 
                    title, 
                    x_label = NULL, 
                    y_label = NULL, 
                    use_fill = TRUE, 
                    fill_col = NULL, 
                    bar_color = "black", 
                    bar_fill = "gray",
                    orientation = "vertical",
                    arrange_bars = NULL) {
  # Optionally reorder the x-axis variable by y-axis values
  if (!is.null(arrange_bars)) {
    data <- data %>%
      mutate(!!sym(x_col) := reorder(!!sym(x_col), !!sym(y_col), 
                                     FUN = if (arrange_bars == "ascending") identity else function(x) -x))
  }
  
  # Base ggplot object
  p <- ggplot(data, aes(x = !!sym(x_col), y = !!sym(y_col)))
  
  # Add fill aesthetics if specified
  if (use_fill && !is.null(fill_col)) {
    p <- p + aes(fill = !!sym(fill_col)) +
      scale_fill_viridis_d(option = "plasma")  # Use a color palette for fill
  }
  
  # Add bar geometry with custom fill
  p <- p +
    geom_bar(stat = "identity", color = bar_color, fill = if (!use_fill) bar_fill else NA) +
    labs(
      title = title,
      x = x_label %||% x_col,  # Use x_label if provided, otherwise default to x_col
      y = y_label %||% y_col,  # Use y_label if provided, otherwise default to y_col
      fill = if (use_fill && !is.null(fill_col)) fill_col else NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = if (use_fill && !is.null(fill_col)) "bottom" else "none"
    )
  
  # Apply coord_flip() for horizontal bars if specified
  if (orientation == "horizontal") {
    p <- p + coord_flip()
  }
  
  return(p)
}



# Function to draw EU maps
eda_EUmap <- function(data, 
                      fill_var, 
                      type = "viridis", 
                      trans = "reverse", 
                      guide = guide_colorbar(
                        barwidth = unit(0.4, "cm"), 
                        barheight = unit(7, "cm"), 
                        ticks = TRUE, 
                        reverse = TRUE
                      ), 
                      x_limits = c(-25, 45), 
                      y_limits = c(35, 70), 
                      ...) {
  # Capture additional arguments for both labs() and scale_fill_continuous()
  dots <- list(...)
  
  # Separate labs() arguments from scale_fill_continuous() arguments
  labs_args <- dots[names(dots) %in% c("title", "subtitle", "caption")]
  scale_fill_args <- dots[!(names(dots) %in% c("title", "subtitle", "caption"))]
  
  # Base plot
  p <- ggplot(data, aes(fill = !!sym(fill_var))) +
    geom_sf() +
    scale_fill_continuous(
      type = type,
      trans = trans,
      guide = guide,
      !!!scale_fill_args  # Pass additional scale_fill_continuous arguments
    ) +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +
    theme_void()
  
  # Add labs() arguments if provided
  if (length(labs_args) > 0) {
    p <- p + do.call(labs, labs_args)
  }
  
  return(p)
}

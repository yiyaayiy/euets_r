###################################################
## Title: Function file for visualization
## Purpose : script for project-specific functions
###################################################


##########################
#### 2. Visualization ####
##########################


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

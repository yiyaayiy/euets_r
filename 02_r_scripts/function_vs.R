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



# Average annual verified emissions by activity categories
eda_activity_emission <- function(
    data, 
    selected_categories, 
    palette_name = "rtist::vanGogh1"
    ) {
  ggplot(
    data = data %>%
      filter(category %in% selected_categories) %>%
      group_by(com_year, category) %>%
      summarise(total = sum(
        com_verified_sum, na.rm = TRUE) / (1e3 * sum(com_verified_sum != 0)), 
        .groups = "drop"
      ), 
    aes(
      x = com_year, 
      y = total, 
      color = category, 
      linetype = category, 
      shape = category, 
      group = category
    )
  ) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    labs(
      title = paste("Average Verified Emissions by Activity Category:\n", paste(selected_categories, collapse = ", ")),
      x = "Year",
      y = "1000 tCO2eq",
      color = "Activity",
      linetype = "Activity",
      shape = "Activity"
    ) +
    paletteer::scale_colour_paletteer_d(palette_name) +
    scale_x_continuous(breaks = seq(2005, 2022, by = 2)) +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    guides(
      color = guide_legend(title.position = "top", title.hjust = 0.5),  # Legend title on top
      linetype = guide_legend(title.position = "top", title.hjust = 0.5),
      shape = guide_legend(title.position = "top", title.hjust = 0.5)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.position = "top"
    )
}
###################################################################
## Title: Master file
## Purpose : master script for cleaning & exploring the EU ETS data
###################################################################


##########################################
#### 0 - Load functions and libraries ####
##########################################

# Load functions
source("02_r_scripts/functions.R")


# Load libraries
load_libraries(
  c(
    "stringdist",  # Data cleaning and manipulation 
    "tidyverse",
    "usethis",     # Git flow
    "gitcreds",
    "rmarkdown",   # R markdown notebook
    "kableExtra",  # R markdown tables
    "paletteer",   # Nice figure color
    "patchwork",   # Combine plots
    "eurostat",    # EU maps
    "sf",
    "giscoR"
    )
)


###########################
#### 1 - Data Cleaning ####
###########################


# Input: all EU ETS raw data from EUTL.INFO (transaction not used)
# Output: 
##  ets_merged.csv
source("02_r_scripts/clean_data.R")



sessionInfo()

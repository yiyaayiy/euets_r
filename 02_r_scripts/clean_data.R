##################################################
## Title: Data Cleaning
## Purpose : script for merging and quick cleaning
##################################################



####======== Main part: load, merge, clean and export ==========####

main <- function(output_path = "01_tidy_data/ets_merged.csv") {
  
  # Step 0: Load csv files in to a list of data frames
  dtls <- load_csv_files("00_raw_data/EUETS_INFO")
  
  # Step 1: Clean selected data frames
  dtls <- clean_selected_data(dtls)
  
  # Step 2: Merge account and account holder
  acc_acch <- merge_account_accholder(dtls)
  
  # Step 3: Merge installation and compliance
  ins_com <- merge_installation_and_compliance(dtls)
  
  # Step 4: Final merging and cleaning
  merged <- merge_and_clean(acc_acch, ins_com)
  
  # Step 5: Export the cleaned dataset
  write.csv(merged, file = output_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Return the final data frame
  return(merged)
}




####======== Functions ==========####

# Step 1 Function
clean_selected_data <- function(dtls) {
  # Define selected data frames with abbreviations for cleaning
  selected <- c(
    acc = "account", 
    ach = "account_holder", 
    com = "compliance", 
    ins = "installation", 
    proj = "project", 
    sur = "surrender"
  )
  # Basic cleaning for each selected data frame
  Map(
    function(i, name) {
      if (name %in% selected) {
        # Use abbreviation for renaming
        abbr <- names(selected[selected == name])
        i %>%
          # Keep needed columns and add file name abbr. as prefix
          select(-c(created_on, updated_on)) %>%
          rename_with(~ paste0(abbr, "_", .x)) %>%
          # Replace empty cells with NA
          mutate(across(everything(), ~ replace(.x, .x == "", NA)))
      } else {
        i
      }
    },
    dtls, names(dtls)
  )
}

# Step 2 Function
merge_account_accholder <- function(dtls) {
  # Merge account and account holder
  dtls[["account"]] %>%
    # Keep valid accounts
    filter(acc_id > 0) %>%
    full_join(
      dtls[["account_holder"]],
      by = c("acc_accountHolder_id" = "ach_id")
    )
}

# Step 3 Function
merge_installation_and_compliance <- function(dtls) {
  # Define columns to sum by installation-year
  ## Duplicated ins_id-com_year during 2020-2030 due to linked EU-Swiss system
  mycol <- c(
    "com_allocatedFree", 
    "com_allocatedNewEntrance", 
    "com_allocatedTotal", 
    "com_allocated10c", 
    "com_verified", 
    "com_verifiedCummulative", 
    "com_surrenderedCummulative"
  )
  
  # Merge installation and compliance
  dtls[["installation"]] %>%
    full_join(
      dtls[["compliance"]],
      by = c("ins_id" = "com_installation_id")
    ) %>%
    # Sum EUETS and CHETS amounts for the same installation-year
    group_by(ins_id, com_year) %>%
    mutate(
      across(
        all_of(mycol), 
        ~ sum(.x, na.rm = TRUE), 
        .names = "{.col}_sum"
        )
    ) %>%
    ungroup() %>%
    distinct(ins_id, com_year, com_verified_sum, .keep_all = TRUE)
}

# Step 4 Function
merge_and_clean <- function(acc_acch, ins_com) {
  # Merge acc_acch with ins_com
  ets_compliance <- acc_acch %>%
    filter(!is.na(acc_installation_id)) %>%
    # Add indicators for duplicates and account closing years
    group_by(acc_installation_id) %>%
    mutate(
      dup_ins = n(),
      same_ach = ifelse(n_distinct(ach_name) == 1, TRUE, FALSE),
      # Add account closing years
      account_close_year = as.integer(substr(acc_closingDate, 1, 4))
    ) %>%
    # Drop former OHA accounts with the same ACH
    filter(!(acc_accountType_id == "120-0" & same_ach)) %>%
    group_by(acc_installation_id) %>%
    mutate(dup_ins = n()) %>%
    ungroup() %>%
    # Merge with ins_com
    full_join(ins_com, by = c("acc_installation_id" = "ins_id"))
  
  # Handle duplicated installation-years
  ets_compliance_dup <- ets_compliance %>%
    filter(dup_ins == 2) %>%
    group_by(acc_installation_id, com_year) %>%
    # Handle installations linked to both old (120-0) and new (100-7) accounts
    mutate(
      foha_close_year = ifelse(
        sum(is.na(account_close_year)) == 2,
        NA,
        min(account_close_year, na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%
    filter(
      (acc_accountType_id == "100-7" &
         (is.na(foha_close_year) | com_year > foha_close_year)) |
        (acc_accountType_id == "120-0" & com_year <= foha_close_year)
    )
  
  # Combine filtered and duplicate-compliant records and clean
  merged <- ets_compliance %>%
    filter(dup_ins == 1) %>%
    bind_rows(ets_compliance_dup) %>%
    ## Change variables with "True" and "False" to logic variables
    mutate(across(
      c(acc_isOpen, acc_isRegisteredEutl, ins_isAircraftOperator, com_verifiedUpdated), 
      ~ ifelse(.x == "True", TRUE, FALSE)
    )) %>%
    ## Add different year variables based on the corresponding original date variables
    mutate(across(
      c(acc_openingDate, acc_closingDate, ins_monitoringFirstYear),
      ~ as.integer(substr(.x, 1, 4)),
      .names = "{.col}_year"
    )) %>%
    ## Add an indicator to indicate if the installation is in the system based on compliance_id
    mutate(regulated = ifelse(!is.na(com_compliance_id), TRUE, FALSE)) %>%
    ## Restore names of some variables changed in merger
    rename(account_holder_id = acc_accountHolder_id,
           installation_id = acc_installation_id) %>%
    ## Keep obs. before 2023
    filter(com_year < 2022) %>%
    mutate(com_year = as.character(com_year))
}



####======== Generate results and clean up ==========####

results <- main()

rm(list = setdiff(ls(), c("results", ls(pattern="eda_"))))








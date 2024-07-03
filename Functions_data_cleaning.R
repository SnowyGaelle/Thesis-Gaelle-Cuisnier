#-------------------------------------------------------------------------------
# Calculate score IPA between 0 and 100
#-------------------------------------------------------------------------------

global_score_IPA <- function(dataset, variable_name, Q1, Q2, Q3, Q4, Q5) {
  dataset[Q1] <- 6 - dataset[Q1]
  dataset[Q2] <- 6 - dataset[Q2]
  dataset[Q3] <- 6 - dataset[Q3]
  dataset[Q4] <- 6 - dataset[Q4]
  dataset[Q5] <- 6 - dataset[Q5]
  
  dataset[variable_name] <- (
    (dataset[Q1] +
       dataset[Q2] +
       dataset[Q3] +
       dataset[Q4] +
       dataset[Q5] - 5) * 5
  )
  
  dataset[Q1] <- NULL
  dataset[Q2] <- NULL
  dataset[Q3] <- NULL
  dataset[Q4] <- NULL
  dataset[Q5] <- NULL
  return(dataset)
}

#-------------------------------------------------------------------------------
# Calculation FU rate
#-------------------------------------------------------------------------------

percentage_FU <- function(dataset, variable_name) {
  variable <- dataset[[variable_name]]
  totalcells <- nrow(dataset)
  missingcells <- sum(is.na(variable))
  percentage <- 100 - ((missingcells * 100) / (totalcells))
  
  return(percentage)
}

#-------------------------------------------------------------------------------
# Create long datasets
#-------------------------------------------------------------------------------

long_dataset <- function(dataset, outcome) {
  excluded_columns <- grep("valid_timepoints", names(dataset), value = TRUE, invert = TRUE)
  relevant_columns <- grep(outcome, excluded_columns, names(dataset), value = TRUE)
  dataset[relevant_columns] <- lapply(dataset[relevant_columns], function(x) {
    as.numeric(x)
  })
  
  long_dataset <- pivot_longer(dataset,
                               cols = relevant_columns,
                               names_to = "FU_timepoint", values_to = outcome
  )
  
  long_dataset <- long_dataset %>%
    mutate(
      FU_timepoint = recode(FU_timepoint,
                            !!relevant_columns[[1]] := "1",
                            !!relevant_columns[[2]] := "2",
                            !!relevant_columns[[3]] := "3",
                            !!relevant_columns[[4]] := "4",
                            !!relevant_columns[[5]]:= "5",
                            !!relevant_columns[[6]] := "6",
                            !!relevant_columns[[7]] := "7",
                            !!relevant_columns[[8]] := "8",
                            !!relevant_columns[[9]] := "9",
                            !!relevant_columns[[10]] := "10",
                            !!relevant_columns[[11]] := "11",
                            !!relevant_columns[[12]] := "0"
      ),
      FU_timepoint = as.numeric(FU_timepoint)
    )
  return(long_dataset)
}


#-------------------------------------------------------------------------------
# Recode hourly PA variables with 2 hours of data per variable
#-------------------------------------------------------------------------------

recode_hourly_PA <- function (dataset, WEEK_or_WKND) {
  # Remove sleep hours
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_00_01", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_01_02", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_02_03", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_03_04", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_04_05", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_05_06", sep = "")]] <- NULL
  
  # Merge columns by sections of 2 hours
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_06_08", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_06_07", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_07_08", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_06_07", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_07_08", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_08_10", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_08_09", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_09_10", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_08_09", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_09_10", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_10_12", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_10_11", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_11_12", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_10_11", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_11_12", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_12_14", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_12_13", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_13_14", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_12_13", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_13_14", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_14_16", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_14_15", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_15_16", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_14_15", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_15_16", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_16_18", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_16_17", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_17_18", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_16_17", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_17_18", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_18_20", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_18_19", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_19_20", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_18_19", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_19_20", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_20_22", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_20_21", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_21_22", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_20_21", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_21_22", sep = "")]] <- NULL
  
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_22_24", sep = "")]] <- dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_22_23", sep = "")]] + dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_23_24", sep = "")]] 
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_22_23", sep = "")]] <- NULL
  dataset[[paste("MIN_STEP_", WEEK_or_WKND, "_23_24", sep = "")]] <- NULL
  
  return(dataset)
}
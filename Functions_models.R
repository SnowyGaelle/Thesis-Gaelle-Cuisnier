#-------------------------------------------------------------------------------
# Check assumptions of linear mixed models
#-------------------------------------------------------------------------------

assumptions_LMM <- function (model, dataset, outcome, cont_covariate, model_name) {
  
  # Linearity between each predictor and the outcome
  
  # Linear relationship between predictors and outcome at baseline
  file_name_baseline <- paste("Assumptions/Linearity/", outcome,"/", model_name, " baseline ",cont_covariate, ".jpg", sep = "")
  outcome_baseline <- paste(outcome, "_baseline", sep = "")
  
  if (!file.exists(file_name_baseline)) {
    x <- dataset[[cont_covariate]]
    y <- dataset[[outcome_baseline]]
    
    plot_linearity_baseline <- ggplot(dataset, aes(x = x, y = y)) +
      geom_point() +
      labs(
        x = label(dataset[[cont_covariate]]),
        y = label(dataset[[outcome_baseline]])
      ) +
      theme_minimal()
    ggsave(file_name_baseline, plot = plot_linearity_baseline, device = "jpg", width = 10, height = 6)
    
    # Linear relationship between predictors and outcome at follow-up
    for (set_number in 1:11) {
      formatted_set_number <- sprintf("%02d", set_number)
      FU_point <- paste(".FU", formatted_set_number, sep = "")
      file_name_FU <- paste("Assumptions/linearity/",outcome, "/", model_name, " ", FU_point,  " ",cont_covariate,".jpg", sep = "")
      outcome_FU <- paste(outcome, ".FU", formatted_set_number, sep = "")
      
      x <- dataset[[cont_covariate]]
      y <- dataset[[outcome_FU]]
      
      plot_linearity_FU <- ggplot(dataset, aes(x = x, y = y)) +
        geom_point() +
        labs(
          x = label(dataset[[cont_covariate]]),
          y = label(dataset[[outcome_FU]])
        ) +
        theme_minimal()
      ggsave(file_name_FU, plot = plot_linearity_FU, device = "jpg", width = 10, height = 6)
    }
  }
  
  # Normality of residuals
  residuals_data <- data.frame(residuals = residuals(model))
  
  file_name_hist <- paste("Assumptions/Normality of residuals/" ,outcome, "/", model_name, ".jpg", sep = "")
  if (!file.exists(file_name_hist)) {
    plot_hist <- ggplot(residuals_data, aes(x = residuals)) + 
      geom_histogram(bins = 40) +
      labs(title = model_name)
    ggsave(file_name_hist, plot = plot_hist, device = "jpg", width = 10, height = 6)
  }
  
  # No multicollinearity
  VIF <- rms::vif(model)
  vif_table <- tibble(
    Variable = names(VIF),
    VIF = round(VIF, 2)
  )
  
  # Create table with all values and save it
  table_name <- paste("Assumptions/Multicollinearity/" ,outcome, "/", model_name, ".pdf", sep = "")
  
  if (!file.exists(table_name)) {
    gt_table <- gt(vif_table) %>%
      tab_header(
        title = paste(model_name, " Variance Inflation Factors"),
      ) %>%
      cols_label(
        Variable = "Variable",
        VIF = "Variance Inflation Factor"
      )
    
    gt::gtsave(gt_table, filename = table_name)
  }
}

#-------------------------------------------------------------------------------
# Store data of a model necessary for the results tables
#-------------------------------------------------------------------------------

create_model_data <- function(coefficients, conf_intervals) {
  result_table <- data.frame(
    Variable = rownames(coefficients),
    Estimate_CI = sprintf("%.2f (%.2f, %.2f)", coefficients[, "Estimate"], conf_intervals[, "2.5 %"], conf_intervals[, "97.5 %"]))
  
  return(result_table)
}

#-------------------------------------------------------------------------------
# Detects if a value in the results table is significant
#-------------------------------------------------------------------------------

is_significant <- function(str) {
  init_split = strsplit(str, '\\(')[[1]]
  if(length(init_split) != 2) {
    return(FALSE)
  }
  
  interval = init_split[2]
  if(grepl("0.00", interval)){
    return(FALSE)
  }
  
  if(grepl("-", interval)){
    minus_split = strsplit(interval, "-")[[1]]
    return(length(minus_split) == 3)
  }
  
  return(TRUE)
  
}

determine_significant_cells <- function(mat) {
  nb_rows = dim(mat)[1]
  nb_cols = dim(mat)[2]
  result = matrix(FALSE, nb_rows, nb_cols)
  for(i in 1:nb_rows){
    for(j in 1:nb_cols){
      result[i, j] = is_significant(mat[i, j])
    }
  }
  return(result)
}

#-------------------------------------------------------------------------------
# Generate models for checking for interactions
#-------------------------------------------------------------------------------

# For each model:
# 1. Create the model
# 2. Rename the coefficients
# 3. Check assumptions

models_interaction <- function (long_dataset, wide_dataset, outcome, covariate, model_name) {
  
  if (covariate == "Sex") {
    # Step 1: create models
    formula_1 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + PA_pattern*Sex +
      Education_level + Marital_status + DT2_status"))
    model_1 <- lm(formula_1, data = long_dataset)
    model_name_1 <- paste(model_name, "1", sep = "_")
    
    formula_2 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + PA_pattern*Sex +
      Education_level + Marital_status + DT2_status + DHD_sum +
      Smoking_status"))
    model_2 <- lm(formula_2, data = long_dataset)
    model_name_2 <- paste(model_name, "2", sep = "_")
    
    formula_3 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + PA_pattern*Sex +
      Education_level + Marital_status + DT2_status + DHD_sum +
      Smoking_status + BMI"))
    model_3 <- lm(formula_3, data = long_dataset)
    model_name_3 <- paste(model_name, "3", sep = "_")
    
    # Step 2: rename coefficients
    model_1 <- rename_coeffs_interaction_1(model_1, covariate)
    model_2 <- rename_coeffs_interaction_2(model_2, covariate)
    model_3 <- rename_coeffs_interaction_3(model_3, covariate)
  }
  
  if (covariate == "DT2_status") {
    # Step 1: create models
    formula_1 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
      Education_level + Marital_status + PA_pattern*DT2_status"))
    model_1 <- lm(formula_1, data = long_dataset)
    model_name_1 <- paste(model_name, "1", sep = "_")
    
    formula_2 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
      Education_level + Marital_status + PA_pattern*DT2_status + DHD_sum +
      Smoking_status"))
    model_2 <- lm(formula_2, data = long_dataset)
    model_name_2 <- paste(model_name, "2", sep = "_")
    
    formula_3 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
      Education_level + Marital_status + PA_pattern*DT2_status + DHD_sum +
      Smoking_status + BMI"))
    model_3 <- lm(formula_3, data = long_dataset)
    model_name_3 <- paste(model_name, "3", sep = "_")
    
    # Step 2: rename coefficients
    model_1 <- rename_coeffs_interaction_1(model_1, covariate)
    model_2 <- rename_coeffs_interaction_2(model_2, covariate)
    model_3 <- rename_coeffs_interaction_3(model_3, covariate)
  }
  
  # Step 3: check assumptions
  assumptions_LMM(model_1, wide_dataset, outcome, "Age", model_name_1)
  assumptions_LMM(model_2, wide_dataset, outcome, "DHD_sum", model_name_2)
  assumptions_LMM(model_3, wide_dataset, outcome, "BMI", model_name_3)
  
  # Step 4: rename models
  models <- list()
  models[[paste(model_name, "1", sep = "_")]] <- model_1
  models[[paste(model_name, "2", sep = "_")]] <- model_2
  models[[paste(model_name, "3", sep = "_")]] <- model_3
  
  return(models)
}

#-------------------------------------------------------------------------------
# Generate models for checking for interactions in subgroups
#-------------------------------------------------------------------------------

# For each model:
# 1. Create the model
# 2. Rename the coefficients
# 3. Check assumptions

models_interaction_subgroup <- function (long_dataset, wide_dataset, outcome, covariate, model_name) {
  
    # Step 1: create models
    formula_1 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + PA_pattern*Sex +
      Education_level + Marital_status"))
    model_1 <- lm(formula_1, data = long_dataset)
    model_name_1 <- paste(model_name, "1", sep = "_")
    
    formula_2 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + PA_pattern*Sex +
      Education_level + Marital_status + DHD_sum + Smoking_status"))
    model_2 <- lm(formula_2, data = long_dataset)
    model_name_2 <- paste(model_name, "2", sep = "_")
    
    formula_3 <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + PA_pattern*Sex +
      Education_level + Marital_status + DHD_sum + Smoking_status + BMI"))
    model_3 <- lm(formula_3, data = long_dataset)
    model_name_3 <- paste(model_name, "3", sep = "_")
    
    # Step 2: rename coefficients
    model_1 <- rename_coeffs_interaction_subgroup_1(model_1, covariate)
    model_2 <- rename_coeffs_interaction_subgroup_2(model_2, covariate)
    model_3 <- rename_coeffs_interaction_subgroup_3(model_3, covariate)
  
  # Step 3: check assumptions
  assumptions_LMM(model_1, wide_dataset, outcome, "Age", model_name_1)
  assumptions_LMM(model_2, wide_dataset, outcome, "DHD_sum", model_name_2)
  assumptions_LMM(model_3, wide_dataset, outcome, "BMI", model_name_3)
  
  # Step 4: rename models
  models <- list()
  models[[paste(model_name, "1", sep = "_")]] <- model_1
  models[[paste(model_name, "2", sep = "_")]] <- model_2
  models[[paste(model_name, "3", sep = "_")]] <- model_3
  
  return(models)
}

#-------------------------------------------------------------------------------
# Generate models for subgroup analysis (stratified by DT2 status)
#-------------------------------------------------------------------------------

# The following steps are done for each model:
# 1. Create the model
# 2. Rename the coefficients
# 3. Check assumptions

models_subgroup <- function(outcome, long_dataset, wide_dataset, model_name) {
  # Step 1: create models
  formula_model_1 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status"))
  model_1 <- lm(formula_model_1, data = long_dataset)
  model_name_1 <- paste(model_name, "1", sep = "_")
  
  formula_model_2 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + DHD_sum +
                               Smoking_status")) 
  model_2 <- lm(formula_model_2, data = long_dataset)
  model_name_2 <- paste(model_name, "2", sep = "_")
  
  formula_model_3 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + DHD_sum +
                               Smoking_status + BMI"))
  model_3 <- lm(formula_model_3, data = long_dataset)
  model_name_3 <- paste(model_name, "3", sep = "_")
  
  # Step 2: rename coefficients
  model_1 <- rename_coeffs_subgroup_1(model_1, "DT2_status")
  model_2 <- rename_coeffs_subgroup_2(model_2, "DT2_status")
  model_3 <- rename_coeffs_subgroup_3(model_3, "DT2_status")
  
  # Step 3: check assumptions
  assumptions_LMM(model_1, wide_dataset, outcome, "Age", model_name_1)
  assumptions_LMM(model_2, wide_dataset, outcome, "DHD_sum", model_name_2)
  assumptions_LMM(model_3, wide_dataset, outcome, "BMI", model_name_3)
  
  # Step 4: rename models
  models <- list()
  models[[paste(model_name, "1", sep = "_")]] <- model_1
  models[[paste(model_name, "2", sep = "_")]] <- model_2
  models[[paste(model_name, "3", sep = "_")]] <- model_3
  
  return(models)
}

#-------------------------------------------------------------------------------
# Generate models with addition of employment status - stratified by T2DM status
#-------------------------------------------------------------------------------

# The following steps are done for each model:
# 1. Create the model
# 2. Rename the coefficients
# 3. Check assumptions

models_employment <- function(outcome, long_dataset, wide_dataset, model_name) {
  # Step 1: create models
  formula_model_1 <- as.formula(paste(outcome, " ~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + Employment_status"))
  model_1 <- lm(formula_model_1, data = long_dataset)
  model_name_1 <- paste(model_name, "1", sep = "_")
  
  formula_model_2 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + Employment_status + 
                               DHD_sum + Smoking_status")) 
  model_2 <- lm(formula_model_2, data = long_dataset)
  model_name_2 <- paste(model_name, "2", sep = "_")
  
  formula_model_3 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + Employment_status + 
                               DHD_sum + Smoking_status + BMI"))
  model_3 <- lm(formula_model_3, data = long_dataset)
  model_name_3 <- paste(model_name, "3", sep = "_")
  
  # Step 2: rename coefficients
  model_1 <- rename_coeffs_employment_1(model_1, "DT2_status")
  model_2 <- rename_coeffs_employment_2(model_2, "DT2_status")
  model_3 <- rename_coeffs_employment_3(model_3, "DT2_status")
  
  # Step 3: check assumptions
  assumptions_LMM(model_1, wide_dataset, outcome, "Age", model_name_1)
  assumptions_LMM(model_2, wide_dataset, outcome, "DHD_sum", model_name_2)
  assumptions_LMM(model_3, wide_dataset, outcome, "BMI", model_name_3)
  
  # Step 4: rename models
  models <- list()
  models[[paste(model_name, "1", sep = "_")]] <- model_1
  models[[paste(model_name, "2", sep = "_")]] <- model_2
  models[[paste(model_name, "3", sep = "_")]] <- model_3
  
  return(models)
}

#-------------------------------------------------------------------------------
# Generate models with addition of year of baseline measurements
#-------------------------------------------------------------------------------

# The following steps are done for each model:
# 1. Create the model
# 2. Rename the coefficients
# 3. Check assumptions

models_date <- function(outcome, long_dataset, wide_dataset, model_name) {
  # Step 1: create models
  formula_model_1 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + Year_visit1"))
  model_1 <- lm(formula_model_1, data = long_dataset)
  model_name_1 <- paste(model_name, "1", sep = "_")
  
  formula_model_2 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + + Year_visit1 +
                               DHD_sum + Smoking_status")) 
  model_2 <- lm(formula_model_2, data = long_dataset)
  model_name_2 <- paste(model_name, "2", sep = "_")
  
  formula_model_3 <- as.formula(paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
                               Education_level + Marital_status + Year_visit1 +
                               DHD_sum + Smoking_status + BMI"))
  model_3 <- lm(formula_model_3, data = long_dataset)
  model_name_3 <- paste(model_name, "3", sep = "_")
  
  # Step 2: rename coefficients
  model_1 <- rename_coeffs_date_1(model_1)
  model_2 <- rename_coeffs_date_2(model_2)
  model_3 <- rename_coeffs_date_3(model_3)

  # Step 3: check assumptions
  assumptions_LMM(model_1, wide_dataset, outcome, "Age", model_name_1)
  assumptions_LMM(model_2, wide_dataset, outcome, "DHD_sum", model_name_2)
  assumptions_LMM(model_3, wide_dataset, outcome, "BMI", model_name_3)
  
  # Step 4: rename models
  models <- list()
  models[[paste(model_name, "1", sep = "_")]] <- model_1
  models[[paste(model_name, "2", sep = "_")]] <- model_2
  models[[paste(model_name, "3", sep = "_")]] <- model_3
  
  return(models)
}

#-------------------------------------------------------------------------------
# Generate table with interaction terms
#-------------------------------------------------------------------------------

table_interaction <- function(model1, model2, model3, covariate, title_table, title_file) {
  
  # Store coefficients and 95% confidence intervals of each model
  coefficients_1 <- coef(summary(model1))
  conf_intervals_1 <- confint(model1, level = 0.95)
  
  coefficients_2 <- coef(summary(model2))
  conf_intervals_2 <- confint(model2, level = 0.95)
  
  coefficients_3 <- coef(summary(model3))
  conf_intervals_3 <- confint(model3, level = 0.95)
  
  # Select coefficients and 95% CI of the variables of interest
  names_PA <- c(
    paste("Consistently low active*", covariate, sep = ""),
    paste("Active on weekdays*", covariate, sep = ""),
    paste("Early birds*", covariate, sep = ""),
    paste("Consistently moderately active*", covariate, sep = ""),
    paste("Weekend warriors*", covariate, sep = ""),
    paste("Consistently highly active*", covariate, sep = "")
  )
  
  coefficients_PA_1 <- coefficients_1[rownames(coefficients_1) %in% names_PA, ]
  conf_intervals_PA_1 <- conf_intervals_1[rownames(conf_intervals_1) %in% names_PA, ]
  
  coefficients_PA_2 <- coefficients_2[rownames(coefficients_2) %in% names_PA, ]
  conf_intervals_PA_2 <- conf_intervals_2[rownames(conf_intervals_2) %in% names_PA, ]
  
  coefficients_PA_3 <- coefficients_3[rownames(coefficients_3) %in% names_PA, ]
  conf_intervals_PA_3 <- conf_intervals_3[rownames(conf_intervals_3) %in% names_PA, ]
  
  data_model1 <- create_model_data(coefficients_PA_1, conf_intervals_PA_1)
  data_model2 <- create_model_data(coefficients_PA_2, conf_intervals_PA_2)
  data_model3 <- create_model_data(coefficients_PA_3, conf_intervals_PA_3)
  
  # Merge the different dataframes and adjust column names
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), list(data_model1, data_model2, data_model3))
  names(combined_data)[2:length(names(combined_data))] <- c("Result1", "Result2", "Result3")
  
  # Order the rows by names_PA (automatically, displayed by alphabetical order)
  combined_data <- combined_data[match(names_PA, combined_data$Variable), ]
  
  # Define which cells present significant results (so we can put them in bold
  # later on)
  significant_cells <- determine_significant_cells(combined_data[, c("Result1", "Result2", "Result3")])
  
  # Create and save table
  table_name <- paste("Results models/", title_file, ".png", sep = "")
  if (!file.exists(table_name)) {
    gt_table <- gt(combined_data) %>%
      cols_label(
        Variable = "Variable",
        Result1 = "Model 1",
        Result2 = "Model 2",
        Result3 = "Model 3"
      ) %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_width(
        Variable ~ px(275),  
        everything() ~ px(200)
      )
    for(i in 1:dim(significant_cells)[1]){
      for(j in 1:dim(significant_cells)[2]){
        if(significant_cells[i, j]){
          gt_table <- gt_table %>%
            tab_style(
              style = cell_text(weight = "bold"),
              locations = cells_body(
                columns = c(j+1),
                rows = c(i)
              )
            )
        }
      }
    }
    gt::gtsave(gt_table, filename = table_name)
  }
}

#-------------------------------------------------------------------------------
# Generate table with the results of one model for both T2DM strata
#-------------------------------------------------------------------------------

table_1_model <- function(model_diabetes, model_no_diabetes, title_table, title_file) {
  # Select coefficients and 95% CI of the variables of interest
  names_PA <- c(
    "Consistently low active",
    "Active on weekdays", 
    "Early birds",
    "Consistently moderately active",
    "Weekend warriors",
    "Consistently highly active",     
    "Consistently low active*time",
    "Active on weekdays*time", 
    "Early birds*time",
    "Consistently moderately active*time",
    "Weekend warriors*time",
    "Consistently highly active*time"
  )
  
  # Store coefficients and 95% confidence intervals of each model
  coefficients_diabetes <- coef(summary(model_diabetes))
  conf_intervals_diabetes <- confint(model_diabetes, level = 0.95)
  
  coefficients_no_diabetes <- coef(summary(model_no_diabetes))
  conf_intervals_no_diabetes <- confint(model_no_diabetes, level = 0.95)
  
  coefficients_PA_diabetes <- coefficients_diabetes[rownames(coefficients_diabetes) %in% names_PA, ]
  conf_intervals_PA_diabetes <- conf_intervals_diabetes[rownames(conf_intervals_diabetes) %in% names_PA, ]
  
  coefficients_PA_no_diabetes <- coefficients_no_diabetes[rownames(coefficients_no_diabetes) %in% names_PA, ]
  conf_intervals_PA_no_diabetes <- conf_intervals_no_diabetes[rownames(conf_intervals_no_diabetes) %in% names_PA, ]
  
  data_model1 <- create_model_data(coefficients_PA_diabetes, conf_intervals_PA_diabetes)
  data_model2 <- create_model_data(coefficients_PA_no_diabetes, conf_intervals_PA_no_diabetes)
  
  # Merge the different dataframes and adjust column names
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), list(data_model1, data_model2))
  names(combined_data)[2:length(names(combined_data))] <- c("Result1", "Result2")
  
  # Order the rows by names_PA (automatically, displayed by alphabetical order)
  combined_data <- combined_data[match(names_PA, combined_data$Variable), ]
  
  significant_cells <- determine_significant_cells(combined_data[, c("Result1", "Result2")])
  
  # Create and save table
  table_name <- paste("Results models/", title_file, ".png", sep = "")
  if (!file.exists(table_name)) {
    gt_table <- gt(combined_data) %>%
      tab_header(title = title_table) %>%
      cols_label(
        Variable = "Variable",
        Result1 = "Participants with type 2 diabetes",
        Result2 = "Participants without type 2 diabetes"
      ) %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_width(
        Variable ~ px(275),  
        everything() ~ px(275)
      )
    
    gt_table <- rows_add(.data = gt_table, .before = 1, Variable = "Consistently inactive", Result1 = "Reference", Result2 = "Reference")
    gt_table <- rows_add(.data = gt_table, .before = 8, Variable = "Consistently inactive*time", Result1 = "Reference", Result2 = "Reference")
    gt_table <- rows_add(.data = gt_table, .before = 8, Variable = "", Result1 = "", Result2 = "")
    
    
    for(i in 1:dim(significant_cells)[1]){
      for(j in 1:dim(significant_cells)[2]){
        if(significant_cells[i, j]){
          # Adding an offset to the rows because rows are added post processing
          # +1 because one row is added on top of the table
          row_nb = c(i) + 1
          # +2 at 8 because two rows are added before the 8th row
          if(row_nb >= 8){
            row_nb = row_nb + 2
          }
          gt_table <- gt_table %>%
            tab_style(
              style = cell_text(weight = "bold"),
              locations = cells_body(
                columns = c(j+1),
                rows = row_nb
              )
            )
        }
      }
    }
    
    # Post processing: adding rows for reference category
    gt::gtsave(gt_table, filename = table_name)
  }
}

#-------------------------------------------------------------------------------
# Generate table with the results of the three models for one stratum
#-------------------------------------------------------------------------------

table_3_models <- function(model1, model2, model3, title_table, title_file) {
  
  # Store coefficients and 95% confidence intervals of each model
  coefficients_1 <- coef(summary(model1))
  conf_intervals_1 <- confint(model1, level = 0.95)
  
  coefficients_2 <- coef(summary(model2))
  conf_intervals_2 <- confint(model2, level = 0.95)
  
  coefficients_3 <- coef(summary(model3))
  conf_intervals_3 <- confint(model3, level = 0.95)
  
  # Select coefficients and 95% CI of the variables of interest
  names_PA <- c(
    "Consistently low active",
    "Active on weekdays", 
    "Early birds",
    "Consistently moderately active",
    "Weekend warriors",
    "Consistently highly active",     
    "Consistently low active*time",
    "Active on weekdays*time", 
    "Early birds*time",
    "Consistently moderately active*time",
    "Weekend warriors*time",
    "Consistently highly active*time"
  )
  
  coefficients_PA_1 <- coefficients_1[rownames(coefficients_1) %in% names_PA, ]
  conf_intervals_PA_1 <- conf_intervals_1[rownames(conf_intervals_1) %in% names_PA, ]
  
  coefficients_PA_2 <- coefficients_2[rownames(coefficients_2) %in% names_PA, ]
  conf_intervals_PA_2 <- conf_intervals_2[rownames(conf_intervals_2) %in% names_PA, ]
  
  coefficients_PA_3 <- coefficients_3[rownames(coefficients_3) %in% names_PA, ]
  conf_intervals_PA_3 <- conf_intervals_3[rownames(conf_intervals_3) %in% names_PA, ]
  
  data_model1 <- create_model_data(coefficients_PA_1, conf_intervals_PA_1)
  data_model2 <- create_model_data(coefficients_PA_2, conf_intervals_PA_2)
  data_model3 <- create_model_data(coefficients_PA_3, conf_intervals_PA_3)
  
  # Merge the different dataframes and adjust column names
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), list(data_model1, data_model2, data_model3))
  names(combined_data)[2:length(names(combined_data))] <- c("Result1", "Result2", "Result3")
  
  # Order the rows by names_PA (automatically, displayed by alphabetical order)
  combined_data <- combined_data[match(names_PA, combined_data$Variable), ]
  
  # Define which cells present significant results (so we can put them in bold
  # later on)
  significant_cells <- determine_significant_cells(combined_data[, c("Result1", "Result2", "Result3")])
  
  # Create and save table
  table_name <- paste("Results models/", title_file, ".png", sep = "")
  if (!file.exists(table_name)) {
    gt_table <- gt(combined_data) %>%
      tab_header(title = title_table) %>%
      cols_label(
        Variable = "Variable",
        Result1 = "Model 1",
        Result2 = "Model 2",
        Result3 = "Model 3"
      ) %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_width(
        Variable ~ px(275),  
        everything() ~ px(200)
      )
    
    # Adding rows for reference category
    gt_table <- rows_add(.data = gt_table, .before = 1, Variable = "Consistently inactive", Result1 = "Reference", Result2 = "Reference", Result3 = "Reference")
    gt_table <- rows_add(.data = gt_table, .before = 8, Variable = "Consistently inactive*time", Result1 = "Reference", Result2 = "Reference", Result3 = "Reference")
    gt_table <- rows_add(.data = gt_table, .before = 8, Variable = "", Result1 = "", Result2 = "", Result3 = "")
    
    for(i in 1:dim(significant_cells)[1]){
      for(j in 1:dim(significant_cells)[2]){
        if(significant_cells[i, j]){
          # Adding an offset to the rows because rows are added post processing
          # +1 because one row is added on top of the table
          row_nb = c(i) + 1
          # +2 at 8 because two rows are added before the 8th row
          if(row_nb >= 8){
            row_nb = row_nb + 2
          }
          gt_table <- gt_table %>%
            tab_style(
              style = cell_text(weight = "bold"),
              locations = cells_body(
                columns = c(j+1),
                rows = row_nb
              )
            )
        }
      }
    }
    gt::gtsave(gt_table, filename = table_name)
  }
}

#-------------------------------------------------------------------------------
# Creation of forest plots
#-------------------------------------------------------------------------------

forest_plot <- function(outcome, dataset, title_file) {
  
  name_PA <- paste("Figures presentation/", title_file, "_baseline.png", sep = "")
  name_PA_time <- paste("Figures presentation/", title_file, "_change.png", sep = "")
  if (!file.exists(name_PA) | !file.exists(name_PA_time)) {
    
    # Create model
    formula <- as.formula(
      paste(outcome, "~ PA_pattern*FU_timepoint + Age + Sex +
        Education_level + Marital_status + DHD_sum +
        Smoking_status + BMI"))
    model <- lm(formula, data = dataset)
    model <- rename_coeffs_subgroup_3(model, "DT2_status")
    
    # Generate estimate and 95% CI
    model_summary <- summary(model)
    estimates <- model_summary$coefficients[, "Estimate"]
    conf_intervals <- confint(model, level = 0.95)
    
    # Select variables of interest
    names_PA <- c(
      "Consistently low active",
      "Active on weekdays",
      "Early birds",
      "Consistently moderately active",
      "Weekend warriors",
      "Consistently highly active"
    )
    
    names_PA_time <- c("Consistently low active*time",
                       "Active on weekdays*time",
                       "Early birds*time",
                       "Consistently moderately active*time",
                       "Weekend warriors*time",
                       "Consistently highly active*time"
    )
    
    # Create dataframe
    df_PA <- data.frame(
      Variable = factor(names_PA, levels = c(
        "Consistently highly active",
        "Weekend warriors",
        "Consistently moderately active",
        "Early birds",
        "Active on weekdays",
        "Consistently low active"
      )),
      Estimate = estimates[names_PA],
      Lower_bound = conf_intervals[names_PA, 1],
      Upper_bound = conf_intervals[names_PA, 2]
    )
    
    df_PA_time <- data.frame(
      Variable = factor(names_PA_time, levels = c(
        "Consistently highly active*time",
        "Weekend warriors*time",
        "Consistently moderately active*time",
        "Early birds*time",
        "Active on weekdays*time",
        "Consistently low active*time"
      )),
      Estimate = estimates[names_PA_time],
      Lower_bound = conf_intervals[names_PA_time, 1],
      Upper_bound = conf_intervals[names_PA_time, 2]
    ) 
    
    # Define levels for dataframe with reference category
    new_levels_PA <- c("Consistently highly active",
                       "Weekend warriors",
                       "Consistently moderately active",
                       "Early birds",
                       "Active on weekdays",
                       "Consistently low active",
                       "Consistently inactive")
    
    new_levels_PA_time <- c( "Consistently highly active*time",
                             "Weekend warriors*time",
                             "Consistently moderately active*time",
                             "Early birds*time",
                             "Active on weekdays*time",
                             "Consistently low active*time",
                             "Consistently inactive*time")
    
    # Add row with reference category in the dataframes
    ref_row_PA <- data.frame(
      Variable = factor("Consistently inactive", levels = new_levels_PA),
      Estimate = 0,
      Lower_bound = 0,
      Upper_bound = 0
    )
    
    ref_row_PA_time <- data.frame(
      Variable = factor("Consistently inactive*time", levels = new_levels_PA_time),
      Estimate = 0,
      Lower_bound = 0,
      Upper_bound = 0
    )
    
    df_PA <- rbind(ref_row_PA, df_PA)
    df_PA_time <- rbind(ref_row_PA_time, df_PA_time)
    
    # Create forest plots
    plot_PA <- ggplot(data = df_PA, aes(y = Variable, x = Estimate, xmin = Lower_bound, xmax = Upper_bound)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      geom_errorbarh(height = 0.3) +
      theme_minimal()
    ggsave(name_PA, plot = plot_PA, device = "jpg", width = 5, height = 3)
    
    plot_PA_time <- ggplot(data = df_PA_time, aes(y = Variable, x = Estimate, xmin = Lower_bound, xmax = Upper_bound)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      geom_errorbarh(height = 0.3) +
      theme_minimal()
    ggsave(name_PA_time, plot = plot_PA_time, device = "jpg", width = 5, height = 3)
  }
} 

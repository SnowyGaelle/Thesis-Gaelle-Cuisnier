#-------------------------------------------------------------------------------
# Rename coefficients of models with interaction terms
#-------------------------------------------------------------------------------

# For models adjusted for age, sex, education level, marital status and diabetes
# type 2 status (defined as model 1)
rename_coeffs_interaction_1 <- function(model, covariate) {
  names(model$coefficients) <- c("(Intercept)",
                                 "Consistently low active",
                                 "Active on weekdays",
                                 "Early birds",
                                 "Consistently moderately active",
                                 "Weekend warriors",
                                 "Consistently highly active",
                                 "Follow-up time point",
                                 "Age",
                                 "Sex",
                                 "Education level medium",
                                 "Education level high",
                                 "Married, domestic partnership, civil union, living together",
                                 "Widowed",
                                 "Divorced/separated",
                                 "Diabetes type 2 status",
                                 "Consistently low active*time",
                                 "Active on weekdays*time",
                                 "Early birds*time",
                                 "Consistently moderately active*time",
                                 "Weekend warriors*time",
                                 "Consistently highly active*time",
                                 paste("Consistently low active*", covariate, sep = ""),
                                 paste("Active on weekdays*", covariate, sep = ""),
                                 paste("Early birds*", covariate, sep = ""),
                                 paste("Consistently moderately active*", covariate, sep = ""),
                                 paste("Weekend warriors*", covariate, sep = ""),
                                 paste("Consistently highly active*", covariate, sep = "")
  )
  return(model)
}

# For models adjusted for age, sex, education level, marital status, diabetes 
# type 2 status, smoking status and diet (defined as model 2)
rename_coeffs_interaction_2 <- function(model, covariate) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Diabetes type 2 status", 
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time",
                                 paste("Consistently low active*", covariate, sep = ""),
                                 paste("Active on weekdays*", covariate, sep = ""),
                                 paste("Early birds*", covariate, sep = ""),
                                 paste("Consistently moderately active*", covariate, sep = ""),
                                 paste("Weekend warriors*", covariate, sep = ""),
                                 paste("Consistently highly active*", covariate, sep = "")
  )
  return(model)
}

# For models adjusted for age, sex, education level, marital status, diabetes 
# type 2 status, smoking status, diet and body mass index (defined as model 3)
rename_coeffs_interaction_3 <- function(model, covariate) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Diabetes type 2 status", 
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Body mass index", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time",
                                 paste("Consistently low active*", covariate, sep = ""),
                                 paste("Active on weekdays*", covariate, sep = ""),
                                 paste("Early birds*", covariate, sep = ""),
                                 paste("Consistently moderately active*", covariate, sep = ""),
                                 paste("Weekend warriors*", covariate, sep = ""),
                                 paste("Consistently highly active*", covariate, sep = "")
  )
  return(model)
}

#-------------------------------------------------------------------------------
# Rename coefficients of models with interaction terms - stratified analysis
#-------------------------------------------------------------------------------

# For models adjusted for age, sex, education level and marital status
# (defined as model 1)
rename_coeffs_interaction_subgroup_1 <- function(model, covariate) {
    names(model$coefficients) <- c("(Intercept)",
                                   "Consistently low active",
                                   "Active on weekdays",
                                   "Early birds",
                                   "Consistently moderately active",
                                   "Weekend warriors",
                                   "Consistently highly active",
                                   "Follow-up time point",
                                   "Age",
                                   "Sex",
                                   "Education level medium",
                                   "Education level high",
                                   "Married, domestic partnership, civil union, living together",
                                   "Widowed",
                                   "Divorced/separated",
                                   "Consistently low active*time",
                                   "Active on weekdays*time",
                                   "Early birds*time",
                                   "Consistently moderately active*time",
                                   "Weekend warriors*time",
                                   "Consistently highly active*time",
                                   paste("Consistently low active*", covariate, sep = ""),
                                   paste("Active on weekdays*", covariate, sep = ""),
                                   paste("Early birds*", covariate, sep = ""),
                                   paste("Consistently moderately active*", covariate, sep = ""),
                                   paste("Weekend warriors*", covariate, sep = ""),
                                   paste("Consistently highly active*", covariate, sep = "")
    )
    return(model)
}

# For models adjusted for age, sex, education level, marital status, smoking 
# status and diet (defined as model 2)
rename_coeffs_interaction_subgroup_2 <- function(model, covariate) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time",
                                 paste("Consistently low active*", covariate, sep = ""),
                                 paste("Active on weekdays*", covariate, sep = ""),
                                 paste("Early birds*", covariate, sep = ""),
                                 paste("Consistently moderately active*", covariate, sep = ""),
                                 paste("Weekend warriors*", covariate, sep = ""),
                                 paste("Consistently highly active*", covariate, sep = "")
  )
  return(model)
}

# For models adjusted for age, sex, education level, marital status, smoking 
# status, diet and body mass index (defined as model 3)
rename_coeffs_interaction_subgroup_3 <- function(model, covariate) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Body mass index", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time",
                                 paste("Consistently low active*", covariate, sep = ""),
                                 paste("Active on weekdays*", covariate, sep = ""),
                                 paste("Early birds*", covariate, sep = ""),
                                 paste("Consistently moderately active*", covariate, sep = ""),
                                 paste("Weekend warriors*", covariate, sep = ""),
                                 paste("Consistently highly active*", covariate, sep = "")
  )
  return(model)
}

#-------------------------------------------------------------------------------
# Rename coefficients of models for non-stratified analysis
#-------------------------------------------------------------------------------

# For models adjusted for age, sex, education level, marital status and diabetes
# type 2 status (defined as model 1)
rename_coeffs_1 <- function(model) {
    names(model$coefficients) <- c("(Intercept)",
                                   "Consistently low active",
                                   "Active on weekdays",
                                   "Early birds",
                                   "Consistently moderately active",
                                   "Weekend warriors",
                                   "Consistently highly active",
                                   "Follow-up time point",
                                   "Age",
                                   "Sex",
                                   "Education level medium",
                                   "Education level high",
                                   "Married, domestic partnership, civil union, living together",
                                   "Widowed",
                                   "Divorced/separated",
                                   "Diabetes type 2 status",
                                   "Consistently low active*time",
                                   "Active on weekdays*time",
                                   "Early birds*time",
                                   "Consistently moderately active*time",
                                   "Weekend warriors*time",
                                   "Consistently highly active*time")
    return(model)
}

# For models adjusted for age, sex, education level, marital status, diabetes 
# type 2 status, smoking status and diet (defined as model 2)
rename_coeffs_2 <- function(model) {
    names(model$coefficients) <- c("(Intercept)", 
                                   "Consistently low active", 
                                   "Active on weekdays", 
                                   "Early birds", 
                                   "Consistently moderately active", 
                                   "Weekend warriors", 
                                   "Consistently highly active", 
                                   "Follow-up time point", 
                                   "Age",
                                   "Sex", 
                                   "Education level medium", 
                                   "Education level high", 
                                   "Married, domestic partnership, civil union, living together", 
                                   "Widowed", 
                                   "Divorced/separated", 
                                   "Diabetes type 2 status",
                                   "Dutch Healthy Diet index", 
                                   "Former smoker", 
                                   "Current smoker", 
                                   "Consistently low active*time", 
                                   "Active on weekdays*time", 
                                   "Early birds*time", 
                                   "Consistently moderately active*time", 
                                   "Weekend warriors*time", 
                                   "Consistently highly active*time")
    return(model)
}

# For models adjusted for age, sex, education level, marital status, diabetes 
# type 2 status, smoking status, diet and body mass index (defined as model 3)
rename_coeffs_3 <- function(model) {
    names(model$coefficients) <- c("(Intercept)", 
                                   "Consistently low active", 
                                   "Active on weekdays", 
                                   "Early birds", 
                                   "Consistently moderately active", 
                                   "Weekend warriors", 
                                   "Consistently highly active", 
                                   "Follow-up time point", 
                                   "Age",
                                   "Sex", 
                                   "Education level medium", 
                                   "Education level high", 
                                   "Married, domestic partnership, civil union, living together", 
                                   "Widowed", 
                                   "Divorced/separated", 
                                   "Diabetes type 2 status",
                                   "Dutch Healthy Diet index", 
                                   "Former smoker", 
                                   "Current smoker", 
                                   "Body mass index", 
                                   "Consistently low active*time", 
                                   "Active on weekdays*time", 
                                   "Early birds*time", 
                                   "Consistently moderately active*time", 
                                   "Weekend warriors*time", 
                                   "Consistently highly active*time")
    return(model)
}

#-------------------------------------------------------------------------------
# Rename coefficients of models for subgroup analysis
#-------------------------------------------------------------------------------

# For models adjusted for age, sex, education level, marital status and diabetes
# type 2 status (defined as model 1)
rename_coeffs_subgroup_1 <- function(model, stratification_variable) {
    if (stratification_variable == "DT2_status") {
    names(model$coefficients) <- c("(Intercept)",
                                   "Consistently low active",
                                   "Active on weekdays",
                                   "Early birds",
                                   "Consistently moderately active",
                                   "Weekend warriors",
                                   "Consistently highly active",
                                   "Follow-up time point",
                                   "Age",
                                   "Sex",
                                   "Education level medium",
                                   "Education level high",
                                   "Married, domestic partnership, civil union, living together",
                                   "Widowed",
                                   "Divorced/separated",
                                   "Consistently low active*time",
                                   "Active on weekdays*time",
                                   "Early birds*time",
                                   "Consistently moderately active*time",
                                   "Weekend warriors*time",
                                   "Consistently highly active*time")
    return(model)
    }
  
  if (stratification_variable == "Sex") {
    names(model$coefficients) <- c("(Intercept)",
                                   "Consistently low active",
                                   "Active on weekdays",
                                   "Early birds",
                                   "Consistently moderately active",
                                   "Weekend warriors",
                                   "Consistently highly active",
                                   "Follow-up time point",
                                   "Age",
                                   "Education level medium",
                                   "Education level high",
                                   "Married, domestic partnership, civil union, living together",
                                   "Widowed",
                                   "Divorced/separated",
                                   "Diabetes type 2 status",
                                   "Consistently low active*time",
                                   "Active on weekdays*time",
                                   "Early birds*time",
                                   "Consistently moderately active*time",
                                   "Weekend warriors*time",
                                   "Consistently highly active*time")
    return(model)
  }
}

# For models adjusted for age, sex, education level, marital status, diabetes 
# type 2 status, smoking status and diet (defined as model 2)
rename_coeffs_subgroup_2 <- function(model, stratification_variable) {
  if (stratification_variable == "DT2_status") {
    names(model$coefficients) <- c("(Intercept)", 
                                   "Consistently low active", 
                                   "Active on weekdays", 
                                   "Early birds", 
                                   "Consistently moderately active", 
                                   "Weekend warriors", 
                                   "Consistently highly active", 
                                   "Follow-up time point", 
                                   "Age",
                                   "Sex", 
                                   "Education level medium", 
                                   "Education level high", 
                                   "Married, domestic partnership, civil union, living together", 
                                   "Widowed", 
                                   "Divorced/separated", 
                                   "Dutch Healthy Diet index", 
                                   "Former smoker", 
                                   "Current smoker", 
                                   "Consistently low active*time", 
                                   "Active on weekdays*time", 
                                   "Early birds*time", 
                                   "Consistently moderately active*time", 
                                   "Weekend warriors*time", 
                                   "Consistently highly active*time")
    return(model)
  }
  
  if (stratification_variable == "Sex") {
    names(model$coefficients) <- c("(Intercept)", 
                                   "Consistently low active", 
                                   "Active on weekdays", 
                                   "Early birds", 
                                   "Consistently moderately active", 
                                   "Weekend warriors", 
                                   "Consistently highly active", 
                                   "Follow-up time point", 
                                   "Age",
                                   "Education level medium", 
                                   "Education level high", 
                                   "Married, domestic partnership, civil union, living together", 
                                   "Widowed", 
                                   "Divorced/separated",
                                   "Diabetes type 2 status",
                                   "Dutch Healthy Diet index", 
                                   "Former smoker", 
                                   "Current smoker", 
                                   "Consistently low active*time", 
                                   "Active on weekdays*time", 
                                   "Early birds*time", 
                                   "Consistently moderately active*time", 
                                   "Weekend warriors*time", 
                                   "Consistently highly active*time")
    return(model)
  }
}

# For models adjusted for age, sex, education level, marital status, diabetes 
# type 2 status, smoking status, diet and body mass index (defined as model 3)
rename_coeffs_subgroup_3 <- function(model, stratification_variable) {
  if (stratification_variable == "DT2_status") {
    names(model$coefficients) <- c("(Intercept)", 
                                   "Consistently low active", 
                                   "Active on weekdays", 
                                   "Early birds", 
                                   "Consistently moderately active", 
                                   "Weekend warriors", 
                                   "Consistently highly active", 
                                   "Follow-up time point", 
                                   "Age",
                                   "Sex", 
                                   "Education level medium", 
                                   "Education level high", 
                                   "Married, domestic partnership, civil union, living together", 
                                   "Widowed", 
                                   "Divorced/separated", 
                                   "Dutch Healthy Diet index", 
                                   "Former smoker", 
                                   "Current smoker", 
                                   "Body mass index", 
                                   "Consistently low active*time", 
                                   "Active on weekdays*time", 
                                   "Early birds*time", 
                                   "Consistently moderately active*time", 
                                   "Weekend warriors*time", 
                                   "Consistently highly active*time")
    return(model)
  }
  
  if (stratification_variable == "Sex") {
    names(model$coefficients) <- c("(Intercept)", 
                                   "Consistently low active", 
                                   "Active on weekdays", 
                                   "Early birds", 
                                   "Consistently moderately active", 
                                   "Weekend warriors", 
                                   "Consistently highly active", 
                                   "Follow-up time point", 
                                   "Age",
                                   "Education level medium", 
                                   "Education level high", 
                                   "Married, domestic partnership, civil union, living together", 
                                   "Widowed", 
                                   "Divorced/separated", 
                                   "Diabetes type 2 status",
                                   "Dutch Healthy Diet index", 
                                   "Former smoker", 
                                   "Current smoker", 
                                   "Body mass index", 
                                   "Consistently low active*time", 
                                   "Active on weekdays*time", 
                                   "Early birds*time", 
                                   "Consistently moderately active*time", 
                                   "Weekend warriors*time", 
                                   "Consistently highly active*time")
    return(model)
  }
}

#-------------------------------------------------------------------------------
# Rename coefficients of models with employment status
#-------------------------------------------------------------------------------

# For models adjusted for age, sex, education level and marital status 
# (defined as model 1)
rename_coeffs_employment_1 <- function(model) {
  names(model$coefficients) <- c("(Intercept)",
                                 "Consistently low active",
                                 "Active on weekdays",
                                 "Early birds",
                                 "Consistently moderately active",
                                 "Weekend warriors",
                                 "Consistently highly active",
                                 "Follow-up time point",
                                 "Age",
                                 "Sex",
                                 "Education level medium",
                                 "Education level high",
                                 "Married, domestic partnership, civil union, living together",
                                 "Widowed",
                                 "Divorced/separated",
                                 "Employment status: employed",
                                 "Employment status: other",
                                 "Consistently low active*time",
                                 "Active on weekdays*time",
                                 "Early birds*time",
                                 "Consistently moderately active*time",
                                 "Weekend warriors*time",
                                 "Consistently highly active*time")
  return(model)
}

# For models adjusted for age, sex, education level, marital status, smoking 
# status and diet (defined as model 2)
rename_coeffs_employment_2 <- function(model) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Employment status: employed",
                                 "Employment status: other",
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time")
  return(model)
}

# For models adjusted for age, sex, education level, marital status, smoking 
# status, diet and body mass index (defined as model 3)
rename_coeffs_employment_3 <- function(model) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Employment status: employed",
                                 "Employment status: other",
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Body mass index", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time")
  return(model)
}

#-------------------------------------------------------------------------------
# Rename coefficients of models with date of baseline measurements
#-------------------------------------------------------------------------------

# For models adjusted for age, sex, education level and marital status 
# (defined as model 1)
rename_coeffs_date_1 <- function(model) {
  names(model$coefficients) <- c("(Intercept)",
                                 "Consistently low active",
                                 "Active on weekdays",
                                 "Early birds",
                                 "Consistently moderately active",
                                 "Weekend warriors",
                                 "Consistently highly active",
                                 "Follow-up time point",
                                 "Age",
                                 "Sex",
                                 "Education level medium",
                                 "Education level high",
                                 "Married, domestic partnership, civil union, living together",
                                 "Widowed",
                                 "Divorced/separated",
                                 "Visit 1 in 2012",
                                 "Visit 1 in 2013",
                                 "Visit 1 in 2014",
                                 "Visit 1 in 2015",
                                 "Visit 1 in 2016",
                                 "Visit 1 in 2017",
                                 "Consistently low active*time",
                                 "Active on weekdays*time",
                                 "Early birds*time",
                                 "Consistently moderately active*time",
                                 "Weekend warriors*time",
                                 "Consistently highly active*time")
  return(model)
}

# For models adjusted for age, sex, education level, marital status, smoking 
# status and diet (defined as model 2)
rename_coeffs_date_2 <- function(model) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Visit 1 in 2012",
                                 "Visit 1 in 2013",
                                 "Visit 1 in 2014",
                                 "Visit 1 in 2015",
                                 "Visit 1 in 2016",
                                 "Visit 1 in 2017",
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time")
  return(model)
}

# For models adjusted for age, sex, education level, marital status, smoking 
# status, diet and body mass index (defined as model 3)
rename_coeffs_date_3 <- function(model) {
  names(model$coefficients) <- c("(Intercept)", 
                                 "Consistently low active", 
                                 "Active on weekdays", 
                                 "Early birds", 
                                 "Consistently moderately active", 
                                 "Weekend warriors", 
                                 "Consistently highly active", 
                                 "Follow-up time point", 
                                 "Age",
                                 "Sex", 
                                 "Education level medium", 
                                 "Education level high", 
                                 "Married, domestic partnership, civil union, living together", 
                                 "Widowed", 
                                 "Divorced/separated", 
                                 "Visit 1 in 2012",
                                 "Visit 1 in 2013",
                                 "Visit 1 in 2014",
                                 "Visit 1 in 2015",
                                 "Visit 1 in 2016",
                                 "Visit 1 in 2017",
                                 "Dutch Healthy Diet index", 
                                 "Former smoker", 
                                 "Current smoker", 
                                 "Body mass index", 
                                 "Consistently low active*time", 
                                 "Active on weekdays*time", 
                                 "Early birds*time", 
                                 "Consistently moderately active*time", 
                                 "Weekend warriors*time", 
                                 "Consistently highly active*time")
  return(model)
}

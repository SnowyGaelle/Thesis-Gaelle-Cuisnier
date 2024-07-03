#-------------------------------------------------------------------------------
# Set up
#-------------------------------------------------------------------------------
source("clear alias.R")
clearall()

source("Functions_data_cleaning.R")
source("Functions_descriptives.R")
source("Functions_renaming_coefficients.R")
source("Functions_models.R")

if (!require(pacman)) install.packages("pacman")
p_load(
  "haven", "dplyr", "forcats", "flextable", "ggplot2", "gtsummary", "tidyr",
  "rms", "gt", "gtExtras"
)

  path <- "E:/DMS_681_Gaelle_Cuisnier/AV681v3_Gaëlle_Cuisnier_20240528.sav"
  raw_dataset <- read_sav(path)
  
  rm(path)

#-------------------------------------------------------------------------------
# Data cleaning and recoding
#-------------------------------------------------------------------------------

# Rename variables for clarity and consistency
raw_dataset <- dplyr::rename(
  raw_dataset,
  PA_pattern = PA_trajectory_groups,
  BMI = bmi,
  Sex = SEX,
  DT2_status = N_DIABETES_2b,
  Smoking_status = smoking_3cat,
  Marital_status = marital_status,
  Education_level = N_Education_3cat,
  SF36_PCS_baseline = SF36_PCS,
  SF36_MCS_baseline = SF36_MCS,
)

# Recoding of categorical variables and creation of labels for each level
raw_dataset$PA_pattern <- factor(raw_dataset$PA_pattern,
  levels = 1:7,
  labels = c(
    "Consistently inactive",
    "Consistently low active",
    "Active on weekdays",
    "Early birds",
    "Consistently moderately active",
    "Weekend warriors",
    "Consistently highly active"
  )
)

raw_dataset$Sex <- factor(raw_dataset$Sex,
  levels = 1:2,
  labels = c(
    "Male",
    "Female"
  )
)

raw_dataset$DT2_status <- factor(raw_dataset$DT2_status,
  levels = 0:1,
  labels = c(
    "No type 2 diabetes",
    "Type 2 diabetes"
  )
)

raw_dataset$Smoking_status <- factor(raw_dataset$Smoking_status,
  levels = 0:2,
  labels = c(
    "Never",
    "Former",
    "Current"
  )
)

raw_dataset$Marital_status <- factor(raw_dataset$Marital_status,
  levels = 1:6,
  labels = c(
    "Single",
    "Married, domestic partnership, civil union",
    "Widowed",
    "Divorced/separated",
    "Living together",
    "Other"
  )
)

raw_dataset$Education_level <- factor(raw_dataset$Education_level,
  levels = 1:3,
  labels = c(
    "Low",
    "Medium",
    "High"
  )
)

raw_dataset$Employment_status <- factor(raw_dataset$Employment_status,
  levels = 0:2,
  labels = c(
    "Unemployed",
    "Employed",
    "Other"
  )
)

raw_dataset$DT2_status <- as.factor(raw_dataset$DT2_status)

# Recoding marital status: combine categories 2 and 5 together and remove 6
levels(raw_dataset$Marital_status) <- ifelse(levels(raw_dataset$Marital_status) %in% c("Married, domestic partnership, civil union", "Living together"), "Married, domestic partnership, civil union, living together", levels(raw_dataset$Marital_status))

raw_dataset <- raw_dataset %>%
  mutate(Marital_status = recode(Marital_status,
    "Other" = NA_character_
  ))

# Create a single variable for IPA summarizing the 5 items and recode the values
# the other way around
dataset_with_IPA <- global_score_IPA(
  raw_dataset,
  "IPA_baseline",
  "IPA_autonomy_Q1",
  "IPA_autonomy_Q2",
  "IPA_autonomy_Q3",
  "IPA_autonomy_Q4",
  "IPA_autonomy_Q5"
)

for (set_number in 1:11) {
  formatted_set_number <- sprintf("%02d", set_number)

  Q1 <- paste("IPA_autonomy_Q1.FU", formatted_set_number, sep = "")
  Q2 <- paste("IPA_autonomy_Q2.FU", formatted_set_number, sep = "")
  Q3 <- paste("IPA_autonomy_Q3.FU", formatted_set_number, sep = "")
  Q4 <- paste("IPA_autonomy_Q4.FU", formatted_set_number, sep = "")
  Q5 <- paste("IPA_autonomy_Q5.FU", formatted_set_number, sep = "")

  variable_name <- paste("IPA.FU", formatted_set_number, sep = "")

  dataset_with_IPA <- global_score_IPA(dataset_with_IPA, variable_name, Q1, Q2, Q3, Q4, Q5)
}

# Recode continuous variables as numerical so no problem by using the haven
# package to import data
dataset_with_IPA$DHD_sum <- as.numeric(dataset_with_IPA$DHD_sum)
dataset_with_IPA$mean_MVPA_min_wake_T <- as.numeric(dataset_with_IPA$mean_MVPA_min_wake_T)
dataset_with_IPA$mean_sed_hr_wake_T <- as.numeric(dataset_with_IPA$mean_sed_hr_wake_T)
dataset_with_IPA$mean_step_min_wake_T <- as.numeric(dataset_with_IPA$mean_step_min_wake_T)

# Creating variables for number of FU timepoints available per outcome variable
# and recode 0 as NA for facilitating analysis

# SF-36 PCS
variables_SF36_PCS <- c("SF36_PCS.FU01", "SF36_PCS.FU02", "SF36_PCS.FU03", "SF36_PCS.FU04", "SF36_PCS.FU05", "SF36_PCS.FU06", "SF36_PCS.FU07", "SF36_PCS.FU08", "SF36_PCS.FU09", "SF36_PCS.FU10", "SF36_PCS.FU11")
missing_SF36_PCS <- numeric(nrow(dataset_with_IPA))
for (i in 1:nrow(dataset_with_IPA)) {
  missing_SF36_PCS[i] <- sum(!is.na(dataset_with_IPA[i, variables_SF36_PCS]))
}

dataset_with_IPA$valid_timepoints_SF36_PCS <- missing_SF36_PCS

dataset_with_IPA <- dataset_with_IPA %>%
  mutate(valid_timepoints_SF36_PCS = replace(valid_timepoints_SF36_PCS, valid_timepoints_SF36_PCS == 0, NA))

# SF-36 MCS
variables_SF36_MCS <- c("SF36_MCS.FU01", "SF36_MCS.FU02", "SF36_MCS.FU03", "SF36_MCS.FU04", "SF36_MCS.FU05", "SF36_MCS.FU06", "SF36_MCS.FU07", "SF36_MCS.FU08", "SF36_MCS.FU09", "SF36_MCS.FU10", "SF36_MCS.FU11")
missing_SF36_MCS <- numeric(nrow(dataset_with_IPA))
for (i in 1:nrow(dataset_with_IPA)) {
  missing_SF36_MCS[i] <- sum(!is.na(dataset_with_IPA[i, variables_SF36_MCS]))
}

dataset_with_IPA$valid_timepoints_SF36_MCS <- missing_SF36_MCS

dataset_with_IPA <- dataset_with_IPA %>%
  mutate(valid_timepoints_SF36_MCS = replace(valid_timepoints_SF36_MCS, valid_timepoints_SF36_MCS == 0, NA))

# IPA
variables_IPA <- c("IPA.FU01", "IPA.FU02", "IPA.FU03", "IPA.FU04", "IPA.FU05", "IPA.FU06", "IPA.FU07", "IPA.FU08", "IPA.FU09", "IPA.FU10", "IPA.FU11")
missing_IPA <- numeric(nrow(dataset_with_IPA))
for (i in 1:nrow(dataset_with_IPA)) {
  missing_IPA[i] <- sum(!is.na(dataset_with_IPA[i, variables_IPA]))
}

dataset_with_IPA$valid_timepoints_IPA <- missing_IPA

dataset_with_IPA <- dataset_with_IPA %>%
  mutate(valid_timepoints_IPA = replace(valid_timepoints_IPA, valid_timepoints_IPA == 0, NA))

# Add/update labels
label(dataset_with_IPA$Education_level) <- "Education level"
label(dataset_with_IPA$DT2_status) <- "Diabetes status"
label(dataset_with_IPA$Smoking_status) <- "Smoking status"
label(dataset_with_IPA$Marital_status) <- "Marital status"
label(dataset_with_IPA$Employment_status) <- "Employment status"
label(dataset_with_IPA$DHD_sum) <- "Dutch Healthy Diet index"
label(dataset_with_IPA$mean_sed_hr_wake_T) <- "Mean sedentary wake hours per day"
label(dataset_with_IPA$mean_sed_hr_wake_WK) <- "Mean sedentary wake hours per day weekdays"
label(dataset_with_IPA$mean_sed_hr_wake_WD) <- "Mean sedentary wake hours per day weekend days"
label(dataset_with_IPA$mean_step_min_wake_T) <- "Mean step wake minutes per day"
label(dataset_with_IPA$mean_step_min_wake_WK) <- "Mean step wake minutes per day week days"
label(dataset_with_IPA$mean_step_min_wake_WD) <- "Mean step wake minutes per day weekend days"
label(dataset_with_IPA$mean_MVPA_min_wake_T) <- "Mean MVPA minutes per day"
label(dataset_with_IPA$mean_MVPA_min_wake_WK) <- "Mean MVPA minutes per day week days"
label(dataset_with_IPA$mean_MVPA_min_wake_WD) <- "Mean MVPA minutes per day weekend days"

label(dataset_with_IPA$IPA_baseline) <- "Baseline social functioning"
label(dataset_with_IPA$IPA.FU01) <- "IPA autonomy outdoors subscale timepoint 1"
label(dataset_with_IPA$IPA.FU02) <- "IPA autonomy outdoors subscale timepoint 2"
label(dataset_with_IPA$IPA.FU03) <- "IPA autonomy outdoors subscale timepoint 3"
label(dataset_with_IPA$IPA.FU04) <- "IPA autonomy outdoors subscale timepoint 4"
label(dataset_with_IPA$IPA.FU05) <- "IPA autonomy outdoors subscale timepoint 5"
label(dataset_with_IPA$IPA.FU06) <- "IPA autonomy outdoors subscale timepoint 6"
label(dataset_with_IPA$IPA.FU07) <- "IPA autonomy outdoors subscale timepoint 7"
label(dataset_with_IPA$IPA.FU08) <- "IPA autonomy outdoors subscale timepoint 8"
label(dataset_with_IPA$IPA.FU09) <- "IPA autonomy outdoors subscale timepoint 9"
label(dataset_with_IPA$IPA.FU10) <- "IPA autonomy outdoors subscale timepoint 10"
label(dataset_with_IPA$IPA.FU11) <- "IPA autonomy outdoors subscale timepoint 11"

label(dataset_with_IPA$SF36_MCS_baseline) <- "Baseline mental functioning"
label(dataset_with_IPA$SF36_MCS.FU01) <- "SF36 Mental component summary score timepoint 1"
label(dataset_with_IPA$SF36_MCS.FU02) <- "SF36 Mental component summary score timepoint 2"
label(dataset_with_IPA$SF36_MCS.FU03) <- "SF36 Mental component summary score timepoint 3"
label(dataset_with_IPA$SF36_MCS.FU04) <- "SF36 Mental component summary score timepoint 4"
label(dataset_with_IPA$SF36_MCS.FU05) <- "SF36 Mental component summary score timepoint 5"
label(dataset_with_IPA$SF36_MCS.FU06) <- "SF36 Mental component summary score timepoint 6"
label(dataset_with_IPA$SF36_MCS.FU07) <- "SF36 Mental component summary score timepoint 7"
label(dataset_with_IPA$SF36_MCS.FU08) <- "SF36 Mental component summary score timepoint 8"
label(dataset_with_IPA$SF36_MCS.FU09) <- "SF36 Mental component summary score timepoint 9"
label(dataset_with_IPA$SF36_MCS.FU10) <- "SF36 Mental component summary score timepoint 10"
label(dataset_with_IPA$SF36_MCS.FU11) <- "SF36 Mental component summary score timepoint 11"

label(dataset_with_IPA$SF36_PCS_baseline) <- "Baseline physical functioning"
label(dataset_with_IPA$SF36_PCS.FU01) <- "SF36 Physical component summary score timepoint 1"
label(dataset_with_IPA$SF36_PCS.FU02) <- "SF36 Physical component summary score timepoint 2"
label(dataset_with_IPA$SF36_PCS.FU03) <- "SF36 Physical component summary score timepoint 3"
label(dataset_with_IPA$SF36_PCS.FU04) <- "SF36 Physical component summary score timepoint 4"
label(dataset_with_IPA$SF36_PCS.FU05) <- "SF36 Physical component summary score timepoint 5"
label(dataset_with_IPA$SF36_PCS.FU06) <- "SF36 Physical component summary score timepoint 6"
label(dataset_with_IPA$SF36_PCS.FU07) <- "SF36 Physical component summary score timepoint 7"
label(dataset_with_IPA$SF36_PCS.FU08) <- "SF36 Physical component summary score timepoint 8"
label(dataset_with_IPA$SF36_PCS.FU09) <- "SF36 Physical component summary score timepoint 9"
label(dataset_with_IPA$SF36_PCS.FU10) <- "SF36 Physical component summary score timepoint 10"
label(dataset_with_IPA$SF36_PCS.FU11) <- "SF36 Physical component summary score timepoint 11"

# Merge variables of hourly PA in variables of 2 hours range
dataset_with_IPA <- recode_hourly_PA(dataset_with_IPA, "WEEK")
dataset_with_IPA <- recode_hourly_PA(dataset_with_IPA, "WKND")

# Recode the date of visit 1 as the year of visit 1
dataset_with_IPA$VISIT1_DATE <- as.character(dataset_with_IPA$VISIT1_DATE)
dataset_with_IPA$Year_visit1 <- substr(dataset_with_IPA$VISIT1_DATE, 1, 4)
dataset_with_IPA$Year_visit1 <- as.factor(dataset_with_IPA$Year_visit1)
dataset_with_IPA$VISIT1_DATE <- NULL

# Create a dataset for complete case analysis
complete_cases <- dataset_with_IPA[complete.cases(dataset_with_IPA[c("PA_pattern", "BMI", "Sex", "Age", "DT2_status", "DHD_sum", "IPA_baseline", "SF36_PCS_baseline", "SF36_MCS_baseline", "Smoking_status", "Marital_status", "Education_level", "valid_timepoints_SF36_PCS", "valid_timepoints_SF36_MCS", "valid_timepoints_IPA")]), ]

# Create a dataset for excluded participants
excluded_participants <- dataset_with_IPA[!complete.cases(dataset_with_IPA[c("PA_pattern", "BMI", "Sex", "Age", "DT2_status", "DHD_sum", "IPA_baseline", "SF36_PCS_baseline", "SF36_MCS_baseline", "Smoking_status", "Marital_status", "Education_level", "valid_timepoints_SF36_PCS", "valid_timepoints_SF36_MCS", "valid_timepoints_IPA")]), ]

rm(
  formatted_set_number, Q1, Q2, Q3, Q4, Q5, set_number, variable_name,
  global_score_IPA, i, missing_IPA, missing_SF36_MCS, missing_SF36_PCS,
  variables_IPA, variables_SF36_MCS, variables_SF36_PCS
)

#-------------------------------------------------------------------------------
# Graphical exploration of continuous variables
#-------------------------------------------------------------------------------

plot_continuous(complete_cases, "mean_sed_hr_wake_T", 0, 20)

plot_continuous(complete_cases, "mean_sed_hr_wake_WK", 0, 20)

plot_continuous(complete_cases, "mean_sed_hr_wake_WD", 0, 20)

plot_continuous(complete_cases, "mean_step_min_wake_T", 0, 500)

plot_continuous(complete_cases, "mean_step_min_wake_WK", 0, 500)

plot_continuous(complete_cases, "mean_step_min_wake_WD", 0, 500)

plot_continuous(complete_cases, "mean_MVPA_min_wake_T", 0, 415)

plot_continuous(complete_cases, "mean_MVPA_min_wake_WK", 0, 415)

plot_continuous(complete_cases, "mean_MVPA_min_wake_WD", 0, 415)

plot_continuous(complete_cases, "BMI", 10, 60)

plot_continuous(complete_cases, "Age", 35, 85)

plot_continuous(complete_cases, "DHD_sum", 0, 150)

plot_continuous(complete_cases, "SF36_PCS_baseline", 0, 100)

plot_continuous(complete_cases, "SF36_MCS_baseline", 0, 100)

plot_continuous(complete_cases, "IPA_baseline", 0, 100)

#-------------------------------------------------------------------------------
# Create plots to describe hourly PA duration for the PA patterns
#-------------------------------------------------------------------------------

curve_patterns(complete_cases, "WEEK", "Weekday hours", "Curve patterns weekdays")
curve_patterns(complete_cases, "WKND", "Weekend day hours", "Curve patterns weekend days")

#-------------------------------------------------------------------------------
# Descriptive statistics tables
#-------------------------------------------------------------------------------

# Create dataset with variables of interest for descriptives
subset_descriptives <- subset(complete_cases,
                              select = c(
                                PA_pattern, Age, Sex, Education_level, DT2_status,
                                DHD_sum, Smoking_status, Marital_status,
                                Employment_status, BMI, SF36_PCS_baseline, 
                                SF36_MCS_baseline, IPA_baseline, 
                                mean_sed_hr_wake_T, mean_step_min_wake_T, 
                                mean_MVPA_min_wake_T
                              )
)

# Create dataset with both included and excluded participants
complete_cases$Group <- "Included"
excluded_participants$Group <- "Excluded"
incl_excl_dataset <- rbind(complete_cases, excluded_participants)
incl_excl_dataset$Group <- as.factor(incl_excl_dataset$Group)

subset_incl_excl <- subset(incl_excl_dataset,
                           select = c(
                             PA_pattern, Age, Sex, Education_level, DT2_status,
                             DHD_sum, Smoking_status, Marital_status,
                             Employment_status, BMI, SF36_PCS_baseline, 
                             SF36_MCS_baseline, IPA_baseline, mean_sed_hr_wake_T, 
                             mean_step_min_wake_T, mean_MVPA_min_wake_T, Group
                           )
)

# Create tables with descriptives
descriptives(subset_descriptives, "PA_pattern", "Descriptives_PA_pattern",1400, 800)
descriptives(subset_descriptives, "DT2_status", "Descriptives_DT2_status",800, 650)
descriptives(subset_incl_excl, "Group", "Descriptives_incl_excl",800, 650)

rm(subset_descriptives, subset_incl_excl)

#-------------------------------------------------------------------------------
# Follow-up rate calculation
#-------------------------------------------------------------------------------

FU_rate("SF36_PCS")
FU_rate("SF36_MCS")
FU_rate("IPA")

#-------------------------------------------------------------------------------
# Creation of long-form datasets for main analysis
#-------------------------------------------------------------------------------

# Datasets with all included participants
PCS_long_complete_cases <- long_dataset(complete_cases, "SF36_PCS")
MCS_long_complete_cases <- long_dataset(complete_cases, "SF36_MCS")
IPA_long_complete_cases <- long_dataset(complete_cases, "IPA")

# Datasets with participants with diabetes type 2
diabetes_complete_cases <- subset(complete_cases, DT2_status == "Type 2 diabetes")

diabetes_PCS_long_complete_cases <- long_dataset(diabetes_complete_cases, "SF36_PCS")
diabetes_MCS_long_complete_cases <- long_dataset(diabetes_complete_cases, "SF36_MCS")
diabetes_IPA_long_complete_cases <- long_dataset(diabetes_complete_cases, "IPA")

# Datasets with participants without diabetes type 2
no_diabetes_complete_cases <- subset(complete_cases, DT2_status == "No type 2 diabetes")

no_diabetes_PCS_long_complete_cases <- long_dataset(no_diabetes_complete_cases, "SF36_PCS")
no_diabetes_MCS_long_complete_cases <- long_dataset(no_diabetes_complete_cases, "SF36_MCS")
no_diabetes_IPA_long_complete_cases <- long_dataset(no_diabetes_complete_cases, "IPA") 

# Datasets with males
male_complete_cases <- subset(complete_cases, Sex == "Male")
male_PCS_long_complete_cases <- long_dataset(male_complete_cases, "SF36_PCS")

# Datasets with females
female_complete_cases <- subset(complete_cases, Sex == "Female")
female_PCS_long_complete_cases <- long_dataset(female_complete_cases, "SF36_PCS")

#-------------------------------------------------------------------------------
# Descriptive curves: relationship between HRF and time per PA pattern
#-------------------------------------------------------------------------------

# For participants with diabetes
plot_association(diabetes_PCS_long_complete_cases, 
                 "SF36_PCS", 
                 "Follow-up time point",
                 "SF-36 PCS score",
                 "diabetes_PCS_curves_time",
                 "diabetes_PCS_reg_time", c(30, 65))

plot_association(diabetes_MCS_long_complete_cases, 
                 "SF36_MCS", 
                  "Follow-up time point",
                 "SF-36 MCS score",
                 "diabetes_MCS_curves_time",
                 "diabetes_MCS_reg_time", c(30, 65))

plot_association(diabetes_IPA_long_complete_cases, 
                 "IPA", 
                 "Follow-up time point",
                 "IPA autonomy outdoors score",
                 "diabetes_IPA_curves_time",
                 "diabetes_IPA_reg_time", c(60, 95))

# For participants without diabetes
plot_association(no_diabetes_PCS_long_complete_cases, 
                 "SF36_PCS", 
                 "Follow-up time point",
                 "SF-36 PCS score",
                 "no_diabetes_PCS_curves_time",
                 "no_diabetes_PCS_reg_time", c(30, 65))

plot_association(no_diabetes_MCS_long_complete_cases, 
                 "SF36_MCS", 
                 "Follow-up time point",
                 "SF-36 MCS score",
                 "no_diabetes_MCS_curves_time",
                 "no_diabetes_MCS_reg_time", c(30, 65))

plot_association(no_diabetes_IPA_long_complete_cases, 
                 "IPA", 
                 "Follow-up time point",
                 "IPA autonomy outdoors score",
                 "no_diabetes_IPA_curves_time",
                 "no_diabetes_IPA_reg_time", c(60, 95))


#-------------------------------------------------------------------------------
# Results in the whole sample
#-------------------------------------------------------------------------------

models_PCS <- models_subgroup("SF36_PCS", PCS_long_complete_cases, complete_cases, "model_PCS")
models_MCS <- models_subgroup("SF36_MCS", MCS_long_complete_cases, complete_cases, "model_MCS")
models_IPA <- models_subgroup("IPA", IPA_long_complete_cases, complete_cases, "model_IPA")

list2env(c(models_PCS, models_MCS, models_IPA), envir = .GlobalEnv)
rm(models_PCS, models_MCS, models_IPA)

table_3_models(model_PCS_1, model_PCS_2, model_PCS_3, "Physical functioning in the global sample", "Results_PCS_whole_sample")
table_3_models(model_MCS_1, model_PCS_2, model_MCS_3, "Mental functioning in the global sample", "Results_MCS_whole_sample")
table_3_models(model_IPA_1, model_IPA_2, model_IPA_3, "Social functioning in the global sample", "Results_IPA_whole_sample")

# Mean decrease over time
decrease_PCS <- model_PCS_3$coefficients[["Follow-up time point"]]
decrease_MCS <- model_MCS_3$coefficients[["Follow-up time point"]]
decrease_IPA <- model_IPA_3$coefficients[["Follow-up time point"]]

#-------------------------------------------------------------------------------
# Models to test interaction between PA patterns and sex
#-------------------------------------------------------------------------------

int_PCS_sex <- models_interaction(PCS_long_complete_cases, complete_cases, "SF36_PCS", "Sex", "int_sex_PCS")
int_MCS_sex <- models_interaction(MCS_long_complete_cases, complete_cases, "SF36_MCS", "Sex", "int_sex_MCS")
int_IPA_sex <- models_interaction(IPA_long_complete_cases, complete_cases, "IPA", "Sex", "int_sex_IPA")

list2env(c(int_PCS_sex, int_MCS_sex, int_IPA_sex), envir = .GlobalEnv)
rm(int_PCS_sex, int_MCS_sex, int_IPA_sex)

table_interaction(int_sex_PCS_1, int_sex_PCS_2, int_sex_PCS_3, "Sex", "Interaction between PA patterns and sex for physical functioning", "Int_sex_PCS")
table_interaction(int_sex_MCS_1, int_sex_MCS_2, int_sex_MCS_3, "Sex", "Interaction between PA patterns and sex for mental functioning", "Int_sex_MCS")
table_interaction(int_sex_IPA_1, int_sex_IPA_2, int_sex_IPA_3, "Sex", "Interaction between PA patterns and sex for social functioning", "Int_sex_IPA")

#-------------------------------------------------------------------------------
# Models to test interaction between PA patterns and diabetes status
#-------------------------------------------------------------------------------

int_PCS_DT2 <- models_interaction(PCS_long_complete_cases, complete_cases, "SF36_PCS", "DT2_status", "int_DT2_PCS")
int_MCS_DT2 <- models_interaction(MCS_long_complete_cases, complete_cases, "SF36_MCS", "DT2_status", "int_DT2_MCS")
int_IPA_DT2 <- models_interaction(IPA_long_complete_cases, complete_cases, "IPA", "DT2_status", "int_DT2_IPA")

list2env(c(int_PCS_DT2, int_MCS_DT2, int_IPA_DT2), envir = .GlobalEnv)
rm(int_PCS_DT2, int_MCS_DT2, int_IPA_DT2)

table_interaction(int_DT2_PCS_1, int_DT2_PCS_2, int_DT2_PCS_3, "DT2_status", "Interaction between PA patterns and T2DM status for physical functioning", "Int_DT2_PCS")
table_interaction(int_DT2_MCS_1, int_DT2_MCS_2, int_DT2_MCS_3, "DT2_status", "Interaction between PA patterns and T2DM status  for mental functioning", "Int_DT2_MCS")
table_interaction(int_DT2_IPA_1, int_DT2_IPA_2, int_DT2_IPA_3, "DT2_status", "Interaction between PA patterns and T2DM status  for social functioning", "Int_DT2_IPA")

#-------------------------------------------------------------------------------
# Models to test interaction between PA patterns and sex in subgroups
#-------------------------------------------------------------------------------

# In participants with type 2 diabetes
int_PCS_sex <- models_interaction_subgroup(diabetes_PCS_long_complete_cases, diabetes_complete_cases, "SF36_PCS", "Sex", "int_sex_PCS_DT2")
int_MCS_sex <- models_interaction_subgroup(diabetes_MCS_long_complete_cases, diabetes_complete_cases, "SF36_MCS", "Sex", "int_sex_MCS_DT2")
int_IPA_sex <- models_interaction_subgroup(diabetes_IPA_long_complete_cases, diabetes_complete_cases, "IPA", "Sex", "int_sex_IPA_DT2")

list2env(c(int_PCS_sex, int_MCS_sex, int_IPA_sex), envir = .GlobalEnv)
rm(int_PCS_sex, int_MCS_sex, int_IPA_sex)

table_interaction(int_sex_PCS_DT2_1, int_sex_PCS_DT2_2, int_sex_PCS_DT2_3, "Sex", "Interaction between PA patterns and sex in participants with T2DM for physical functioning", "Int_sex_PCS_DT2")
table_interaction(int_sex_MCS_DT2_1, int_sex_MCS_DT2_2, int_sex_MCS_DT2_3, "Sex", "Interaction between PA patterns and sex in participants with T2DM for mental functioning", "Int_sex_MCS_DT2")
table_interaction(int_sex_IPA_DT2_1, int_sex_IPA_DT2_2, int_sex_IPA_DT2_3, "Sex", "Interaction between PA patterns and sex in participants with T2DM for social functioning", "Int_sex_IPA_DT2")

# In participants without type 2 diabetes
int_PCS_sex <- models_interaction_subgroup(no_diabetes_PCS_long_complete_cases, no_diabetes_complete_cases, "SF36_PCS", "Sex", "int_sex_PCS_no_DT2")
int_MCS_sex <- models_interaction_subgroup(no_diabetes_MCS_long_complete_cases, no_diabetes_complete_cases, "SF36_MCS", "Sex", "int_sex_MCS_no_DT2")
int_IPA_sex <- models_interaction_subgroup(no_diabetes_IPA_long_complete_cases, no_diabetes_complete_cases, "IPA", "Sex", "int_sex_IPA_no_DT2")

list2env(c(int_PCS_sex, int_MCS_sex, int_IPA_sex), envir = .GlobalEnv)
rm(int_PCS_sex, int_MCS_sex, int_IPA_sex)

table_interaction(int_sex_PCS_no_DT2_1, int_sex_PCS_no_DT2_2, int_sex_PCS_no_DT2_3, "Sex", "Interaction between PA patterns and sex in participants without T2DM for physical functioning", "Int_sex_PCS_no_DT2")
table_interaction(int_sex_MCS_no_DT2_1, int_sex_MCS_no_DT2_2, int_sex_MCS_no_DT2_3, "Sex", "Interaction between PA patterns and sex in participants without T2DM for mental functioning", "Int_sex_MCS_no_DT2")
table_interaction(int_sex_IPA_no_DT2_1, int_sex_IPA_no_DT2_2, int_sex_IPA_no_DT2_3, "Sex", "Interaction between PA patterns and sex in participants without T2DM for social functioning", "Int_sex_IPA_no_DT2")

#-------------------------------------------------------------------------------
# Main analysis - results stratified by T2DM status
#-------------------------------------------------------------------------------

# The assumptions are also checked when creating the models

# Models in participants with T2DM
models_PCS_diabetes <- models_subgroup("SF36_PCS", diabetes_PCS_long_complete_cases, diabetes_complete_cases, "model_diabetes_PCS")
models_MCS_diabetes <- models_subgroup("SF36_MCS", diabetes_MCS_long_complete_cases, diabetes_complete_cases, "model_diabetes_MCS")
models_IPA_diabetes <- models_subgroup("IPA", diabetes_IPA_long_complete_cases, diabetes_complete_cases, "model_diabetes_IPA")
list2env(c(models_PCS_diabetes, models_MCS_diabetes, models_IPA_diabetes), envir = .GlobalEnv)

# Models in participants without T2DM
models_PCS_no_diabetes <- models_subgroup("SF36_PCS", no_diabetes_PCS_long_complete_cases, no_diabetes_complete_cases, "model_no_diabetes_PCS")
models_MCS_no_diabetes <- models_subgroup("SF36_MCS", no_diabetes_MCS_long_complete_cases, no_diabetes_complete_cases, "model_no_diabetes_MCS")
models_IPA_no_diabetes <- models_subgroup("IPA", no_diabetes_IPA_long_complete_cases, no_diabetes_complete_cases, "model_no_diabetes_IPA")
list2env(c(models_PCS_no_diabetes, models_MCS_no_diabetes, models_IPA_no_diabetes), envir = .GlobalEnv)

rm(models_PCS_diabetes, models_MCS_diabetes, models_IPA_diabetes, 
   models_PCS_no_diabetes, models_MCS_no_diabetes, models_IPA_no_diabetes)

# Results in participants with T2DM
table_3_models(model_diabetes_PCS_1, model_diabetes_PCS_2, model_diabetes_PCS_3, "Physical functioning in participants with T2DM", "Results_PCS_diabetes")
table_3_models(model_diabetes_MCS_1, model_diabetes_PCS_2, model_diabetes_MCS_3, "Mental functioning in participants with T2DM", "Results_MCS_diabetes")
table_3_models(model_diabetes_IPA_1, model_diabetes_IPA_2, model_diabetes_IPA_3, "Social functioning in participants with T2DM", "Results_IPA_diabetes")

# Results in participants without T2DM
table_3_models(model_no_diabetes_PCS_1, model_no_diabetes_PCS_2, model_no_diabetes_PCS_3, "Physical functioning in participants without T2DM", "Results_PCS_no_diabetes")
table_3_models(model_no_diabetes_MCS_1, model_no_diabetes_PCS_2, model_no_diabetes_MCS_3, "Mental functioning in participants without T2DM", "Results_MCS_no_diabetes")
table_3_models(model_no_diabetes_IPA_1, model_no_diabetes_IPA_2, model_no_diabetes_IPA_3, "Social functioning in participants without T2DM", "Results_IPA_no_diabetes")

# Results in both strata (only results from model 3)
table_1_model(model_diabetes_PCS_3, model_no_diabetes_PCS_3, "Physical functioning stratified by diabetes type 2 status", "Results_PCS")
table_1_model(model_diabetes_MCS_3, model_no_diabetes_MCS_3, "Mental functioning stratified by diabetes type 2 status", "Results_MCS")
table_1_model(model_diabetes_IPA_3, model_no_diabetes_IPA_3, "Social functioning stratified by diabetes type 2 status", "Results_IPA")

#-------------------------------------------------------------------------------
# Sensitivity analysis with addition of employment status in models
#-------------------------------------------------------------------------------

# The assumptions are also checked when creating the models

#Creation of necessary datasets
diabetes_employment_complete_cases <- subset(complete_cases, DT2_status == "Type 2 diabetes" & !is.na(Employment_status))
no_diabetes_employment_complete_cases <- subset(complete_cases, DT2_status == "No type 2 diabetes" & !is.na(Employment_status))

diabetes_employment_PCS_long_complete_cases <- long_dataset(diabetes_employment_complete_cases, "SF36_PCS")
diabetes_employment_MCS_long_complete_cases <- long_dataset(diabetes_employment_complete_cases, "SF36_MCS")
diabetes_employment_IPA_long_complete_cases <- long_dataset(diabetes_employment_complete_cases, "IPA")

no_diabetes_employment_PCS_long_complete_cases <- long_dataset(no_diabetes_employment_complete_cases, "SF36_PCS")
no_diabetes_employment_MCS_long_complete_cases <- long_dataset(no_diabetes_employment_complete_cases, "SF36_MCS")
no_diabetes_employment_IPA_long_complete_cases <- long_dataset(no_diabetes_employment_complete_cases, "IPA")

# Models in participants with T2DM
models_PCS_employment_diabetes <- models_subgroup("SF36_PCS", diabetes_employment_PCS_long_complete_cases, diabetes_employment_complete_cases, "model_employment_diabetes_PCS")
models_MCS_employment_diabetes <- models_subgroup("SF36_MCS", diabetes_employment_MCS_long_complete_cases, diabetes_employment_complete_cases, "model_employment_diabetes_MCS")
models_IPA_employment_diabetes <- models_subgroup("IPA", diabetes_employment_IPA_long_complete_cases, diabetes_employment_complete_cases, "model_employment_diabetes_IPA")
list2env(c(models_PCS_employment_diabetes, models_MCS_employment_diabetes, models_IPA_employment_diabetes), envir = .GlobalEnv)

# Models in participants without T2DM
models_PCS_employment_no_diabetes <- models_subgroup("SF36_PCS", no_diabetes_employment_PCS_long_complete_cases, no_diabetes_employment_complete_cases, "model_employment_no_diabetes_PCS")
models_MCS_employment_no_diabetes <- models_subgroup("SF36_MCS", no_diabetes_employment_MCS_long_complete_cases, no_diabetes_employment_complete_cases, "model_employment_no_diabetes_MCS")
models_IPA_employment_no_diabetes <- models_subgroup("IPA", no_diabetes_employment_IPA_long_complete_cases, no_diabetes_employment_complete_cases, "model_employment_no_diabetes_IPA")
list2env(c(models_PCS_employment_no_diabetes, models_MCS_employment_no_diabetes, models_IPA_employment_no_diabetes), envir = .GlobalEnv)

rm(models_PCS_employment_diabetes, models_MCS_employment_diabetes, 
   models_IPA_employment_diabetes, models_PCS_employment_no_diabetes, 
   models_MCS_employment_no_diabetes, models_IPA_employment_no_diabetes)

# Results in participants with T2DM
table_3_models(model_employment_diabetes_PCS_1, model_employment_diabetes_PCS_2, model_employment_diabetes_PCS_3, "Sensitivity analysis with employment status: physical functioning in participants with T2DM", "Results_employment_PCS_diabetes")
table_3_models(model_employment_diabetes_MCS_1, model_employment_diabetes_PCS_2, model_employment_diabetes_MCS_3, "Sensitivity analysis with employment status: mental functioning in participants with T2DM", "Results_employment_MCS_diabetes")
table_3_models(model_employment_diabetes_IPA_1, model_employment_diabetes_IPA_2, model_employment_diabetes_IPA_3, "Sensitivity analysis with employment status: social functioning in participants with T2DM", "Results_employment_IPA_diabetes")

# Results in participants without T2DM
table_3_models(model_employment_no_diabetes_PCS_1, model_employment_no_diabetes_PCS_2, model_employment_no_diabetes_PCS_3, "Sensitivity analysis with employment status: physical functioning in participants without T2DM", "Results_employment_PCS_no_diabetes")
table_3_models(model_employment_no_diabetes_MCS_1, model_employment_no_diabetes_PCS_2, model_employment_no_diabetes_MCS_3, "Sensitivity analysis with employment status: mental functioning in participants without T2DM", "Results_employment_MCS_no_diabetes")
table_3_models(model_employment_no_diabetes_IPA_1, model_employment_no_diabetes_IPA_2, model_employment_no_diabetes_IPA_3, "Sensitivity analysis with employment status: social functioning in participants without T2DM", "Results_employment_IPA_no_diabetes")

# Results in both strata (only results from model 3)
table_1_model(model_employment_diabetes_PCS_3, model_employment_no_diabetes_PCS_3, "Sensitivity analysis with employment status: physical functioning stratified by diabetes type 2 status", "Results_employment_PCS")
table_1_model(model_employment_diabetes_MCS_3, model_employment_no_diabetes_MCS_3, "Sensitivity analysis with employment status: mental functioning stratified by diabetes type 2 status", "Results_employment_MCS")
table_1_model(model_employment_diabetes_IPA_3, model_employment_no_diabetes_IPA_3, "Sensitivity analysis with employment status: social functioning stratified by diabetes type 2 status", "Results_employment_IPA")

#//TODO
#-------------------------------------------------------------------------------
# Sensitivity analysis in participants with at least 4 valid FU time points
#-------------------------------------------------------------------------------

# Creation of datasets
diabetes_PCS_4_wide_complete_cases <- subset(diabetes_complete_cases, valid_timepoints_SF36_PCS >= 4)
diabetes_PCS_4_long_complete_cases <- long_dataset(diabetes_PCS_4_wide_complete_cases, "SF36_PCS")

diabetes_MCS_4_wide_complete_cases <- subset(diabetes_complete_cases, valid_timepoints_SF36_MCS >= 4)
diabetes_MCS_4_long_complete_cases <- long_dataset(diabetes_MCS_4_wide_complete_cases, "SF36_MCS")

diabetes_IPA_4_wide_complete_cases <- subset(diabetes_complete_cases, valid_timepoints_IPA >= 4)
diabetes_IPA_4_long_complete_cases <- long_dataset(diabetes_IPA_4_wide_complete_cases, "IPA")

# Creation of datasets
no_diabetes_PCS_4_wide_complete_cases <- subset(no_diabetes_complete_cases, valid_timepoints_SF36_PCS >= 4)
no_diabetes_PCS_4_long_complete_cases <- long_dataset(no_diabetes_PCS_4_wide_complete_cases, "SF36_PCS")

no_diabetes_MCS_4_wide_complete_cases <- subset(no_diabetes_complete_cases, valid_timepoints_SF36_MCS >= 4)
no_diabetes_MCS_4_long_complete_cases <- long_dataset(no_diabetes_MCS_4_wide_complete_cases, "SF36_MCS")

no_diabetes_IPA_4_wide_complete_cases <- subset(no_diabetes_complete_cases, valid_timepoints_IPA >= 4)
no_diabetes_IPA_4_long_complete_cases <- long_dataset(no_diabetes_IPA_4_wide_complete_cases, "IPA")

# Models in participants with T2DM
models_PCS_diabetes_4 <- models_subgroup("SF36_PCS", diabetes_PCS_4_long_complete_cases, diabetes_PCS_4_wide_complete_cases, "model_4_diabetes_PCS")
models_MCS_diabetes_4 <- models_subgroup("SF36_MCS", diabetes_MCS_4_long_complete_cases, diabetes_PCS_4_wide_complete_cases, "model_4_diabetes_MCS")
models_IPA_diabetes_4 <- models_subgroup("IPA", diabetes_IPA_4_long_complete_cases, diabetes_PCS_4_wide_complete_cases, "model_4_diabetes_IPA")
list2env(c(models_PCS_diabetes_4, models_MCS_diabetes_4, models_IPA_diabetes_4), envir = .GlobalEnv)

rm(models_PCS_diabetes_4, models_MCS_diabetes_4, models_IPA_diabetes_4)

# Models in participants without T2DM
models_PCS_no_diabetes_4 <- models_subgroup("SF36_PCS", no_diabetes_PCS_4_long_complete_cases, no_diabetes_PCS_4_wide_complete_cases, "model_4_no_diabetes_PCS")
models_MCS_no_diabetes_4 <- models_subgroup("SF36_MCS", no_diabetes_MCS_4_long_complete_cases, no_diabetes_PCS_4_wide_complete_cases, "model_4_no_diabetes_MCS")
models_IPA_no_diabetes_4 <- models_subgroup("IPA", no_diabetes_IPA_4_long_complete_cases, no_diabetes_PCS_4_wide_complete_cases, "model_4_no_diabetes_IPA")
list2env(c(models_PCS_no_diabetes_4, models_MCS_no_diabetes_4, models_IPA_no_diabetes_4), envir = .GlobalEnv)

rm(models_PCS_no_diabetes_4, models_MCS_no_diabetes_4, models_IPA_no_diabetes_4)

# Results in participants with T2DM
table_title_DT2_PCS <- paste("Physical functioning in participants with diabetes type 2 with at least four valid FU time points (N =", nrow(diabetes_PCS_4_wide_complete_cases), ")")
table_title_DT2_MCS <- paste("Mental functioning in participants with diabetes type 2 with at least four valid FU time points (N =", nrow(diabetes_MCS_4_wide_complete_cases), ")")
table_title_DT2_IPA <- paste("Social functioning in participants with diabetes type 2 with at least four valid FU time points (N =", nrow(diabetes_IPA_4_wide_complete_cases), ")")

table_3_models(model_4_diabetes_PCS_1, model_4_diabetes_PCS_2, model_4_diabetes_PCS_3, table_title_DT2_PCS, "Results_PCS_4_FU_diabetes")
table_3_models(model_4_diabetes_MCS_1, model_4_diabetes_PCS_2, model_4_diabetes_MCS_3, table_title_DT2_MCS, "Results_MCS_4_FU_diabetes")
table_3_models(model_4_diabetes_IPA_1, model_4_diabetes_IPA_2, model_4_diabetes_IPA_3, table_title_DT2_IPA, "Results_IPA_4_FU_diabetes")

# Results in participants with T2DM
table_title_no_DT2_PCS <- paste("Physical functioning in participants without diabetes type 2 with at least four valid FU time points (N =", nrow(no_diabetes_PCS_4_wide_complete_cases), ")")
table_title_no_DT2_MCS <- paste("Mental functioning in participants without diabetes type 2 with at least four valid FU time points (N =", nrow(no_diabetes_MCS_4_wide_complete_cases), ")")
table_title_no_DT2_IPA <- paste("Social functioning in participants without diabetes type 2 with at least four valid FU time points (N =", nrow(no_diabetes_IPA_4_wide_complete_cases), ")")

table_3_models(model_4_no_diabetes_PCS_1, model_4_no_diabetes_PCS_2, model_4_no_diabetes_PCS_3, table_title_no_DT2_PCS, "Results_PCS_4_FU_no_diabetes")
table_3_models(model_4_no_diabetes_MCS_1, model_4_no_diabetes_PCS_2, model_4_no_diabetes_MCS_3, table_title_no_DT2_MCS, "Results_MCS_4_FU_no_diabetes")
table_3_models(model_4_no_diabetes_IPA_1, model_4_no_diabetes_IPA_2, model_4_no_diabetes_IPA_3, table_title_no_DT2_IPA, "Results_IPA_4_FU_no_diabetes")

# Results in both strata (only results from model 3)
table_1_model(model_4_diabetes_PCS_3, model_4_no_diabetes_PCS_3, "Physical functioning stratified by diabetes type 2 status in participants with at least 4 valid follow-up timepoints", "Results_PCS_4_FU")
table_1_model(model_4_diabetes_MCS_3, model_4_no_diabetes_MCS_3, "Mental functioning stratified by diabetes type 2 status in participants with at least 4 valid follow-up timepoints", "Results_MCS_4_FU")
table_1_model(model_4_diabetes_IPA_3, model_4_no_diabetes_IPA_3, "Social functioning stratified by diabetes type 2 status in participants with at least 4 valid follow-up timepoints", "Results_IPA_4_FU")

rm(table_title_DT2_PCS, table_title_DT2_MCS, table_title_DT2_IPA, 
   table_title_no_DT2_PCS, table_title_no_DT2_MCS, table_title_no_DT2_IPA)

#-------------------------------------------------------------------------------
# Create forest plots for presentation
#-------------------------------------------------------------------------------

forest_plot("SF36_PCS", diabetes_PCS_long_complete_cases, "PCS_diabetes")
forest_plot("SF36_PCS", no_diabetes_PCS_long_complete_cases, "PCS_no_diabetes")

forest_plot("SF36_MCS", diabetes_MCS_long_complete_cases, "MCS_diabetes")
forest_plot("SF36_MCS", no_diabetes_MCS_long_complete_cases, "MCS_no_diabetes")

forest_plot("IPA", diabetes_IPA_long_complete_cases, "IPA_diabetes")
forest_plot("IPA", no_diabetes_IPA_long_complete_cases, "IPA_no_diabetes")

#-------------------------------------------------------------------------------
# Additional analysis: run models in 65+ years old participants
#-------------------------------------------------------------------------------

older_participants <- subset(complete_cases, Age >= 65)

older_PCS_long_complete_cases <- long_dataset(older_participants, "SF36_PCS")
older_MCS_long_complete_cases <- long_dataset(older_participants, "SF36_MCS")
older_IPA_long_complete_cases <- long_dataset(older_participants, "IPA")

# Create models
models_PCS_older <- models_subgroup("SF36_PCS", older_PCS_long_complete_cases, older_participants, "PCS_older")
models_MCS_older <- models_subgroup("SF36_MCS", older_MCS_long_complete_cases, older_participants, "MCS_older")
models_IPA_older <- models_subgroup("IPA", older_IPA_long_complete_cases, older_participants, "IPA_older")

list2env(c(models_PCS_older, models_MCS_older, models_IPA_older), envir = .GlobalEnv)
rm(models_PCS_older, models_MCS_older, models_IPA_older)

# Generate results

title_PCS <- paste("Physical functioning in participants older than 65 (N =", nrow(older_participants), ")")
title_MCS <- paste("Mental functioning in participants older than 65 (N =", nrow(older_participants), ")")
title_IPA <- paste("Social functioning in participants older than 65 (N =", nrow(older_participants), ")")

table_3_models(PCS_older_1, PCS_older_2, PCS_older_3, title_PCS, "PCS_65+")
table_3_models(MCS_older_1, MCS_older_2, MCS_older_3, title_MCS, "MCS_65+")
table_3_models(IPA_older_1, IPA_older_2, IPA_older_3, title_IPA, "IPA_65+")

# Mean decrease over time
decrease_PCS_older <- PCS_older_3$coefficients[["Follow-up time point"]]
decrease_MCS_older <- MCS_older_3$coefficients[["Follow-up time point"]]
decrease_IPA_older <- IPA_older_3$coefficients[["Follow-up time point"]]

rm(title_PCS, title_MCS, title_IPA)

#-------------------------------------------------------------------------------
# Additional analysis: add the year of baseline measurement in the models
#-------------------------------------------------------------------------------

# In participants with diabetes
models_PCS_date_diabetes <- models_date("SF36_PCS", diabetes_PCS_long_complete_cases, diabetes_complete_cases, "model_PCS_date_diabetes")
models_MCS_date_diabetes <- models_date("SF36_MCS", diabetes_MCS_long_complete_cases, diabetes_complete_cases, "model_MCS_date_diabetes")
models_IPA_date_diabetes <- models_date("IPA", diabetes_IPA_long_complete_cases, diabetes_complete_cases, "model_IPA_date_diabetes")

list2env(c(models_PCS_date_diabetes, models_MCS_date_diabetes, models_IPA_date_diabetes), envir = .GlobalEnv)
rm(models_PCS_date_diabetes, models_MCS_date_diabetes, models_IPA_date_diabetes)

table_3_models(model_PCS_date_diabetes_1, model_PCS_date_diabetes_2, model_PCS_date_diabetes_3, "Physical functioning in participants with T2DM when adding year of visit 1 to the model", "Results_PCS_diabetes_date")
table_3_models(model_MCS_date_diabetes_1, model_MCS_date_diabetes_2, model_MCS_date_diabetes_3, "Mental functioning in participants with T2DM when adding year of visit 1 to the model", "Results_MCS_diabetes_date")
table_3_models(model_IPA_date_diabetes_1, model_IPA_date_diabetes_2, model_IPA_date_diabetes_3, "Social functioning in participants with T2DM when adding year of visit 1 to the model", "Results_IPA_diabetes_date")

# In participants without diabetes
models_PCS_date_no_diabetes <- models_date("SF36_PCS", no_diabetes_PCS_long_complete_cases, no_diabetes_complete_cases, "model_PCS_date_no_diabetes")
models_MCS_date_no_diabetes <- models_date("SF36_MCS", no_diabetes_MCS_long_complete_cases, no_diabetes_complete_cases, "model_MCS_date_no_diabetes")
models_IPA_date_no_diabetes <- models_date("IPA", no_diabetes_IPA_long_complete_cases, no_diabetes_complete_cases, "model_IPA_date_no_diabetes")

list2env(c(models_PCS_date_no_diabetes, models_MCS_date_no_diabetes, models_IPA_date_no_diabetes), envir = .GlobalEnv)
rm(models_PCS_date_no_diabetes, models_MCS_date_no_diabetes, models_IPA_date_no_diabetes)

table_3_models(model_PCS_date_no_diabetes_1, model_PCS_date_no_diabetes_2, model_PCS_date_no_diabetes_3, "Physical functioning in participants without T2DM when adding year of visit 1 to the model", "Results_PCS_no_diabetes_date")
table_3_models(model_MCS_date_no_diabetes_1, model_MCS_date_no_diabetes_2, model_MCS_date_no_diabetes_3, "Mental functioning in participants without T2DM when adding year of visit 1 to the model", "Results_MCS_no_diabetes_date")
table_3_models(model_IPA_date_no_diabetes_1, model_IPA_date_no_diabetes_2, model_IPA_date_no_diabetes_3, "Social functioning in participants without T2DM when adding year of visit 1 to the model", "Results_IPA_no_diabetes_date")

#-------------------------------------------------------------------------------
# Generate histogram with boxplot for data exploration
#-------------------------------------------------------------------------------

plot_continuous <- function(dataset, variable, min, max) {
  
  file_name <- paste("Figures/Data exploration/", variable, ".jpg", sep = "")
  if (!file.exists(file_name)) {
    jpeg(file_name, width = 800, height = 600)
    
    variable <- dataset[[variable]]
    layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE), height = c(1, 8))
    par(mar = c(0, 3.1, 1.1, 2.1))
    boxplot(variable, horizontal = TRUE, ylim = c(min, max), xaxt = "n", col = rgb(0.8, 0.8, 0, 0.5), frame = F)
    par(mar = c(4, 3.1, 1.1, 2.1))
    hist(variable, breaks = 40, col = rgb(0.2, 0.8, 0.5, 0.5), border = F, main = "", xlab = label(variable), xlim = c(min, max))
    
    dev.off()
  }
} 

#-------------------------------------------------------------------------------
# Create table for descriptives
#-------------------------------------------------------------------------------

descriptives <- function(dataset, grouping_variable, file_name, width, height) {
  
  file_name <- paste(file_name, ".png", sep = "")
  
  if (!file.exists(file_name)) {
    table <- tbl_summary(
      dataset,
      by = grouping_variable,
      statistic = list(all_categorical() ~ "{n}    ({p}%)",
                       Age ~ "{mean} ({sd})",
                       BMI ~ "{mean} ({sd})",
                       SF36_PCS_baseline ~ "{median} [{p25}, {p75}]",
                       SF36_MCS_baseline ~ "{median} [{p25}, {p75}]",
                       IPA_baseline ~ "{median} [{p25}, {p75}]",
                       mean_sed_hr_wake_T ~ "{median} [{p25}, {p75}]",
                       mean_step_min_wake_T ~ "{median} [{p25}, {p75}]",
                       mean_MVPA_min_wake_T ~ "{median} [{p25}, {p75}]"),
      digits = list(all_continuous()  ~ c(2, 2),
                    all_categorical() ~ c(0, 2)),
      missing_text = "Missing"
    )
    table <- add_overall(table)
    table <- bold_labels(table)
    table <- add_p(table)
    
    gt_table <- as_gt(table)
    
    css_style <- paste0(
      "width: ", 
      sprintf("%.2f%%", 100 / 8), ";"
    )
    
    gt_table <- gt_table %>%
      tab_options(
        table.layout = "auto",
        summary_row.padding = 50,
        data_row.padding = px(3),
        table.font.size = px(9)  # Set font size to 10px, adjust as needed
      )
    
    # Save the gt_table as HTML temporarily
    html_file <- tempfile(fileext = ".html")
    gtsave(gt_table, filename = html_file)
    
    modified_html <- gsub("\\\\", "/", html_file)
    url <- paste("file://", modified_html, sep="")
    
    webshot2::webshot(
      url = url, 
      file = file_name, 
      zoom = 5, 
      vwidth = width, 
      vheight = height, 
      delay = 2)
  }
}

#-------------------------------------------------------------------------------
# Calculate follow up rate for each time point
#-------------------------------------------------------------------------------

FU_rate <- function(outcome){
  for (set_number in 1:11) {
    formatted_set_number <- sprintf("%02d", set_number)
    variable_name <- paste(outcome, ".FU", formatted_set_number, sep = "")
    percentage <- percentage_FU(dataset_with_IPA, variable_name)
    cat("Data available for", outcome, "at FU timepoint", set_number, ": ", percentage, "%", "\n")
  }
}

#-------------------------------------------------------------------------------
# Calculate the mean decrease in HRF over time
#-------------------------------------------------------------------------------

decrease <- function(dataset, outcome) {
  means <- dataset %>%
    group_by(FU_timepoint) %>%
    dplyr::summarize(mean_outcome = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  model <- lm(mean_outcome ~ FU_timepoint)
  slope <- coef(model)["FU_timepoint"]
  percentage <- (slope / means$mean_outcome[1]) * 100
  
  return(percentage)
}

#-------------------------------------------------------------------------------
# Create plots of relationship between outcome and time per PA pattern
#-------------------------------------------------------------------------------

plot_association <- function (dataset, 
                              outcome, 
                              xlab, 
                              ylab, 
                              file_name_curve,
                              file_name_reg,
                              limits) {
  file_name_curve <- paste("Figures/Descriptive curves association/", file_name_curve, ".jpg", sep = "")
  file_name_reg <- paste("Figures/Descriptive curves association/", file_name_reg, ".jpg", sep = "")
  
  # Calculate mean PCS score for each time point and each PA pattern
  means <- dataset %>%
    group_by(FU_timepoint, PA_pattern) %>%
    dplyr::summarize(mean_outcome = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  # Plot with curves and regression lines
  if (!file.exists(file_name_curve)) {
    plot_curve <- ggplot(means, aes(x = FU_timepoint, y = mean_outcome, color = PA_pattern, group = PA_pattern)) +
      geom_line() +
      ylim(limits) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, linetype = "dotted", aes(group = PA_pattern)) +
      labs(
        x = xlab,
        y = ylab,
        color = "Physical activity patterns"
      ) +
      theme_minimal() +
      scale_color_manual(values = c(
        "#9467BD",
        "#FF7F0E", 
        "#2CA02C", 
        "#D62728", 
        "#1F77B4",
        "#8C564B", 
        "#E377C2"  
      ))
    
    
    ggsave(file_name_curve, plot = plot_curve, device = "jpg", width = 10, height = 6)
  }
  
  # Plot only with regression lines more readable
  if (!file.exists(file_name_reg)) {
    plot_reg <- ggplot(means, aes(x = FU_timepoint, y = mean_outcome, color = PA_pattern, group = PA_pattern)) +
      geom_smooth(method = "lm", se = FALSE, aes(group = PA_pattern)) + 
      ylim(limits) +
      labs(
        x = xlab,
        y = ylab,
        color = "Physical activity patterns"
      ) +
      theme_minimal() +
      scale_color_manual(values = c(
        "#9467BD",
        "#FF7F0E", 
        "#2CA02C", 
        "#D62728", 
        "#1F77B4",
        "#8C564B", 
        "#E377C2"  
      ))
    
    ggsave(file_name_reg, plot = plot_reg, device = "jpg", width = 10, height = 6)
  }
}

#-------------------------------------------------------------------------------
# Create curve for mean activity min for the different PA patterns
#-------------------------------------------------------------------------------

curve_patterns <- function (dataset, WEEK_or_WKND, legend_x_axis, file_name) {
  
  file_name <- paste("Figures/", file_name, ".jpg", sep = "")
  if (!file.exists(file_name)) {
    # Create long dataset
    relevant_columns <- grep(paste("MIN_STEP_", WEEK_or_WKND, sep = ""), names(dataset), value = TRUE)
    long_dataset <- pivot_longer(dataset,
                                 cols = relevant_columns,
                                 names_to = "Hour", values_to = "PA_duration"
    )
    
    long_dataset <- long_dataset %>%
      mutate(
        Hour = recode(Hour,
                      !!relevant_columns[[1]] := "7",
                      !!relevant_columns[[2]] := "9",
                      !!relevant_columns[[3]] := "11",
                      !!relevant_columns[[4]] := "13",
                      !!relevant_columns[[5]] := "15",
                      !!relevant_columns[[6]] := "17",
                      !!relevant_columns[[7]] := "19",
                      !!relevant_columns[[8]] := "21",
                      !!relevant_columns[[9]] := "23"
        ), 
        Hour = as.numeric(Hour)
      )
    
    # Create dataframe with values of interest for the plot
    means <- long_dataset %>%
      group_by(Hour, PA_pattern) %>%
      dplyr::summarize(Mean_PA_duration = mean(.data[["PA_duration"]], na.rm = TRUE), .groups = "drop")
    
    # Create the plot
    curve <- ggplot(means, aes(x = Hour, y = Mean_PA_duration, color = PA_pattern, group = PA_pattern)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = seq(7, 23, by = 2)) +
      ylim(c(0, 42)) +
      labs(
        x = legend_x_axis,
        y = "Mean minutes of activity / 2 hours",
        color = "Physical activity patterns"
      ) +
      theme_minimal() +
      scale_color_manual(values = c(
        "#9467BD",
        "#FF7F0E", 
        "#2CA02C", 
        "#D62728", 
        "#1F77B4",
        "#8C564B", 
        "#E377C2"  
      ))
    
    ggsave(file_name, plot = curve, device = "jpg", width = 6, height = 4)
  }
}
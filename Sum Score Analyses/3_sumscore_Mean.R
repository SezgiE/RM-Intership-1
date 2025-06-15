## Sezgi Ercan - 20.06.2025
## Script 3 for sum score analysis

##---------------------- Plotting Mean Sum scores ----------------------##

# Necessary Libraries
library(tidyverse)
library(openxlsx)
library(stringr)

rm(list = ls()) # clean memory


# This script takes "sumscore_Data.RDS" file from first script as input. !!! Not "sumscore_RawData.RDS" !!!!
# Please set the working directory to where the above-mentioned file is.
setwd("/Users/sezgi/Desktop/scripts/sum_score/imputation_results")

# Please indicate your desired output directory. Outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/trend_results"

#
#-----------------------------Code Starts---------------------------#
#


df <- readRDS("sumscore_Data.RDS")


## Keep the complete cases
data <- df[complete.cases(df[, c("FID", "PID", "mult_id_fam", "sex", "yob", "zyg", "assigned_age")]), ]


## Create 6-year bins for birth year
data <- data %>%
  mutate(
    yob_bin = cut(
      yob,
      breaks = seq(min(yob), max(yob) + 6, by = 6),  # Define breaks for 6-year bins
      include.lowest = TRUE,
      right = FALSE,  # Use left-inclusive, right-exclusive bins
      labels = paste0(seq(min(yob), max(yob), by = 6), "-", seq(min(yob) + 5, max(yob) + 5, by = 6))  # Define labels
    ))


# Calculate binned descriptive statistics for each birth year, rater, and age
descriptive_stats <- data %>%
  group_by(scale, sex, yob_bin, respondent, assigned_age) %>%
  summarise(
    mean_response = mean(score, na.rm = TRUE),  # Mean rating
    sd_response = sd(score, na.rm = TRUE),     # Standard deviation
    n = n(), # Number of observations
    sem_response = sd_response / sqrt(n), # sem
    .groups = 'drop'
  )


plotting_data <- data %>%
  group_by(scale, sex, yob_bin, respondent, assigned_age) %>%
  mutate(yob_bin_plot = ifelse(n() < 100, "2004-2009", as.character(yob_bin))) %>%
  ungroup()

descriptive_stats_plotting <- plotting_data %>%
  group_by(scale, sex, yob_bin_plot, respondent, assigned_age) %>%
  summarise(
    mean_response = mean(score, na.rm = TRUE),  # Mean rating
    sd_response = sd(score, na.rm = TRUE),     # Standard deviation
    n = n(), # Number of observations
    sem_response = sd_response / sqrt(n), # sem
    .groups = 'drop'
  )


## Loop parameters
sexes <- c("male", "female")
scales <- c("emp", "dsm")

scale_names <- list("emp" = "AP", "dsm" = "ADHP")


# Empty plot list
plot_list <- list()


for (scale_type in scales){
  
  scale_name <- scale_names[[scale_type]]
  
  for (gender in sexes){
    
    data_filtered <- descriptive_stats_plotting %>%
      filter(scale == scale_type) %>%
      filter(sex == gender)
    
    
    ## plotting the trend
    plot_trend <- ggplot(data_filtered, aes(x = as.factor(yob_bin_plot), y = mean_response, color = respondent)) +
      geom_line(aes(linetype = as.factor(assigned_age), group = interaction(respondent, assigned_age)), size = 1) +
      geom_point(size = 3) +
      labs(
        title = paste0(scale_name, " Scale - ", str_to_title(gender)),
        x = "Birth Cohort",
        y = "Mean Sum Score",
        color = "Respondent",
        linetype = "Age"
      ) +
      theme_classic() +
      theme(
        plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
        plot.title = element_text(hjust = 0.5, size = 23, family = "Arial"),
        axis.title = element_text(size = 21, family = "Arial"),
        axis.text = element_text(size = 18, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 18, family = "Arial"),
        legend.position = "bottom",  # legend to the bottom
        legend.box = "horizontal",  # legend items horizontally
        panel.grid.major = element_line(color = "gray90", size = 0.2),  # light grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        strip.background = element_blank(),  # Remove background for faceted plots (if used)
        strip.text = element_text(size = 15, family = "Arial")  # Customize facet labels
      ) +
      scale_color_manual(values = c("m" = "#1f77b4", "f" = "#ff7f0e", "t" = "#2ca02c")) +  # Custom colors
      scale_linetype_manual(values = c("7" = "solid", "10" = "dashed", "12" = "dotted")) +  # Custom line types
      scale_x_discrete(breaks = unique(data_filtered$yob_bin_plot)) # Treat x-axis as categorical and show all unique bins
    
    plot_name <- paste0(scale_type, "_", gender)
    plot_list[[plot_name]] <- plot_trend # Store the plot in the list
    
    
  } # sex loop
} # scale loop


final_plot <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)


plot_path <- file.path(output_dir, "sumscore_trend.png")


ggsave(plot_path, plot = final_plot, 
       width = 1920 / 100, height = 1080 / 100, dpi = 300, units = "in", bg = "white")


write.xlsx(descriptive_stats, file = paste0(output_dir,"/sumscore_trend_desc.xlsx"))
write.xlsx(descriptive_stats_plotting, file = paste0(output_dir,"/sumscore_trend_desc_plotting.xlsx"))







































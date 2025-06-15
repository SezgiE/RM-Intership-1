## Sezgi Ercan - 20.06.2025
## Script 2 for item level analysis

##------------------------- Plotting Mean Item Scores ------------------------##

# Necessary Libraries
library(gridExtra)
library(tidyverse)
library(openxlsx)
library(stringr)
library(psych)

rm(list = ls(all=TRUE)) # clean memory


# This script takes 12 .RDS files from first script as input.
# Please set the working directory to where the above-mentioned files are.
# There should be no other .RDS files than the 12 item level data files within the directory
setwd("/Users/sezgi/Desktop/scripts/data/item_level_data")
file_list <- list.files(pattern = "*.rds") # Obtaining 12 .RDS files

# Please indicate your desired output directory. Outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/trend_item/trend_plot"



#
#----------------------------------Code Starts---------------------------------#
#


plot_list_trend <- list()
trend_desc <- data.frame()
trend_desc_plotting <- trend_desc <- data.frame()

#for loop
# Loop through each item
for (file in file_list) {
  
  ## Read the dataset
  df <- readRDS(file)
  q_name <- sub("^(q\\d+).*", "\\1", file)
  
  ## Remove NAs if there is
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
  
  ## for plotting
  plotting_data <- data %>%
    group_by(sex, yob_bin, respondent, assigned_age) %>%
    mutate(yob_bin_plot = ifelse(n() < 80, "2004-2009", as.character(yob_bin))) %>%
    ungroup()
  
  
  genders <- c("male", "female")
  
  for (gender in genders){
    
    ## Descriptive statistics by bin, rater, and age
    descriptive_binned <- data %>%
      filter(sex == gender) %>%
      group_by(yob_bin, respondent, assigned_age) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),  # Mean rating
        sd_response = sd(response, na.rm = TRUE),     # Standard deviation
        n = n(), # Number of observations
        sem_response = sd_response / sqrt(n), # sem
        .groups = 'drop'
      )
    
    descriptive_binned <- descriptive_binned %>%
      mutate(q_name = q_name) %>%
      mutate(sex = gender)
    
    # descriptives for plotting
    descriptive_stats_plotting <- plotting_data %>%
      filter(sex == gender) %>%
      group_by(yob_bin_plot, respondent, assigned_age) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),  # Mean rating
        sd_response = sd(response, na.rm = TRUE),     # Standard deviation
        n = n(), # Number of observations
        sem_response = sd_response / sqrt(n), # sem
        .groups = 'drop'
      )
    
    descriptive_stats_plotting <- descriptive_stats_plotting %>%
      mutate(q_name = q_name) %>%
      mutate(sex = gender) %>%
      mutate(yob_bin_plot = as.factor(yob_bin_plot))
    
    
    # y_axis limits
    y_max <- round(max(descriptive_stats_plotting$mean_response, na.rm = TRUE), 2) + 0.05
    y_min <- round(min(descriptive_stats_plotting$mean_response, na.rm = TRUE), 2) - 0.05
    y_break <- round((y_max - y_min)/3, 2)
    
    
    ## plotting the trend
    plot_trend <- ggplot(descriptive_stats_plotting, aes(x = as.factor(yob_bin_plot), y = mean_response, color = respondent)) +
      geom_line(aes(linetype = as.factor(assigned_age), group = interaction(respondent, assigned_age)), size = 1) +
      geom_point(size = 3) +
      labs(
        title = paste0(toupper(q_name), "- ", paste0(toupper(substring(gender, 1, 1)),
                                                     tolower(substring(gender, 2)))),
        x = "Birth Cohort",
        y = "Mean Response",
        color = "Respondent",
        linetype = "Age"
      ) +
      theme_classic() +
      theme(
        plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
        plot.title = element_text(hjust = 0.5, size = 21, family = "Arial"),
        axis.title = element_text(size = 21, family = "Arial"),
        axis.text = element_text(size = 18, family = "Arial"),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 18, family = "Arial"),
        legend.position = "bottom",  # legend to the bottom
        legend.box = "horizontal",  # legend items horizontally
        panel.grid.major = element_line(color = "gray90", size = 0.2),  # light grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        strip.background = element_blank(),  # Remove background for faceted plots (if used)
        strip.text = element_text(size = 15, family = "Arial"),
        axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.65)
      ) +
      scale_color_manual(values = c("m" = "#1f77b4", "f" = "#ff7f0e", "t" = "#2ca02c")) +  
      scale_linetype_manual(values = c("7" = "solid", "10" = "dashed", "12" = "dotted")) +
      scale_x_discrete(breaks = unique(descriptive_stats_plotting$yob_bin_plot)) +
      scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = y_break))
    
    plot_trend
      
    plot_name <- paste0(gender, q_name)
    plot_list_trend[[plot_name]] <- plot_trend # Store the plot in the list
    
    
    # Putting into trend_desc df
    trend_desc <- rbind(trend_desc, descriptive_binned)
    trend_desc_plotting <- rbind(trend_desc_plotting, descriptive_stats_plotting)
    
    
  } # Gender loop for trend plot
  
  cat("Processed:", q_name, "\n")
  
} # item loop


## Saving Trend plots
# For males
male_plots <- grep("^ma", names(plot_list_trend), value = TRUE)
male_plot_list <- plot_list_trend[male_plots]

plot_numbers <- as.numeric(gsub("^maleq(\\d+)$", "\\1", names(male_plot_list)))
plot_order <- data.frame(plot_name = names(male_plot_list), number = plot_numbers)
ordered_names_m <- plot_order %>%
  arrange(number) %>%
  pull(plot_name)

ordered_male <- male_plot_list[ordered_names_m]
male_plot <- grid.arrange(grobs = ordered_male, ncol = 4, nrow = 3)

ggsave(paste0(output_dir,"/male_trend.png"), 
       plot = male_plot, width = (1920 / 100) * 1.5, height = (1080 / 100) * 1.5, dpi = 200, units = "in", bg = "white")


# For females
female_plots <- grep("^fe", names(plot_list_trend), value = TRUE)
female_plot_list <- plot_list_trend[female_plots]

plot_order <- data.frame(plot_name = names(female_plot_list), number = plot_numbers)
ordered_names_f <- plot_order %>%
  arrange(number) %>%
  pull(plot_name)

ordered_female <- female_plot_list[ordered_names_f]
female_plot <- grid.arrange(grobs = ordered_female, ncol = 4, nrow = 3)

ggsave(paste0(output_dir,"/female_trend.png"), 
       plot = female_plot, width = (1920 / 100)*1.5, height = (1080 / 100)*1.5, dpi = 200, units = "in", bg = "white")


write.xlsx(trend_desc, file = paste0(output_dir,"/trend_desc.xlsx"))
write.xlsx(trend_desc_plotting, file = paste0(output_dir,"/trend_desc_plotting.xlsx"))



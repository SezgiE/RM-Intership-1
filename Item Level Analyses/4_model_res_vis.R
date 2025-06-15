## Sezgi Ercan - 20.06.2025
## Script 4 for item level analysis

##----------------------- OpenMx Model - Result Plotting ----------------------##

library(tidyverse)
library(readxl)
library(gridExtra)
library(grid)


rm(list = ls(all=TRUE)) # clean memory

# This script takes OpenMx result files from third script as input.
# Please set the working directory to where the above-mentioned files are.
setwd("/Users/sezgi/Desktop/scripts/trend_item/model_results")

# Please indicate your desired output directory. Plots will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/trend_item/model_results"

data <- data.frame(read_excel("openmx_results.xlsx"))


#
#----------------------------------Code Starts---------------------------------#
#


alpha_threshold <- round(0.05/36, 5)

# selecting necessary variables
data <- data  %>%
  filter(comparison == "no_trend") %>%
  mutate(sig = ifelse(p < alpha_threshold, "Significant", "Non-significant")) %>%
  dplyr::select(q_name, rater, age, p, sig)


# Data and label adjustments 
data$age <- factor(data$age, levels=c("7", "10", "12"))
data$respondent <- gsub('m\\b', 'Mother', data$rater)
data$respondent <- gsub('f\\b', 'Father', data$respondent)
data$respondent <- gsub("t\\b", 'Teacher', data$respondent)
data$question_number <- as.numeric(str_extract(data$q_name, "\\d+"))


# ranking function
rank_p_centered <- function(p_values) {
  ranked <- rank(p_values)
  
  middle_rank <- median(ranked)
  
  centered_ranks <- ranked - middle_rank
  
  return(centered_ranks)
}


data <- data %>%
  group_by(rater, age) %>%
  mutate(p_rank = rank_p_centered(p),
         q_rank = rank_p_centered(question_number)) %>%
  ungroup()


my_plot <- data %>%
  ggplot(aes(age, q_rank)) +
  geom_point(aes(color = sig), size = 25) +
  geom_text(aes(label = q_name), size = 7.5) +
  facet_wrap(~respondent, strip.position = "bottom") +
  scale_x_discrete(expand = c(.3, .3)) +
  theme_minimal()  +
  scale_color_manual(values = c("darkturquoise", "coral1")) +
  theme(
    panel.grid.major.x = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0, "cm"),
    legend.position = "bottom",
    strip.text = element_text(size = 25, face = "bold"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    plot.title = element_text(size = 25, face = "bold", margin = margin(b = 40, t = 40))) +
  labs(color = paste0("p < ", alpha_threshold), 
       x = "", 
       y = "CBCL/6-18 Items") +
  guides(color = guide_legend(override.aes = list(size = 5)))


my_plot

my_plot_path <- file.path(output_dir, "model_res_plot.png")

ggsave(my_plot_path, my_plot, width = (1920 / 100), height = (1280 / 100), dpi = 300, units = "in", bg = "white")


# Item level trend predicted by the model

trend_df <- data.frame(read_excel("trend_plot_df.xlsx"))

trend_df <- trend_df %>%
  mutate(instensity = ifelse(significance, 1, 0.75)) %>%
  filter(significance == TRUE)

items <- unique(trend_df$q_name)

p_list <- list()

for (item in items){
  

  plot_df <- trend_df %>%
    filter(q_name == item) %>%
    mutate(rated_age = as.numeric(as.character(rated_age)))
  
  ## plotting the trend
  p <- ggplot(plot_df, aes(x = cohort, y = mean_liability, color = rater)) +
    geom_line(aes(linetype = as.factor(rated_age), group = interaction(rater, rated_age)), size = 1) +
    labs(
      title = toupper(item),
      x = "Birth Cohort",
      y = "Mean Liability",
      color = "Respondent",
      linetype = "Age"
    ) +
    theme_classic() +
    theme(
      plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
      plot.title = element_text(hjust = 0.5, size = 25, family = "Arial"),
      axis.title = element_text(size = 22, family = "Arial"),
      axis.text = element_text(size = 20, family = "Arial"),
      legend.title = element_text(size = 20, family = "Arial"),
      legend.text = element_text(size = 20, family = "Arial"),
      legend.position = "bottom",  # legend to the bottom
      legend.box = "horizontal",  # legend items horizontally
      panel.grid.major = element_line(color = "gray90", size = 0.2),  # light grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      strip.background = element_blank(),  # Remove background for faceted plots (if used)
      strip.text = element_text(size = 15, family = "Arial")  # Customize facet labels
    ) +
    scale_color_manual(values = c("m" = "#1f77b4", "f" = "#ff7f0e", "t" = "#2ca02c")) +  # Custom colors
    scale_linetype_manual(values = c("7" = "solid", "10" = "dashed", "12" = "dotted")) +
    scale_x_continuous(breaks = c(1990, 2000, 2010))
    guides(linetype = guide_legend(override.aes = list(color = "black")))

  
  p
  
  p_list[[item]] <- p

}

plot_names <- names(p_list)

p_list_sorted <- p_list[order(as.numeric(gsub("q", "", plot_names)))]

# Trends in Mean Liability Over 30 Years by Birth Cohort, Rater and Age
trend_plot <- grid.arrange(grobs = p_list_sorted, ncol = 4, nrow = 3)


plot_path <- file.path(output_dir, "model_trend_plot.png")

ggsave(plot_path, trend_plot, width = (1920 / 100) * 1.5, height = (1080 / 100) * 1.5, dpi = 200, units = "in", bg = "white")











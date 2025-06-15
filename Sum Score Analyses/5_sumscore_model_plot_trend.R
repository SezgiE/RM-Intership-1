## Sezgi Ercan - 20.06.2025
## Script 5 for sum score analysis

##-------------------- Plotting the Estimated Trend ----------------------##

# Necessary Libraries
library(tidyverse)
library(readxl)

rm(list = ls()) # clean memory


# This script takes OpenMx result files from fourth script as input.
# Please set the working directory to where the above-mentioned files are.
setwd("/Users/sezgi/Desktop/scripts/sum_score/trend_results/model_results")


# Please indicate your desired output directory. Outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/trend_results/model_results"


#
#-----------------------------Code Starts---------------------------#
#


trend_df <- data.frame(read_excel("trend_plot_df.xlsx"))

trend_df <- trend_df %>%
  filter(significance == TRUE)


scales <- c("emp", "dsm")
scale_names <- list("emp" = "AP", "dsm" = "ADHP")

p_list <- list()


for (scale_i in scales){
  
  scale_name <- scale_names[[scale_i]]
  
  plot_df <- trend_df %>%
    filter(scale == scale_i) %>%
    mutate(rated_age = as.numeric(as.character(rated_age)))
  
  ## plotting the trend
  p <- ggplot(plot_df, aes(x = cohort, y = mean_liability, color = rater)) +
    geom_line(aes(linetype = as.factor(rated_age), group = interaction(rater, rated_age)), size = 1) +
    labs(
      title = paste0(scale_name, " Scale"),
      x = "Birth Cohort",
      y = "Mean Liability",
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
    scale_linetype_manual(values = c("7" = "solid", "10" = "dashed", "12" = "dotted")) +
    scale_x_continuous(breaks = c(1990, 2000, 2010))
  guides(linetype = guide_legend(override.aes = list(color = "black")))
  
  
  p
  
  p_list[[scale_i]] <- p
  
}


trend_plot <- grid.arrange(grobs = p_list, ncol = 2, nrow = 1)


plot_path <- file.path(output_dir, "model_trend_plot.png")

ggsave(plot_path, trend_plot, width = (1920 / 200)*1.5, height = (980 / 200)*1.5, dpi = 200, units = "in", bg = "white")




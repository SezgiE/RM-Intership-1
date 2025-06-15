## Sezgi Ercan - 20.06.2025
## Script 2 for sum score analysis

##---------------------- Plotting Imputation Quality ----------------------##

# Necessary Libraries
library(gridExtra)
library(tidyverse)
library(ggpubr)
library(grid)

rm(list = ls()) # clean memory

# This script takes "sumscore_RawData.RDS" file from first script as input.
# Please set the working directory to where the above-mentioned file is.
setwd("/Users/sezgi/Desktop/scripts/sum_score/imputation_results")

# Please indicate your desired output directory. Outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/imputation_results"


#
#-----------------------------Code Starts---------------------------#
#


df <- readRDS("sumscore_RawData.RDS") # reading the dataset


raters <- c("m", "v", "t")

ages <- c("7", "10", "12")

scales <- c("emp", "dsm")

rater_names <- list("m" = "Mother", "v" = "Father", "t" = "Teacher")
scale_names <- list("emp" = "AP", "dsm" = "ADHP")


# Create a list to store the plots
plot_list <- list()


for (scale in scales){
  
  scale_name <- scale_names[[scale]]
  up_lim <- ifelse(scale == "dsm", 17, 23)
  cor_pos_x <- ifelse(scale == "dsm", 11, 16)
  cor_pos_y <- ifelse(scale == "dsm", 16.5, 22.5)
  
  for (rater in raters){
    for (rated_age in ages) {
      
      raw_col <- paste0(rater, rated_age, "_", scale, "_raw")
      imp_col <- paste0(rater, rated_age, "_", scale)
  
      rater_name <- rater_names[[rater]]
      
      
      # Create the scatter plot using ggplot2
      p <- ggplot(df, aes(x = .data[[raw_col]], y = .data[[imp_col]])) +
        geom_point(color = "black") +
        geom_smooth(method = "lm", se = FALSE, color = "red") + # Add a linear regression line in red
        stat_cor(method = "pearson", label.x = cor_pos_x, label.y = cor_pos_y, size = 4.5) +
        labs(
          x = "Raw Sum Scores",
          y = "Imputed Sum Scores",
          title = paste0(rater_name, "-Rated ", rated_age, "-year-old Subjects") # Title of the plot
        ) +
        theme_classic() +
        theme(
          plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
          plot.title = element_text(hjust = 0.5, size = 21, family = "Arial"),
          axis.title = element_text(size = 20, family = "Arial"),
          axis.text = element_text(size = 18, family = "Arial"),
          panel.grid.major.x = element_line(size = 0.5, color = "gray90"),
          panel.grid.major.y = element_line(size = 0.5, color = "gray90")
          ) + 
        scale_x_continuous(limits = c(0, up_lim)) +
        scale_y_continuous(limits = c(0, up_lim))
      
      
      plot_name <- paste0(rater, rated_age, "_", scale)
      plot_list[[plot_name]] <- p # Store the plot in the list
      
    }
  }
}


# Create a list for names ending with "_emp"
emp_plots <- plot_list[grep("_emp$", names(plot_list))]

# Create a list for names ending with "_dsm"
dsm_plots <- plot_list[grep("_dsm$", names(plot_list))]


# Arrange the plots in a 3x3 grid
final_plot_emp <- grid.arrange(grobs = emp_plots, ncol = 3, nrow = 3)
final_plot_dsm <- grid.arrange(grobs = dsm_plots, ncol = 3, nrow = 3)

plot_path_emp <- file.path(output_dir, paste0("emp", "_imp_plot.png"))
plot_path_dsm <- file.path(output_dir, paste0("dsm", "_imp_plot.png"))


# Saving the plots
ggsave(plot_path_emp, plot = final_plot_emp, 
       width = 1920 / 100, height = 1080 / 100, dpi = 300, units = "in", bg = "white")

ggsave(plot_path_dsm, plot = final_plot_dsm, 
       width = 1920 / 100, height = 1080 / 100, dpi = 300, units = "in", bg = "white")


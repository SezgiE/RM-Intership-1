## Sezgi Ercan - 20.06.2025
## Script 6 for sum score analysis

##-------------------- Sum score Response Frequencies ----------------------##

# Necessary Libraries
library(tidyverse)
library(readxl)
library(gridExtra)
library(grid)


rm(list = ls(all=TRUE)) # clean memory

# This script takes OpenMx result files from fourth script as input.
# Please set the working directory to where the above-mentioned files are.
setwd("/Users/sezgi/Desktop/scripts/sum_score/trend_results/model_results")


# Please indicate your desired output directory. Outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/trend_results/model_results/res_freq"


#
#-----------------------------Code Starts---------------------------#
#


trend_df <- data.frame(read_excel("trend_plot_df.xlsx"))

trend_df <- trend_df %>%
  filter(significance == TRUE) %>%
  filter(n > 5)


rater_names <- list("m" = "Mother", "f" = "Father", "t" = "Teacher")
scale_names <- list("emp" = "AP", "dsm" = "ADHP")

scales <- unique(trend_df$scale)


for (scale_i in scales){
  
  plot_list_freq <- list()
  
  scale_df <- trend_df %>%
    filter(scale == scale_i)
  
  
  raters <- unique(scale_df$rater)
  scale_name <- scale_names[[scale_i]]
  

  for (rater_i in raters){
    
    rater_df <- scale_df %>%
      filter(rater == rater_i)
    
    ages <- unique(rater_df$rated_age)
    
    for (age_i in ages){
      
      rater_name <- rater_names[[rater_i]]
      
      filtered_df <- rater_df %>%
        filter(rated_age == age_i)
      
      
      plot_df <- filtered_df %>%
        dplyr::select(cohort, obs_prob_0, obs_prob_1, obs_prob_2, est_prob_0, est_prob_0,
                      est_prob_1, est_prob_2, scale, rater, rated_age, significance) %>%
        pivot_longer(cols = starts_with(c("e","o")), 
                     names_to = "label", 
                     values_to = "probability")
      
      plot_df <- plot_df %>%
        mutate(type = str_extract(label, "est|obs")) %>%
        mutate(response = str_extract(label, "[012]"))
      
      plot_df$type <- gsub('est', 'estimated', plot_df$type)
      plot_df$type <- gsub('obs', 'observed', plot_df$type)
      
      
      
      ## Create 6-year bins for birth year
      plot_df <- plot_df %>%
        mutate(
          cohort_bin = cut(
            cohort,
            breaks = seq(min(cohort), max(cohort) + 2, by = 2),  # Define breaks for 6-year bins
            include.lowest = TRUE,
            right = FALSE,  # Use left-inclusive, right-exclusive bins
            labels = paste0(seq(min(cohort), max(cohort), by = 2), "-", seq(min(cohort) + 1, max(cohort) + 1, by = 2))  # Define labels
          ))
      
      plot_title <- paste0(toupper(scale_name), " - ", rater_name, "-rated ", 
                           age_i, "-year-old Twins")
        
        
      ## plotting the trend
      p <- ggplot(plot_df, aes(x = cohort, y = probability, color = response)) +
        geom_line(aes(linetype = as.factor(type), group = interaction(response, type)), size = 1) +
        labs(
          title = plot_title,
          x = "Birth Cohort",
          y = "Response Probability",
          color = "Response",
          linetype = "Type"
        ) +
        theme_classic() +
        theme(
          plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
          plot.title = element_text(hjust = 0.5, size = 25, family = "Arial"),
          axis.title = element_text(size = 24, family = "Arial"),
          axis.text = element_text(size = 23, family = "Arial"),
          legend.title = element_text(size = 24, family = "Arial"),
          legend.text = element_text(size = 23, family = "Arial"),
          legend.position = "bottom",  # legend to the bottom
          legend.box = "horizontal",  # legend items horizontally
          panel.grid.major = element_line(color = "gray90", size = 0.2),  # light grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          strip.background = element_blank(),  # Remove background for faceted plots (if used)
          strip.text = element_text(size = 15, family = "Arial")  # Customize facet labels
        ) +
        scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e", "2" = "#2ca02c")) +  # Custom colors
        scale_linetype_manual(values = c("observed" = "solid", "estimated" = "dotted"))
      
      plot_list_freq[[paste0(rater_i, age_i)]] <- p
      
    }
  }
  
  width_size <- case_when(
    length(plot_list_freq) > 6.1 ~ 1500,
    length(plot_list_freq) == 6  ~ 1000,
    length(plot_list_freq) == 5  ~ 1000,
    length(plot_list_freq) == 4  ~ 1000,
    length(plot_list_freq) == 3  ~ 500,
    length(plot_list_freq) == 1  ~ 500,
    TRUE                        ~ 1000 
  )


  height_size <- case_when(
    length(plot_list_freq) == 4  ~ 720,
    length(plot_list_freq) == 1 ~ 360,
    TRUE                        ~ 1080 
  )
  
  freq_plot <- grid.arrange(grobs = plot_list_freq)
                            #ncol = length(raters), nrow = length(ages),)
  
  freq_path <- paste0(output_dir, "/", scale_i, "_res_freq.png")
  
  ggsave(freq_path, plot = freq_plot, 
         width = (width_size / 80)*1.5, 
         height = (height_size / 80)*1.5, 
         dpi = 200, units = "in", bg = "white")
  
}

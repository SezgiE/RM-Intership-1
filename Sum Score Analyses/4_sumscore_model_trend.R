## Sezgi Ercan - 20.06.2025
## Script 4 for sum score analysis

##-------------------- Sum Score OpenMx Model for Trend ----------------------##

# Necessary Libraries
library(gridExtra)
library(tidyverse)
library(openxlsx)
library(OpenMx)
library(ggpubr)
library(scales)
library(psych)
library(MASS)
library(grid)

rm(list = ls()) # clean memory

# This script takes "sumscore_Data.RDS" file from first script as input. !!! Not "sumscore_RawData.RDS" !!!!
# Please set the working directory to where the above-mentioned file is.
setwd("/Users/sezgi/Desktop/scripts/sum_score/imputation_results")


# Please indicate your desired output directory. Outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/trend_results/model_results"


#
#-----------------------------Code Starts---------------------------#
#


# Quantile to ordinal function
quantile_to_ordinal <- function(x) {
  
  # Compute default quantiles
  probs <- c(0, 1/3, 2/3, 1)
  q <- quantile(x, probs = probs, na.rm = TRUE)
  
  # Check if breaks are unique
  if (length(unique(q)) < length(q)) {
    # Calculate zero_threshold
    zero_threshold <- sum(x == 0, na.rm = TRUE) / sum(!is.na(x))
    
    # Update probs with zero_threshold at position 2
    probs[2] <- zero_threshold
    
    # Recalculate quantiles
    q <- quantile(x, probs = probs, na.rm = TRUE)
  }
  
  # Return cut result as numeric
  as.numeric(as.character(cut(x,
                              breaks = q,
                              labels = c(0, 1, 2),
                              include.lowest = TRUE)))
}


df <- readRDS("sumscore_Data.rds")
df <- df %>%
  mutate(sex_chr = sex,
         sex = ifelse(sex == "male", 0, 1))


df <- df %>%
  group_by(yob, respondent, age) %>%
  mutate(no_data = ifelse(all(is.na(score)), TRUE, FALSE)) %>%
  ungroup()

df <- df %>%
  filter(no_data == FALSE)


# loop specs
raters <- c("m", "f", "t")

ages <- c("7", "10", "12")

scales <- c("emp", "dsm")

sig_thresh <- 0.05/6


rater_names <- list("m" = "Mother", "f" = "Father", "t" = "Teacher")
scale_names <- list("emp" = "AP", "dsm" = "ADHP")


# Dataframes and plot list for the results
trend_plot_data <- data.frame()
openmx_results <- data.frame()
hist_list <- list()
hist_list_ord <- list()
ordinal_info <- data.frame()

for (scale_i in scales){

  scale_name <- scale_names[[scale_i]]

  for (rater in raters) {
      
    rater_name <- rater_names[[rater]]
      
    for (rated_age in ages) {
      
      # filter data
      data <- df %>%
        filter(scale == scale_i) %>%
        filter(respondent == rater) %>%
        filter(age == rated_age)
      
      # remove non-paired subjects
      data <- data %>%
        group_by(twin_id) %>%
        filter(n() == 2) %>%
        ungroup()
      
      
      hist_p <- ggplot(data, aes(x = score)) +
        geom_histogram(colour = "black", fill = "#1f77b4", binwidth = 1) +
        labs(
          x = "Sum Score",
          y = "Subjects (in Thousands)",
          title = paste0(rater_name, "-Rated ", rated_age, "-years-old Subjects ")
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
        scale_y_continuous(
          labels = scales::label_number(scale = 1/1000, suffix = "K"))
      
      plot_name <- paste0(scale_i, "_", rater, rated_age)
      hist_list[[plot_name]] <- hist_p # Store the plot in the list
      
        
      ## Converting to wide format
      # Separate shared and twin-specific variables
      shared_vars <- data %>%
        group_by(twin_id) %>%
        slice(1) %>%  # Take one row per twin_id
        ungroup() %>%
        dplyr::select(twin_id, zyg, yob, assigned_age)
      
      
      # Reshape twin-specific variables
      twin_specific <- data %>%
        dplyr::select(twin_id, birthorder, sex, score) %>%
        pivot_wider(
          names_from = birthorder,
          values_from = c(sex, score),
          names_sep = "_twin"
        )
      
      # Merge the shared and twin-specific parts
      data_wide <- left_join(shared_vars, twin_specific, by = "twin_id") 
      
      data_wide <- as.data.frame(data_wide)
      
      
      names(data_wide) <- c("twin_id", "zyg", "cohort", "assigned_age", "sex1", "sex2",
                            "cph1", "cph2") 
      
      min_coh <- min(data_wide$cohort)
      
      # MZ and DZ datasets  
      mz_data <- data_wide %>%
        filter(zyg == "MZ")
      dz_data <- data_wide %>%
        filter(zyg == "DZ")
      
      
      ## Continous to ordinal scale (0,1,2)
      # Cutting data
      mz_data$oph1 <- quantile_to_ordinal(mz_data$cph1)
      
      mz_data$oph2 <- quantile_to_ordinal(mz_data$cph2)
      
      dz_data$oph1 <- quantile_to_ordinal(dz_data$cph1)
      
      dz_data$oph2 <- quantile_to_ordinal(dz_data$cph2)
      
      
      quantiles <- data.frame("scale" = scale_i,
                              "rater" = rater,
                              "age" = rated_age,
                              "mz1" = table(mz_data$oph1),
                              "mz2" = table(mz_data$oph2),
                              "dz1" = table(dz_data$oph1),
                              "dz2" = table(dz_data$oph2))
      
      
      ordinal_info <- rbind(ordinal_info, quantiles)
      
      
      # Ordinal Distribution Plot
      ord_hist_df <- data.frame(c(mz_data$oph1, mz_data$oph2, dz_data$oph1, dz_data$oph2))
      names(ord_hist_df) <- c("sumscores_ordinal")
      ord_hist_df <- na.omit(ord_hist_df)
      
      hist_p_ord <- ggplot(ord_hist_df, aes(x = as.factor(sumscores_ordinal))) +
        geom_bar(colour = "black", fill = "#1f77b4") +
        labs(
          x = "Ordinal Sum Scores",
          y = "Number of Subjects",
          title = paste0(rater_name, "-Rated ", rated_age, "-years-old Subjects ")
        ) +
        theme_classic() +
        theme(
          plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
          plot.title = element_text(hjust = 0.5, size = 18, family = "Arial"),
          axis.title = element_text(size = 18, family = "Arial"),
          axis.text = element_text(size = 15, family = "Arial"),
          panel.grid.major.x = element_line(size = 0.5, color = "gray90"),
          panel.grid.major.y = element_line(size = 0.5, color = "gray90")
        )
      
      hist_p_ord
      hist_list_ord[[plot_name]] <- hist_p_ord # Store the plot in the list
      
      
      # orthogonal polynomials
      mz_data <- mz_data %>%
        mutate(cohort = cohort - min(cohort))
      
      dz_data <- dz_data %>%
        mutate(cohort = cohort - min(cohort))
      
      
      ddatmz <- mz_data %>%
        dplyr::select(c(oph1, oph2, cohort, sex1, sex2))
      
      ddatdz <- dz_data %>%
        dplyr::select(c(oph1, oph2, cohort, sex1, sex2))
      
      
      ##---------------------------------OpenMx-------------------------------------##
      mx_intervals = FALSE # TRUE if you want to have conf_intervarls in model results
      
      
      ## starting values
      # regression models for obtaining starting values
      
      mz1_coef <- summary(polr(as.factor(oph1) ~ cohort + sex1, data = ddatmz, Hess = TRUE))$coeff
      mz2_coef <- summary(polr(as.factor(oph2) ~ cohort + sex2, data = ddatmz, Hess = TRUE))$coeff
      
      dz1_coef <- summary(polr(as.factor(oph1) ~ cohort + sex1, data = ddatdz, Hess = TRUE))$coeff
      dz2_coef <- summary(polr(as.factor(oph2) ~ cohort + sex2, data = ddatdz, Hess = TRUE))$coeff
      
      
      # main effect of polynomial cohort on ph
      svpcoh1 <- round((mz1_coef[1] + mz2_coef[1] + dz1_coef[1] + dz2_coef[1]) / 4, 3)

      
      # main effect of sex on ph
      svb1_sex <- round((mz1_coef[4] + mz2_coef[4] + dz1_coef[4] + dz2_coef[4]) / 4, 3)
      
      
      # starting values for thresholds
      qmz = polychoric(ddatmz[, c("oph1", "oph2")])$tau  # estimated thresholds
      qdz = polychoric(ddatdz[, c("oph1", "oph2")])$tau  # estimated thresholds
      
      q2 = t(qmz +qdz)/2 # mz dz average
      thr1 = round(mean(q2[1,1:2]),2) # twin 1 twin 2 average
      thr2 = round(mean(q2[2,1:2]),2) #
      svthr = matrix(c(thr1,thr1,thr2,thr2),2,2,byrow=T) # 2x2 matrix for threshold starting values
      
      # Starting value for covariance matrix
      mz_cor <- polychoric(ddatmz[, c("oph1", "oph2")])$rho[1,2]
      dz_cor <- polychoric(ddatdz[, c("oph1", "oph2")])$rho[1,2]
      
      
      #-------------------------------Model Starts-----------------------------------#
      selVars=c('oph1', 'oph2')
      #
      # SAT Model
      # Data objects for Multiple Groups .. define as ordinal
      #
      ddatmz$oph1 <- mxFactor(ddatmz$oph1, levels=c(0:2) )
      ddatmz$oph2 <- mxFactor(ddatmz$oph2, levels=c(0:2) )
      ddatdz$oph1 <- mxFactor(ddatdz$oph1, levels=c(0:2) )
      ddatdz$oph2 <- mxFactor(ddatdz$oph2, levels=c(0:2) )
      #
      dataMZ   	<- mxData( observed=ddatmz, type="raw" )
      dataDZ   	<- mxData( observed=ddatdz, type="raw" )  
        
      #
      covMZ     	<- mxMatrix(type='Symm',nrow=2, ncol=2, free=T, values=c(1,mz_cor,1), labels=c('vphmz','cmz','vphmz'), name='SMZ')
      covDZ    	<- mxMatrix(type='Symm',nrow=2, ncol=2, free=T,  values=c(1,dz_cor,1), labels=c('vphdz','cdz','vphdz'), name='SDZ') 
      
      
      # Matrix & Algebra for expected means vector and expected thresholds
      b0    	<- mxMatrix( type="Full", nrow=1, ncol=2, labels=c('b0','b0'),free=TRUE, name="B0" )
      threshold 	<- mxMatrix( type="Full", nrow=2, ncol=2, free=FALSE, 
                              values = svthr, name="threshold" )
      #
      defct    	<- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, 
                             labels=c("data.cohort","data.cohort"), name="Coh" )

      defsex    	<- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, 
                              labels=c("data.sex1","data.sex2"), name="Sex" )
      #
      Bsex     	<- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE, 
                             values= svb1_sex, label="bsex", name="Bsex" )
      
      
      Bcoh     	<- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE, 
                             values=svpcoh1, label="bcoh", name="Bcoh" )
      
      # 
      expm <- mxAlgebra(expression = B0 + Coh*Bcoh + Sex*Bsex, name='Mean') 
      
      
      #
      # Create Algebra for expected Threshold Matrices
      #
      # Algebra to compute total variances and standard deviations (diagonal only)
      #
      # Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins
      covMZ_    	<- mxAlgebra( expression= SMZ, name="expCovMZ" )
      covDZ_    	<- mxAlgebra( expression= SDZ, name="expCovDZ" )
      #
      ##
      # Expectation objects for Multiple Groups
      expMZ     	<- 	mxExpectationNormal( covariance="expCovMZ", means="Mean", dimnames=selVars,
                                          thresholds="threshold" )
      expDZ     	<- 	mxExpectationNormal( covariance="expCovDZ", means="Mean", dimnames=selVars,
                                          thresholds="threshold" )
      #
      #
      bits 		<-c(expm, b0,defct, defsex, Bsex, Bcoh)
      funML     	<- 	mxFitFunctionML()
      #
      modelMZ  	<- mxModel( bits,threshold, covMZ, covMZ_, dataMZ, funML, expMZ, name="MZ" )
      modelDZ  	<- mxModel( bits,threshold, covDZ, covDZ_, dataDZ, funML,expDZ, name="DZ" )
      #
      #
      #
      # Create Confidence Interval Objects
      #ciCov     	<- mxCI( c('MZ.rmz','DZ.rdz'))
      #
      #
      # Combine Groups
      multi     	<- mxFitFunctionMultigroup( c("MZ","DZ") )
      satModel 	<- mxModel( "SAT", modelMZ, modelDZ, funML, multi  )
      #
      # ---
      out1 = mxTryHard(satModel)

      
      # test of no trend
      no_trend <- mxModel(out1, name="no_trend" )
      no_trend <- omxSetParameters(no_trend, labels='bcoh', values=0, free=F)
      fit_no_trend <- mxTryHard(no_trend, intervals = mx_intervals)
      
      trend_test <- data.frame(mxCompare(out1, fit_no_trend))   # ns
      
      model_info <- data.frame("scale" = c("", scale_i),
                               "rater" = c("", rater),
                               "age" = c("", rated_age)
      )
      
      trend_test <- cbind(trend_test, model_info)
      
      ##--------------------------------End of OpenMx Model-------------------------##
      
      # Adding all the results in to a df
      openmx_results <- rbind(openmx_results, trend_test)
      
      
      # dataframe for plotting the trend
      out_paramters <- data.frame(out1$output$estimate)
      
      # obtaining outbput parameters
      b0 = out_paramters["b0",1]
      bsex = out_paramters["bsex",1]
      bcoh = out_paramters["bcoh",1]
      vphmz = out_paramters["vphmz",1]
      vphdz = out_paramters["vphdz",1]
      thresh_1 = svthr[1,1]
      thresh_2 = svthr[2,2]
      p_val = trend_test[2, "p"]
      
      significance <- ifelse(p_val < sig_thresh, TRUE, FALSE)
      
      # Creating years dataset for plotting the trend
      combined_zyg <- rbind(mz_data, dz_data)
      
      years_df <- data.frame(unique(combined_zyg$cohort))
      colnames(years_df) <- "cohort"
      years_df$cohort <- years_df[order(years_df$cohort), ]
      
      
      
      trend_plot_df <- years_df %>%
        mutate(
          beta_bc = bcoh,
          beta_0 = b0,
          beta_sex = bsex,
          mean_liability = (bcoh * cohort),
          
        ) %>%
        mutate(est_prob_0 = pnorm(thresh_1, mean = mean_liability, sd = sqrt((vphmz + vphdz)/2), lower.tail = T, log.p = FALSE)) %>%
        mutate(est_prob_2 = pnorm(thresh_2, mean = mean_liability, sd = sqrt((vphmz + vphdz)/2), lower.tail = F, log.p = FALSE)) %>%
        mutate(est_prob_1 = 1- (est_prob_0 + est_prob_2))
      
      # Observed probabilities-------------------------
      
      prob_lookUp <- combined_zyg %>%
        pivot_longer(cols = c("oph1", "oph2"), 
                     names_to = "birthorder", 
                     values_to = "ph") %>%
        filter(!is.na(ph))
      
      
      observed_probs <- prob_lookUp %>%
        group_by(cohort) %>%
        summarise(count_0 = sum(ph == 0, na.rm = T),
                  count_1 = sum(ph == 1, na.rm = T),
                  count_2 = sum(ph == 2, na.rm = T),
                  n = n()
        ) %>%
        ungroup() %>%
        mutate(obs_prob_0 = count_0/n,
               obs_prob_1 = count_1/n,
               obs_prob_2 = count_2/n)
      
      # Observed probabilities-------------------------
      
      trend_plot_df <- trend_plot_df %>%
        left_join(observed_probs, by = c("cohort"))
      
      
      trend_plot_df <- trend_plot_df %>%
        mutate(lower_thresh = thresh_1,
               upper_thresh = thresh_2,
               scale = scale_i,
               rater = rater,
               rated_age = rated_age,
               significance = significance) %>%
        dplyr::select(-count_0, -count_1, -count_2)
      
      trend_plot_df <- trend_plot_df %>%
        mutate(coded_cohort = cohort, 
          cohort = cohort + min_coh)
      
      
      trend_plot_data <- rbind(trend_plot_data, trend_plot_df)
      # printing the progress
      cat("Processed:", scale_i, rater, rated_age, "\n")
  
  
    } # age loop
  } # rater loop    
} # scale loop    
    

# Save excel results
file_path_plotDf <- file.path(output_dir, "trend_plot_df.xlsx")
write.xlsx(trend_plot_data, file = file_path_plotDf, rowNames = FALSE)

file_path_res <- file.path(output_dir, "openmx_results.xlsx")
write.xlsx(openmx_results, file = file_path_res, rowNames = FALSE)

file_path_res <- file.path(output_dir, "ordinal_info.xlsx")
write.xlsx(ordinal_info, file = file_path_res, rowNames = FALSE)


# Saving the histograms
emp_list <- hist_list[grepl("^emp", names(hist_list))]
dsm_list <- hist_list[grepl("^dsm", names(hist_list))]


emp_plot <- grid.arrange(grobs = emp_list, ncol = 3, nrow = 3)


plot_path_emp <- file.path(output_dir, paste0("emp", "_hist.png"))
ggsave(plot_path_emp, plot = emp_plot, width = 1920 / 100, height = 1440 / 100, dpi = 300, units = "in", bg = "white")


dsm_plot <- grid.arrange(grobs = dsm_list, ncol = 3, nrow = 3)


plot_path_dsm <- file.path(output_dir, paste0("dsm", "_hist.png"))
ggsave(plot_path_dsm, plot = dsm_plot, width = 1920 / 100, height = 1440 / 100, dpi = 300, units = "in", bg = "white")


# Ordinal histograms
# Saving the histograms
emp_list_ord <- hist_list_ord[grepl("^emp", names(hist_list_ord))]
dsm_list_ord <- hist_list_ord[grepl("^dsm", names(hist_list_ord))]


emp_plot_ord <- grid.arrange(grobs = emp_list_ord, ncol = 3, nrow = 3,
                         top = textGrob("Distribution of the Ordinal Sum Scores - AP Scale",
                                        gp = gpar(fontsize = 20)))


plot_path_emp_ord <- file.path(output_dir, paste0("emp_ord", "_hist.png"))
ggsave(plot_path_emp_ord, plot = emp_plot_ord, width = 1920 / 100, height = 1440 / 100, dpi = 300, units = "in", bg = "white")


dsm_plot_ord <- grid.arrange(grobs = dsm_list_ord, ncol = 3, nrow = 3,
                         top = textGrob("Distribution of the Ordinal Sum Scores - ADHP Scale",
                                        gp = gpar(fontsize = 20)))


plot_path_dsm_ord <- file.path(output_dir, paste0("dsm_ord", "_hist.png"))
ggsave(plot_path_dsm_ord, plot = dsm_plot_ord, width = 1920 / 100, height = 1440 / 100, dpi = 300, units = "in", bg = "white")





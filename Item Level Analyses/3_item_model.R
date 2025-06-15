## Sezgi Ercan - 20.06.2025
## Script 3 for item level analysis

##------------------------- OpenMx Model Fitting ------------------------##

library(tidyverse)
library(openxlsx)
library(OpenMx)
library(psych)
library(MASS)
library(polycor)


rm(list = ls(all=TRUE)) # clean memory

# This script takes 12 .RDS files from first script as input.
# Please set the working directory to where the above-mentioned files are.
# There should be no other .RDS files than the 12 item level data files within the directory
setwd("/Users/sezgi/Desktop/scripts/data/item_level_data")
file_list <- list.files(pattern = "*.rds") # Obtaining 12 .RDS files

# Please indicate your desired output directory. OpenMX outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/trend_item/model_results"


#
#----------------------------------Code Starts---------------------------------#
#


raters <- c("m", "f", "t")

ages <- c("7", "10", "12")

rater_names <- list("m" = "Mother", "f" = "Father", "t" = "Teacher")

sig_thresh <- round(0.05/36, 5)


# Dataframes for the results
trend_plot_data <- data.frame()
openmx_results <- data.frame()


for (file in file_list){
  
  df <- data.frame(readRDS(file))
  
  data <- df[complete.cases(df[, c("FID", "PID", "mult_id_fam", "sex", "yob", "zyg", "assigned_age")]), ]
  
  data <- data %>%
    dplyr::select(-PID, -age, -exact_age)
  data$sex <- ifelse(data$sex == "female", 1, 0)
  
  
  q_name <- sub("^(q\\d+).*", "\\1", file)
  
  for (rater in raters){
    for (rated_age in ages){
      
      
      raterAge <- paste0(rater, rated_age)
      
      rater_name <- rater_names[[rater]]
      
      data_q <- data %>%
        filter(respondent == rater) %>%
        filter(assigned_age == rated_age) %>%
        mutate(twin_id = paste0(FID, mult_id_fam))
      
      
      # remove if non-paired
      data_q <- data_q %>%
        group_by(twin_id) %>%
        filter(n() == 2) %>%
        filter(sum(is.na(response)) < 2) %>%
        ungroup()
      
      
      ## Converting to wide format
      # Separate shared and twin-specific variables
      shared_vars <- data_q %>%
        group_by(twin_id) %>%
        slice(1) %>%  # Take one row per twin_id
        ungroup() %>%
        dplyr::select(twin_id, zyg, yob, assigned_age)
      
      
      # Reshape twin-specific variables
      twin_specific <- data_q %>%
        dplyr::select(twin_id, birthorder, sex, response) %>%
        pivot_wider(
          names_from = birthorder,
          values_from = c(sex, response),
          names_sep = "_twin"
        )
      
      # Merge the shared and twin-specific parts
      data_wide <- left_join(shared_vars, twin_specific, by = "twin_id") 
      
      data_wide <- as.data.frame(data_wide)
      
      min_coh <- min(data_wide$yob)
      
      data_wide <- data_wide %>%
        mutate(yob = yob - min_coh)
      
      
      names(data_wide) <- c("twin_id", "zyg", "cohort", "assigned_age", "sex1", "sex2",
                            "ph1", "ph2")
      
      # Create mz and dz datasets
      mz_data <- data_wide %>%
        filter(zyg == "MZ") %>%
        dplyr::select(-twin_id)
      
      
      dz_data <- data_wide %>%
        filter(!zyg == "MZ") %>%
        dplyr::select(-twin_id)
      
      
      ddatmz <- mz_data %>%
        dplyr::select(c(ph1, ph2, cohort, sex1, sex2))
      
      ddatdz <- dz_data %>%
        dplyr::select(c(ph1, ph2, cohort, sex1, sex2))
      
      
      ##---------------------------------OpenMx-------------------------------------##
      mx_intervals = FALSE # TRUE if you want to have conf_intervarls in model results
      
      
      ## starting values
      # regression models for obtaining starting values
      
      mz1_coef <- summary(polr(as.factor(ph1) ~ cohort + sex1, data = ddatmz, Hess = TRUE))$coeff
      mz2_coef <- summary(polr(as.factor(ph2) ~ cohort + sex2, data = ddatmz, Hess = TRUE))$coeff
      
      dz1_coef <- summary(polr(as.factor(ph1) ~ cohort + sex1, data = ddatdz, Hess = TRUE))$coeff
      dz2_coef <- summary(polr(as.factor(ph2) ~ cohort + sex2, data = ddatdz, Hess = TRUE))$coeff
      
      
      # main effect of polynomial cohort on ph
      svpcoh1 <- round((mz1_coef[1] + mz2_coef[1] + dz1_coef[1] + dz2_coef[1]) / 4, 3)
      
      
      # main effect of sex on ph
      svb1_sex <- round((mz1_coef[4] + mz2_coef[4] + dz1_coef[4] + dz2_coef[4]) / 4, 3)
      
      
      # starting values for thresholds
      qmz = polychoric(ddatmz[, c("ph1", "ph2")])$tau  # estimated thresholds
      qdz = polychoric(ddatdz[, c("ph1", "ph2")])$tau  # estimated thresholds
      
      q2 = t(qmz +qdz)/2 # mz dz average
      thr1 = round(mean(q2[1,1:2]),2) # twin 1 twin 2 average
      thr2 = round(mean(q2[2,1:2]),2) #
      svthr = matrix(c(thr1,thr1,thr2,thr2),2,2,byrow=T) # 2x2 matrix for threshold starting values
      
      # Starting value for covariance matrix
      mz_cor <- polychoric(ddatmz[, c("ph1", "ph2")])$rho[1,2]
      dz_cor <- polychoric(ddatdz[, c("ph1", "ph2")])$rho[1,2]
      
      
      #-------------------------------Model Starts-----------------------------------#
      
      
      # 
      selVars=c('ph1', 'ph2')
      #
      # SAT Model
      # Data objects for Multiple Groups .. define as ordinal
      #
      ddatmz$ph1 <- mxFactor(ddatmz$ph1, levels=c(0:2) )
      ddatmz$ph2 <- mxFactor(ddatmz$ph2, levels=c(0:2) )
      ddatdz$ph1 <- mxFactor(ddatdz$ph1, levels=c(0:2) )
      ddatdz$ph2 <- mxFactor(ddatdz$ph2, levels=c(0:2) )
      #
      dataMZ   	<- mxData( observed=ddatmz, type="raw" )
      dataDZ   	<- mxData( observed=ddatdz, type="raw" )
      #
      covMZ     	<- mxMatrix(type='Symm',nrow=2, ncol=2, free=T, values=c(1, mz_cor, 1), labels=c('vphmz','cmz','vphmz'), name='SMZ')
      covDZ    	<- mxMatrix(type='Symm',nrow=2, ncol=2, free=T,  values=c(1, dz_cor, 1), labels=c('vphdz','cdz','vphdz'), name='SDZ') 
      #
      # Matrix & Algebra for expected means vector and expected thresholds
      b0    	<- mxMatrix( type="Full", nrow=1, ncol=2, labels=c('b0','b0'),free=TRUE, name="B0" )
      threshold 	<- mxMatrix( type="Full", nrow=2, ncol=2, free=FALSE, 
                              values=svthr, name="threshold" )
      #
      defct    	<- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, 
                             labels=c("data.cohort","data.cohort"), name="Coh" )
      defsex    	<- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, 
                              labels=c("data.sex1","data.sex2"), name="Sex" )
      #
      Bsex     	<- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE, 
                             values=svb1_sex, label="bsex", name="Bsex" )
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
      #
      out1$output
      
      # test of no trend
      no_trend <- mxModel(out1, name="no_trend" )
      no_trend <- omxSetParameters(no_trend, labels='bcoh', values=0, free=F)
      fit_no_trend <- mxTryHard(no_trend, intervals = mx_intervals)
      
      trend_test <- data.frame(mxCompare(out1, fit_no_trend))   # ns
      
      model_info <- data.frame("q_name" = c("", q_name),
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
      prob_lookUp <- data_wide %>%
        pivot_longer(cols = c("ph1", "ph2"), 
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
               upper_thresh = thresh_2) %>%
        mutate(q_name = q_name) %>%
        mutate(rater = rater) %>%
        mutate(rated_age = rated_age) %>%
        mutate(significance = significance) %>%
        dplyr::select(-count_0, -count_1, -count_2)
      
      trend_plot_df <- trend_plot_df %>%
        mutate(coded_cohort = cohort, 
               cohort = cohort + min_coh)
      
      
      trend_plot_data <- rbind(trend_plot_data, trend_plot_df)
      
      # printing the progress
      cat("Processed:", q_name, rater, rated_age, "\n")
      
    } # age loop
  } # rater loop
} # item loop


file_path_plotDf <- file.path(output_dir, "trend_plot_df.xlsx")
write.xlsx(trend_plot_data, file = file_path_plotDf, rowNames = FALSE)

file_path_res <- file.path(output_dir, "openmx_results.xlsx")
write.xlsx(openmx_results, file = file_path_res, rowNames = FALSE)



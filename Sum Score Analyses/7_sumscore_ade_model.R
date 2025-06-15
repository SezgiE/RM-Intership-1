## Sezgi Ercan - 20.06.2025
## Script 7 for sum score analysis

##-------------------- Sum score ADE Model Fitting ----------------------##

# Necessary Libraries
library(tidyverse)
library(OpenMx)
library(MASS)
library(psych)
library(polycor)
library(gridExtra)
library(grid)
library(openxlsx)

rm(list = ls()) # clean memory


# This script takes "sumscore_Data.RDS" file from first script as input. !!! Not "sumscore_RawData.RDS" !!!!
# Please set the working directory to where the above-mentioned file is.
setwd("/Users/sezgi/Desktop/scripts/sum_score/imputation_results")


# Please indicate your desired output directory. ADE model outputs will be automatically saved there.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/trend_results/ade_model_results"

# Please indicate which moderation model results you would like to save. ADE or E
results_to_save <- "Full_ADE" # "Drop_AD" 

# results plotting are not automatic. Please see below for do it manually.

#
#-----------------------------Code Starts---------------------------#
#


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


scales <- c("emp", "dsm")
ages <- c(7,10,12)


results_df <- data.frame()
plot_list <- list()

scale_names <- list("emp" = "AP", "dsm" = "ADHP")


for (scale_i in scales){
  for (rated_age in ages){
    
    scale_name <- scale_names[[scale_i]]
    
    data <- df %>%
      filter(respondent == "m") %>%
      filter(age == rated_age) %>%
      filter(scale == scale_i)
    
    data <- data %>%
      group_by(twin_id) %>%
      filter(n() == 2) %>%
      ungroup()
    
    
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
      filter(zyg == "MZ") %>%
      mutate(cohort = cohort-min(cohort))
    
    dz_data <- data_wide %>%
      filter(zyg == "DZ") %>%
      mutate(cohort = cohort-min(cohort))
    
    
    ## Continous to ordinal scale (0,1,2)
    # Cutting data
    mz_data$oph1 <- quantile_to_ordinal(mz_data$cph1)
    
    mz_data$oph2 <- quantile_to_ordinal(mz_data$cph2)
    
    dz_data$oph1 <- quantile_to_ordinal(dz_data$cph1)
    
    dz_data$oph2 <- quantile_to_ordinal(dz_data$cph2)
    
    
    #---------------------------------OpenMx--------------------------------------##
    
    mx_intervals = TRUE # TRUE if you want to have conf_intervarls in model results
    selVars <- c("oph1", "oph2")
    
    
    modeltype="ADE"  # the model type is ADE or ACE. here: ADE
    nv=1 #
    ntv=nv*2 #
    
    
    ## starting values
    # regression models for obtaining starting values
    
    mz1_coef <- summary(polr(as.factor(oph1) ~ cohort + sex1, data = mz_data, Hess = TRUE))$coeff
    mz2_coef <- summary(polr(as.factor(oph2) ~ cohort + sex2, data = mz_data, Hess = TRUE))$coeff
    
    dz1_coef <- summary(polr(as.factor(oph1) ~ cohort + sex1, data = dz_data, Hess = TRUE))$coeff
    dz2_coef <- summary(polr(as.factor(oph2) ~ cohort + sex2, data = dz_data, Hess = TRUE))$coeff
    
    
    # main effect of polynomial cohort on ph
    svpcoh1 <- round((mz1_coef[1] + mz2_coef[1] + dz1_coef[1] + dz2_coef[1]) / 4, 3)
    
    
    # B0
    svb0 = (mean(mz_data$cph1, na.rm = T) + mean(mz_data$cph2, na.rm = T) +
              mean(dz_data$cph1, na.rm = T) + mean(dz_data$cph2, na.rm = T)) / 4
    
    
    # main effect of sex on ph
    svb1_sex <- round((mz1_coef[4] + mz2_coef[4] + dz1_coef[4] + dz2_coef[4]) / 4, 3)
    
    
    # correlations and confidence intervals
    mz_cor <- polychor(mz_data$oph1, mz_data$oph2, ML=TRUE, std.err=TRUE)
    se_mz <- sqrt(mz_cor$var[1, 1])
    
    comp_cases_mz <- sum(complete.cases(mz_data[, c("oph1", "oph2")]))
    
    lower_mz <- round(mz_cor$rho - 1.96 * se_mz, 2)
    upper_mz <- round(mz_cor$rho + 1.96 * se_mz,2)
    
    cat(nrow(mz_data), comp_cases_mz, round(mz_cor$rho, 2), paste0("(",lower_mz, "-", upper_mz, ")"))
    
    
    dz_cor <- polychor(dz_data$oph1, dz_data$oph2, ML=TRUE, std.err=TRUE)
    se_dz <- sqrt(dz_cor$var[1, 1])
    
    comp_cases_dz <- sum(complete.cases(dz_data[, c("oph1", "oph2")]))
    
    lower_dz <- round(dz_cor$rho - 1.96 * se_dz, 2)
    upper_dz <- round(dz_cor$rho + 1.96 * se_dz,2)
    
    cat(nrow(dz_data), comp_cases_dz, round(dz_cor$rho, 2), paste0("(",lower_dz, "-", upper_dz, ")"))
    
    
    # unstandardized (raw) variances
    rmz = polychoric(mz_data[, c("oph1", "oph2")])$rho[2]
    rdz = polychoric(dz_data[, c("oph1", "oph2")])$rho[2]
    
    svVA = (4*rdz - rmz) * 1
    svVD = (2*rmz - 4*rdz) * 1
    svVE = (1 - svVA - svVD) * 1
    
    
    # starting values for the path coefficients
    svsdA = sqrt(svVA)
    svsdD = sqrt(svVD)
    svsdE = sqrt(svVE)
    
    # starting values for thresholds
    qmz = polychoric(mz_data[, c("oph1", "oph2")])$tau  # estimated thresholds
    qdz = polychoric(dz_data[, c("oph1", "oph2")])$tau  # estimated thresholds
    
    q2 = t(qmz +qdz)/2 # mz dz average
    thr1 = round(mean(q2[1,1:2]),2) # twin 1 twin 2 average
    thr2 = round(mean(q2[2,1:2]),2) #
    thfix = matrix(c(thr1,thr1,thr2,thr2),2,2,byrow=T) # 2x2 matrix for threshold starting values
    
    # Starting value for covariance matrix
    mz_cor <- polychoric(mz_data[, c("oph1", "oph2")])$rho[1,2]
    dz_cor <- polychoric(dz_data[, c("oph1", "oph2")])$rho[1,2]
    
    
    # ------------------- Openmx model starts --------------------- ##
    
    
    # Data objects
    mz_data$oph1 <- mxFactor(mz_data$oph1, levels=c(0:2) )
    mz_data$oph2 <- mxFactor(mz_data$oph2, levels=c(0:2) )
    dz_data$oph1 <- mxFactor(dz_data$oph1, levels=c(0:2) )
    dz_data$oph2 <- mxFactor(dz_data$oph2, levels=c(0:2) )
    #
    #
    # Create regression model for expected Mean Matrices
    B0_     <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svb0, labels=c("b0","b0"), name="B0" )
    
    Bcoh     	<- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE,
                           values=svpcoh1, label="bcoh", name="Bcoh" )
    
    Bsex     	<- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE,
                           values=svb1_sex, label="bsex", name="Bsex" )
    
    
    ThreshFixed = mxMatrix( type = "Full", nrow=2, ncol=2, free=F, labels = c('t0','t1','t0','t1'),values=thfix, name='Th') # the fixed thresholds based on the data
    #
    #
    # define sex and cohort
    Sex <- mxMatrix(type="Full", nrow=1, ncol=ntv, free=FALSE, labels=c('data.sex1','data.sex2'), name="sex")
    #
    Cohort    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=FALSE,
                             labels=c("data.cohort","data.cohort"), name="coh" )
    
    #
    # define cohorts for moderation effect
    Coh1 <- mxMatrix(type="Full", nrow=1, ncol=1, free=FALSE, labels=c('data.cohort'),name="coh1")
    
    #
    ExpMean <- mxAlgebra(expression = B0 + coh*Bcoh + sex*Bsex, name='expMean')
    
    #
    #
    #ACDE model
    # Create Matrices for Variance Components
    pathA      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=svsdA, label="pA11", name="pthA" )
    #
    modA      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=0, label="mA11", name="mA" )
    
    #
    if (modeltype=="ACE") {
      pathC      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=svsdC, label="pC11", name="pthC" )
      pathD      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=FALSE, values=0, label="pD11", name="pthD" )
      #
      modC      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=0, label="mC11", name="mC" )
      modD      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=FALSE, values=0, label="mD11", name="mD" )
      #
    }
    #
    if (modeltype=="ADE") {
      pathC      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=FALSE, values=0, label="pC11", name="pthC" )
      pathD      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=svsdD, label="pD11", name="pthD" )
      modC      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=FALSE, values=0, label="mC11", name="mC" )
      modD      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=0, label="mD11", name="mD" )
    }
    #
    pathE      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=svsdE, label="pE11", name="pthE" )
    modE      <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=0, label="mE11", name="mE" )
    #
    # ACDE model
    # Create Matrices for Variance Components
    covE      <- mxAlgebra( expression = (pthE+mE%*%coh1)%*%(pthE+mE%*%coh1), name="VE" )
    covC      <- mxAlgebra( expression = (pthC+mC%*%coh1)%*%(pthC+mC%*%coh1), name="VC" )
    covD      <- mxAlgebra( expression = (pthD+mD%*%coh1)%*%(pthD+mD%*%coh1), name="VD" )
    covA      <- mxAlgebra( expression = (pthA+mA%*%coh1)%*%(pthA+mA%*%coh1), name="VA" )
    #ACDE model
    # Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
    covP      <- mxAlgebra( expression= VA+VC+VD+VE, name="V" )
    covMZ     <- mxAlgebra( expression= VA+VC+VD, name="cMZ" )
    covDZ     <- mxAlgebra( expression= 0.5%x%VA+VC+.25%x%VD, name="cDZ" )
    expCovMZ  <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
    expCovDZ  <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )
    
    #ACE model
    # Create Data Objects for Multiple Groups
    #ACE model
    dataMZ    <- mxData( observed=mz_data, type="raw" )
    dataDZ    <- mxData( observed=dz_data, type="raw" )
    #
    #
    # Create Expectation Objects for Multiple Groups
    expMZ     <- mxExpectationNormal( covariance="expCovMZ", means="expMean", threshold="Th", dimnames=selVars,threshnames=selVars )
    expDZ     <- mxExpectationNormal( covariance="expCovDZ", means="expMean", threshold="Th", dimnames=selVars,threshnames=selVars )
    funML     <- mxFitFunctionML()
    #ACDE model
    # Create Model Objects for Multiple Groups
    pars      <- list(B0_, Bcoh, Bsex, ThreshFixed, pathA, pathC, pathD, pathE,
                      modA, modD, modC, modE)
    
    modelMZ   <- mxModel( pars, covA, covC, covD, covE, covP, ExpMean, Sex, Cohort, Coh1, covMZ, expCovMZ, ExpMean, dataMZ, expMZ, funML, name="MZ" )
    modelDZ   <- mxModel( pars, covA, covC, covD, covE, covP, ExpMean, Sex, Cohort, Coh1, covDZ, expCovDZ, ExpMean, dataDZ, expDZ, funML, name="DZ" )
    multi     <- mxFitFunctionMultigroup( c("MZ","DZ") )
    # ACDE model
    # Create Confidence Interval Objects
    ciACDE     <- mxCI(c("mA","mD","mE"))
    # ACDE model
    # Build Model with Confidence Intervals
    omodelACDEmod  <- mxModel(name=modeltype, pars, modelMZ, modelDZ, multi, ciACDE )
    #ACDE model
    # Run ACDE Model
    ofitACDEmod2    <- mxTryHard( omodelACDEmod, 20, intervals = T)
    osumACDEmod2    <- summary( ofitACDEmod2 )
    
    # omnibus test
    om_test=omxSetParameters(ofitACDEmod2, labels=c("mA11", "mD11", "mE11"), value=c(0,0,0), free=FALSE)
    om_test_out    <- mxTryHard( om_test, intervals=F)
    mxCompare(ofitACDEmod2, om_test_out)
    
    # # test A mod
    # otestA=omxSetParameters(ofitACDEmod2, labels=c('mA11'), value=0, free=FALSE)
    # otestA_out    <- mxTryHard( otestA, intervals=F)
    # mxCompare(ofitACDEmod2, otestA_out)
    # 
    # # test D mod
    # otestD=omxSetParameters(ofitACDEmod2, labels=c('mD11'), value=0, free=FALSE)
    # otestD_out    <- mxTryHard( otestD, intervals=F)
    # mxCompare(ofitACDEmod2, otestD_out)
    # 
    # # test E mod
    # otestE=omxSetParameters(ofitACDEmod2, labels=c('mE11'), value=0, free=FALSE)
    # otestE_out    <- mxTryHard( otestE, intervals=F)
    # mxCompare(ofitACDEmod2, otestE_out)
    # 
    # A and D dropped
    otestAD=omxSetParameters(ofitACDEmod2, labels=c("mA11", "mD11"), value=c(0,0), free=FALSE)
    otestAD_out    <- mxTryHard( otestAD, intervals=F)
    mxCompare(ofitACDEmod2, otestAD_out)
    # 
    # # D and E dropped
    # otestAD=omxSetParameters(ofitACDEmod2, labels=c("mA11", "mD11"), value=c(0,0), free=FALSE)
    # otestAD_out    <- mxTryHard( otestAD, intervals=F)
    # mxCompare(ofitACDEmod2, otestAD_out)
    
    
    
    # dataframe for plotting the trend
    if (results_to_save == "Full_ADE"){
      
      out_paramters <- data.frame(ofitACDEmod2$output$estimate)
      model_name <- "Full_ADE"
      
    } else if (results_to_save == "Drop_AD"){
      
      out_paramters <- data.frame(otestAD_out$output$estimate)
      model_name <- "Drop_AD"
    }
    
    #-----------------------------------------------------------------
    
    # obtaining outbput parameters
    b0 = out_paramters["b0",1]
    bsex = out_paramters["bsex",1]
    bcoh1 = out_paramters["bcoh1",1]
    bcoh2 = out_paramters["bcoh2",1]
    bcoh3 = out_paramters["bcoh3",1]
    path_A = out_paramters["pA11",1]
    path_D = out_paramters["pD11",1]
    path_E = out_paramters["pE11",1]
    
    m_A1 = ifelse(is.na(out_paramters["mA11",1]), 0, out_paramters["mA11",1])
    
    m_D1 = ifelse(is.na(out_paramters["mD11",1]), 0, out_paramters["mD11",1])
    
    m_E1 = ifelse(is.na(out_paramters["mE11",1]), 0, out_paramters["mE11",1])
    
    
    combined_zyg <- rbind(mz_data, dz_data)
    
    years_df <- data.frame(unique(combined_zyg$cohort))
    
    colnames(years_df) <- "years"
    years_df$years <- years_df[order(years_df$years), ]
    years_df$birth_cohort = years_df$years + min_coh
    
    years_df$birth_cohort_short <- sprintf("%02d", years_df$birth_cohort %% 100)
    
    
    var_A = (path_A + years_df$years*m_A1)^2
    var_D = (path_D + years_df$years*m_D1)^2
    var_E = (path_E + years_df$years*m_E1)^2
    total_var = var_A + var_D + var_E
    var_AD = var_A + var_D
    
    var_df <- cbind(years_df, var_A, var_D, var_E, var_AD, total_var)
    
    var_df <- var_df %>%
      pivot_longer(cols = c("var_A", "var_D", "var_E", "var_AD", "total_var"),
                   names_to = "variable",
                   values_to = "variance")
    
    
    # standardized variance components
    std_var_A = var_A / total_var
    std_var_D = var_D / total_var
    std_var_E = var_E / total_var
    std_total_var = 1
    
    std_df <- cbind(years_df, std_var_A, std_var_D, std_var_E, std_total_var)
    
    std_df <- std_df %>%
      pivot_longer(cols = c("std_var_A", "std_var_D", "std_var_E", "std_total_var"),
                   names_to = "variable",
                   values_to = "variance")
    
    #filter
    filtered_var_df <- var_df %>%
      filter(!variable == "var_D") %>%
      filter(!variable == "var_A")
    
    
    # plotting ADE
    p <- ggplot(filtered_var_df, aes(x = as.factor(birth_cohort), y = variance, color = variable, group = variable)) +
      geom_line(size = 1.2) +
      geom_point( size = 3) +
      labs(
        title = paste0(scale_name, " Scale - ", rated_age, "-year-old Twins"),
        x = "Birth Cohort",
        y = "Variance",
        color = "Variance",
      ) +
      theme_classic() +
      theme(
        plot.margin = unit(c(0.5, 0.25, 0, 0.25), "cm"),
        plot.title = element_text(hjust = 0.5, size = 25, family = "Arial"),
        axis.title = element_text(size = 25, family = "Arial"),
        axis.text = element_text(size = 19, family = "Arial"),
        legend.title = element_text(size = 24, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial"),
        legend.position = "bottom",  # legend to the bottom
        legend.box = "horizontal",  # legend items horizontally
        panel.grid.major = element_line(color = "gray90", size = 0.2),  # light grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        strip.background = element_blank(),  # Remove background for faceted plots (if used)
        strip.text = element_text(size = 15, family = "Arial"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.65),
        axis.title.x = element_text(hjust = 0.5, vjust = -1)
      ) +
      scale_color_manual(values = c("var_AD" = "#1f77b4", "var_E" = "#2ca02c", "total_var" = "darkred"),
                         labels = c("var_AD" = "A+D", "var_E" = "E", "total_var" = "Total Variance")) +  
      scale_x_discrete(breaks = unique(var_df$birth_cohort),
                       labels = sprintf("%02d", unique(var_df$birth_cohort) %% 100)) + #Treat x-axis as categorical and show all unique bins
      scale_y_continuous(
        limits = c(0, 1.5),              # Sets the minimum and maximum of the y-axis
        breaks = seq(0, 1.5, by = 0.25)  # Creates breaks at 0, 0.25, 0.50, ..., 1.50
      )
    
    p
    
    plot_name <- paste0(scale_i, "_", rated_age)
    plot_list[[plot_name]] <- p 
    
    # Correlation by birth cohorts
    mz_desc <- mz_data %>%
      group_by(cohort) %>%
      summarise(
        MZ_poly_cor = possibly(~ polychor(., oph2, ML=TRUE, std.err=TRUE)$rho, otherwise = NA)(oph1),
        n_MZ = n(),
        .groups = 'drop'
      )
    
    dz_desc <- dz_data %>%
      group_by(cohort) %>%
      summarise(
        Dz_poly_cor = possibly(~ polychor(., oph2, ML=TRUE, std.err=TRUE)$rho, otherwise = NA)(oph1),
        n_DZ = n(),
        .groups = 'drop'
      )
    
    B_A = paste0(round(m_A1, 4), " (", round(ofitACDEmod2$output$confidenceIntervals["mA11",1], 4), 
                 " - ", round(ofitACDEmod2$output$confidenceIntervals["mA11",3], 4), ")")
    B_D = paste0(round(m_D1, 4), " (", round(ofitACDEmod2$output$confidenceIntervals["mD11",1], 4), 
                 " - ", round(ofitACDEmod2$output$confidenceIntervals["mD11",3], 4), ")")
    B_E = paste0(round(m_E1, 4), " (", round(ofitACDEmod2$output$confidenceIntervals["mE11",1], 4), 
                 " - ", round(ofitACDEmod2$output$confidenceIntervals["mE11",3],4), ")")
    
    
    res_df <- data.frame(
      "Scale" = scale_i,
      "Age" = rated_age,
      "Model" = model_name,
      "Birth cohort" = years_df$years + min_coh,
      "cohort" = years_df$years,
      "B_A" = B_A,
      "B_D" = B_D,
      "B_E" = B_E,
      "var_phen" = total_var,
      "varA" = var_A,
      "varD" = var_D,
      "varE" = var_E,
      "std_varA" = std_var_A,
      "std_varD" = std_var_D,
      "std_varE" = std_var_E
    )
    
    res_df <- res_df %>%
      left_join(mz_desc, by = "cohort") %>%
      left_join(dz_desc, by = "cohort")
    
    results_df <- rbind(results_df, res_df)
    
    # Print progress
    cat("Processed:", scale_i, rated_age, "\n")
    
  } 
}
  
final_plot <- grid.arrange(grobs = plot_list, ncol = 3, nrow = 2,
                           top = textGrob("ADE Variance Components by Years",
                                          gp = gpar(fontsize = 25),
                                          y = unit(0.5, "npc") - unit(0, "line"))
                           )


##------------------------------ Plot Saving --------------------------##

print(plot_list) # plot order is given here

# Please put the order number of the plot you would like to print and change the saving name
plot_s <- plot_list[[5]]

plot_path <- file.path(output_dir, "adhp12_trend.png") # change


ggsave(plot_path, plot = plot_s, 
       width = 1520 / 150, height = 1080 / 150, dpi = 200, units = "in", bg = "white")

write.xlsx(results_df, file = paste0(output_dir, "/ade_results.xlsx"))
  

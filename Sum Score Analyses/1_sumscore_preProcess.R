## Sezgi Ercan - 20.06.2025
## Script 1 for sum score analysis

##---------------------- Preprocessing and Sum Score Calculation ----------------------##

## As imputation will be conducted, running this script may take a while

# Necessary Libraries
library(missMethods)
library(tidyverse)
library(viridis)
library(foreign)
library(stringr)
library(openxlsx)
library(psych)
library(mice)
library(VIM)

rm(list = ls(all=TRUE)) # clean memory


## Load the raw SPSS data file from NTR
df_raw <- read.spss("/Users/sezgi/Desktop/scripts/data/upload_20250303_LL/PHE_20250303_4758_LL.sav", 
                    to.data.frame = TRUE, use.value.labels = FALSE)

## Set the output directory for imputed sum scores
## Creating an empty file for the output directory is recommended.
output_dir <- "/Users/sezgi/Desktop/scripts/sum_score/imputation_results"

## Missingness information for each item will be plotted and saved the below directory.
## Please make sure this directory is an empty directory and different than the "output_dir".
output_miss <- "/Users/sezgi/Desktop/scripts/sum_score/imputation_results/missingness"


#
#----------------------------------Code Starts---------------------------------#
#


## empty df for missingness info
missing_info_scale <- data.frame(matrix(ncol= 8))


colnames(missing_info_scale) <- c("rater-Age", "total", "missing", "filtered", 
                                  "NA%", "female", "male", "scale")

mind_threshold <- 0.3


## Removing non-twin subjects
df_raw <- df_raw %>% filter(multiple_type == 2)


## Removing non-paired subjects
df_raw <- df_raw %>%
  group_by(FID, mult_id_fam) %>%  # Group by family and mult_id_fam
  mutate(
    complete_pair = n() > 1  # TRUE if there is more than one subject with the same FID and mult_id_fam
  ) %>%
  ungroup()

df_raw <- df_raw %>%
  filter(complete_pair)

## NA check
sum(is.na(df_raw$FID))
sum(is.na(df_raw$PID))
sum(is.na(df_raw$sex))
sum(is.na(df_raw$yob))
sum(is.na(df_raw$twzyg))


## removing individuals with missing zyg information
df_raw <- df_raw %>%
  filter(!is.na(twzyg))


## Imputing missing sex based on zygosity information
df <- df_raw %>%
  group_by(FID, mult_id_fam) %>%
  mutate(
    co_twin_sex = if_else(is.na(sex), sex[!is.na(sex)], NA_real_),
    inferred_sex = case_when(
      !is.na(sex) ~ as.numeric(sex),
      twzyg %in% c(1, 2) ~ 1,
      twzyg %in% c(3, 4) ~ 2,
      twzyg == 6 & row_number() == 1 ~ 2,
      twzyg == 6 & row_number() == 2 ~ 1,
      twzyg == 5 & row_number() == 1 ~ 1,
      twzyg == 5 & row_number() == 2 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(sex = if_else(is.na(sex), inferred_sex, as.numeric(sex))) %>%
  dplyr::select(-co_twin_sex, -inferred_sex)

## Creating a variable called "participant_id" by merging FID and PID
df$participant_id <- paste0(df$FID, df$PID)
df$twin_id <- paste0(df$FID, df$mult_id_fam)

## Recoding sex
df <- df %>%
  mutate(
    sex = case_when(
      sex == 1 ~ "male",
      sex == 2 ~ "female")
  ) 


# Birth order imputation randomly
# Set seed for reproducibility
set.seed(108)

sum(is.na(df$birthorder))

df <- df %>%
  group_by(twin_id) %>%
  mutate(
    birthorder = if (all(is.na(birthorder)) & n() == 2) {
      sample(c(1, 2))
    } else birthorder
  ) %>%
  ungroup()


##---------------------------------Loop Parameters-----------------------------#
## Questions, raters, ages
questions <- c("q1", "q10", "q104", "q13", "q17", "q4", "q41", "q61", "q78", "q8", "q80", "q93")
emp_questions <- c("q1", "q10", "q13", "q17", "q4", "q41", "q61", "q78", "q8", "q80")
dsm_questions <- c("q4","q8", "q10", "q41","q78", "q93", "q104")


scale_sets <- list(emp = emp_questions, dsm = dsm_questions)

raters <- c("m", "v", "t")

ages <- c("7", "10", "12")


## outcome datafrome
sumscore_df <- df[, c("FID", "PID", "participant_id", "twin_id", "mult_id_fam", "birthorder", 
                      "sex", "twzyg", "yob", "agem7", "agem10","agem12",
                      "agev7", "agev10","agev12",
                      "agetrf7", "agetrf10","agetrf12")]


##----------------------------------MAIN LOOP---------------------------------##
for (scale_name in names(scale_sets)) {
  
  current_scale <- scale_sets[[scale_name]]
  
  for (i in raters){
    for (j in ages){
      
      
      ## Participation information column
      if (i == "t"){
        is_participated <- paste0("in_YS_", "TRF", j)
        
        is_teacher = TRUE
      } else {
        is_participated <- paste0("in_YS_", j, toupper(i))
        is_teacher = FALSE
      }
      
      
      ## filtering the dataset for each rater and age
      ageRater <- paste0(i,j)
      
      question_columns <- paste0(questions, ageRater)
      scale_columns <- paste0(current_scale, ageRater)
      
      filtered_df <- df[, c("participant_id", "sex", is_participated, question_columns), drop = FALSE]
  
      
      ##-----------------------------Missingness------------------------------##
      
      
      ## number of NAs per subject
      filtered_df$na_count <- apply(filtered_df[, scale_columns], 1, function(row) sum(is.na(row)))
      
      
      ## remove subjects with more than 20% missing
      df_mind <- filtered_df %>%
        filter(na_count/length(current_scale) <= mind_threshold)
      
      
      ## Storing missingness information and sex composition
      total_n <- sum(filtered_df[, is_participated] == 1, na.rm = TRUE)
      
      
      missing_n <- total_n - nrow(df_mind)
      female_n <- sum(df_mind$sex == "female")
      male_n <- sum(df_mind$sex == "male")
      
      missingness <- c(ageRater, total_n, missing_n, nrow(df_mind), 
                       round(missing_n/total_n, 2), female_n, male_n, scale_name)
      
      missing_info_scale <- rbind(missing_info_scale, missingness)
      
      
      ## Missingness by question
      aggr_plot <- aggr(df_mind[, question_columns],numbers=TRUE, sortVars=TRUE, 
                        labels=names(df_mind[, question_columns]), plot = FALSE)
      
      missing_info <- data.frame(aggr_plot$missings) %>%
        mutate(proportion = Count / nrow(df_mind)) %>%
        arrange(desc(proportion)) %>%
        extract(Variable, into = "item_no", regex = "^(q\\d+)", remove = FALSE) %>%
        mutate(item_no = factor(item_no, levels = item_no))
      
      
      # plotting 
      missing_plot_title <- paste0("Proportion of Missingness by Items in ", 
                                   i, "-Rated ", j, "-years-old Subjects")
      
      missing_plot <- ggplot(missing_info, aes(x = item_no, y = proportion)) +
        geom_bar(stat = "identity", fill = "#1f77b4") +
        labs(
          title = missing_plot_title,
          x = "Variable",
          y = "Proportion of Missing Values"
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = "Arial"),
          axis.title = element_text(size = 20, family = "Arial", margin = margin(t = )),
          axis.text = element_text(size = 16, family = "Arial"),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.2))
      
      # Saving the plot
      plot_file_name <- file.path(output_miss, paste0(ageRater, "miss_plot.png"))
      ggsave(plot_file_name, missing_plot, width = 1920 / 100, height = 1080 / 100, dpi = 300, units = "in")

      
      ##-----------------------------Imputation-------------------------------##
      
      ## scale columns as factor
      df_mind_backup <- df_mind
      df_mind[, scale_columns] <- lapply(df_mind[, scale_columns], factor)
      
      
      ## MICE imputation
      imp_res <- mice(df_mind[, scale_columns], m=5, maxit=1, meth='polr', seed=500)
      
      ## Sumscore calculation for each imputed df
      vars <- grep("^q", names(imp_res$data), value = TRUE)
      
      expr <- parse(text = paste0("as.numeric(as.character(", vars, "))", collapse = " + "))[[1]]
      
      sumscores <- with(imp_res, eval(expr))
      
      
      ## Average sumscores
      sumscore_list <- sumscores$analyses
      sumscore_combined <- do.call(cbind, sumscore_list)
      
      df_imputed <- data.frame(cbind(df_mind$participant_id, rowMeans(sumscore_combined, na.rm = TRUE)))
      df_imputed[, 2] <- as.numeric(as.character(df_imputed[, 2]))

      
      col_name <- paste0(ageRater, "_", scale_name)
      colnames(df_imputed) <- c("participant_id", col_name)
      
      sumscore_df <- sumscore_df %>%
        left_join(df_imputed, by = "participant_id")
      
      ##--------------------------Non-imputed sumscores-----------------------##
    
      
      raw_sum <- data.frame(rowSums(df_mind_backup[,scale_columns], na.rm = TRUE))
      raw_sum <- cbind(df_mind_backup$participant_id, raw_sum)
      
      colnames(raw_sum) <- c("participant_id", paste0(ageRater, "_", scale_name, "_raw"))
      
      sumscore_df <- sumscore_df %>%
        left_join(raw_sum, by = "participant_id")
      
        
      ##--------------------------Non-imputed sumscores-----------------------##
      
      # Print progress
      cat("Processed:", scale_name, i, j, "\n")
      
      
    } # age loop
  } # rater loop
} # scale loop


saveRDS(sumscore_df, file.path(output_dir, "sumscore_RawData.rds"))  # Save as RDS


sumscore_df_with_raw <- sumscore_df


## Formatting the data

score_columns <- c("m7_emp", "m7_dsm", "m10_emp", "m10_dsm", "m12_emp","m12_dsm",
                   "v7_emp", "v7_dsm", "v10_emp", "v10_dsm", "v12_emp", "v12_dsm",
                   "t7_emp", "t7_dsm", "t10_emp", "t10_dsm", "t12_emp", "t12_dsm")


## Converting to long format
df_long <- sumscore_df %>%
  pivot_longer(cols = score_columns, 
               names_to = "variable", 
               values_to = "score")

## Extracting required information
df_long <- df_long %>%
  mutate(
    scale = str_extract(variable, "(?<=_)[^_]+$"),  
    respondent = str_extract(variable, "[mvt]"),  
    age = as.numeric(str_extract(variable, "(?<=[mv|t])\\d+")),
    zyg = case_when(
      twzyg %in% c(1, 3) ~ "MZ",
      twzyg %in% c(2, 4, 5, 6) ~ "DZ")
  )


## Exacting the age at measurement
df_long$respondent2 <- gsub('t', 'trf', df_long$respondent)

age_lookup <- sumscore_df %>%
  dplyr::select(starts_with("age")) %>%
  mutate(participant_id = sumscore_df$participant_id) %>%
  pivot_longer(cols = -participant_id, names_to = "exact_age_col", values_to = "measured_age")

age_lookup <- age_lookup %>%
  mutate(scale = "emp")
age_lookup2 <- age_lookup %>%
  mutate(scale = "dsm")

age_lookup <- rbind(age_lookup, age_lookup2)


## Adding exact the age at measurement to the long dataframe
df_long <- df_long %>%
  mutate(
    exact_age_col = paste0("age", respondent2, age),  # Create the exact age column name
  ) %>%
  left_join(age_lookup, by = c("participant_id", "exact_age_col", "scale"))

df_long <- df_long  %>%
  dplyr::select(c(-respondent2, -exact_age_col))


## final format of the data
data <- df_long[,c("FID", "PID", "participant_id", "twin_id", "mult_id_fam", "birthorder", 
                   "sex", "zyg", "yob", "variable", "respondent", "age", "measured_age", "scale", 
                   "score")]


data <- data %>%
  mutate(
    measured_age = as.numeric(as.character(measured_age))) # Factor -> Character -> Numeric


## Adding the assigned_age based on exact_age
data <- data %>%
  mutate(
    assigned_age = case_when(
      measured_age > 6 & measured_age <= 8 ~ 7,       # Assign to age 7 if 6 < x <= 8
      measured_age > 9 & measured_age <= 11 ~ 10,     # Assign to age 10 if 9 < x <= 11
      measured_age > 11 & measured_age <= 13 ~ 12,    # Assign to age 12 if 11 < x <= 13
      TRUE ~ NA_real_  # Assign NA for any other values (optional, for clarity)
    )
  )


## Duplicate removal
data <- data %>%
  mutate(age_diff = abs(assigned_age - measured_age)) %>%
  group_by(participant_id, scale, respondent, assigned_age) %>%
  filter(age_diff == min(age_diff)) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  dplyr::select(-age_diff)


## Father label from v to f
data$respondent[data$respondent == "v"] <- "f"

saveRDS(data, file.path(output_dir, "sumscore_Data.rds"))  # Save as RDS

missing_info_scale <- missing_info_scale[-1, ]

missing_path <- file.path(output_dir, paste0("t",mind_threshold,"_miss_info.xlsx"))

write.xlsx(missing_info_scale, file = missing_path)


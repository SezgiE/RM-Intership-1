## Sezgi Ercan - 20.06.2025
## Script 1 for item level analysis

##------------------------- Preprocessing of Raw Data ------------------------##

# Necessary Libraries
library(tidyverse)
library(foreign)
library(stringr)
library(psych)

rm(list = ls(all=TRUE)) # clean memory


## Load the raw SPSS data file from NTR
df_raw <- read.spss("/Users/sezgi/Desktop/scripts/data/upload_20250303_LL/PHE_20250303_4758_LL.sav", 
                    to.data.frame = TRUE, use.value.labels = FALSE)

## Set the output directory
## 12 different .RDS files will be produced as output files (one .RDS file for each item) 
## Creating an empty file for the output directory is recommended
output_dir <- "/Users/sezgi/Desktop/scripts/data/item_level_data"


#
#----------------------------------Code Starts---------------------------------#
#

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
df$participant_id <- paste(df$FID, df$PID, sep = "")
df$twin_id <- paste(df$FID, df$mult_id_fam, sep = "")


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


## Pre-proccesing and restructring data and creating dataframe for each item
# Identify age columns
age_cols <- names(df)[grepl("^age(m|v|trf)\\d+$", names(df))]  # Columns like "agem7", "agev7", "agetrf7"


## Converting to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("q"), 
               names_to = "question_code", 
               values_to = "response")


## Extracting necessary variables
df_long <- df_long %>%
  mutate(
    question = str_extract(question_code, "q\\d+"),  
    respondent = str_extract(question_code, "[mvt]"),  
    age = as.numeric(str_extract(question_code, "\\d+$")),
    zyg = case_when(
      twzyg %in% c(1, 3) ~ "MZ",
      twzyg %in% c(2, 4, 5, 6) ~ "DZ")
  ) %>%
  dplyr::select(FID, PID, participant_id, twin_id, mult_id_fam, birthorder, sex, zyg, yob, question, respondent, age, response)


## Exacting the age at measurement
df_long$respondent2 <- gsub('t', 'trf', df_long$respondent)

age_lookup <- df %>%
  dplyr::select(starts_with("age")) %>%
  mutate(participant_id = df$participant_id) %>%
  pivot_longer(cols = -participant_id, names_to = "exact_age_col", values_to = "exact_age")


## Adding exact the age at measurement to the long dataframe
df_long <- df_long %>%
  mutate(
    exact_age_col = paste0("age", respondent2, age),  # Create the exact age column name
  ) %>%
  left_join(age_lookup, by = c("participant_id", "exact_age_col"))

df_long <- df_long  %>%
  dplyr::select(c(-respondent2, -exact_age_col))


## Adding the assigned_age based on exact_age
df_long <- df_long %>%
  mutate(
    assigned_age = case_when(
      exact_age > 6 & exact_age <= 8 ~ 7,       # Assign to age 7 if 6 < x <= 8
      exact_age > 9 & exact_age <= 11 ~ 10,     # Assign to age 10 if 9 < x <= 11
      exact_age > 11 & exact_age <= 13 ~ 12,    # Assign to age 12 if 11 < x <= 13
      TRUE ~ NA_real_  # Assign NA for any other values (optional, for clarity)
    )
  )


## Duplicate removal
final_data <- df_long %>%
  mutate(age_diff = abs(assigned_age - exact_age)) %>%
  group_by(participant_id, question, respondent, assigned_age) %>%
  filter(age_diff == min(age_diff)) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  dplyr::select(-age_diff)


## Father label from v to f
final_data$respondent[final_data$respondent == "v"] <- "f"


## Recode sex as character
final_data <- final_data %>%
  mutate(
    sex = case_when(
      sex == 1 ~ "male",
      sex == 2 ~ "female")
  )


## Create separate datasets for each question as "q1, q4, to q104 etc."

questions <- unique(final_data$question)
remove <- c("q15", "q22", "q24", "q53", "q67", "q100")
questions <- questions[!questions %in% remove]


# .RDS file with whole data 
# saveRDS(final_data, file.path("_", "whole_data.rds")) # should be saved to another directory


for (q in questions) {
  df_question <- final_data %>% filter(question == q)
  
  saveRDS(df_question, file.path(output_dir, paste0(q, "_data.rds")))  # Save as RDS
  
  cat("Saved", q, "data to", file.path(output_dir, paste0(q, "_data.rds")), "\n")
}


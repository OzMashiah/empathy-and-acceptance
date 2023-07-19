rm(list = ls()) # remove all variables from environment
cat("\014") # clean console
# fix score flag
fix_score = TRUE
# import libraries
library(readr)
library(dplyr)

#set working directory
setwd("C:/Users/Oz/Documents/Empathy And Acceptance/empathy-and-acceptance/")

# read raw data files
beginning<-read_tsv('rawdata\\Beginning Survey_July 2, 2023_04.49.tsv', locale = locale(encoding = "UTF-16"))
ending<-readr::read_tsv("rawdata\\Ending Survey_July 2, 2023_04.49.tsv", locale = locale(encoding = "UTF-16"))
daily<-readr::read_tsv("rawdata\\daily survey_July 2, 2023_04.48.tsv", locale = locale(encoding = "UTF-16"))

# Remove duplicate ID except the last occurrence & people who did not finish.
beginning_clean <- beginning %>%
  group_by(ID) %>%
  filter(row_number() == n() & Progress == 100 & DistributionChannel == "anonymous")

ending_clean <- ending %>%
  group_by(ID) %>%
  filter(row_number() == n() & Progress == 100 & DistributionChannel == "anonymous")

daily_clean <- daily %>%
  filter(DistributionChannel == "anonymous" & Progress == 100 &
           (Q18_5 == 2 | is.na(Q18_5) | Q18_5 == "") &
           (Q12_5 == 2 | is.na(Q12_5) | Q12_5 == ""))

# convert to numeric.
ending_clean <- ending_clean %>%
  mutate(across(starts_with("Nonacceptance"), as.numeric))
beginning_clean <- beginning_clean %>%
  mutate(across(starts_with("Nonacceptance"), as.numeric))

ending_clean <- ending_clean %>%
  mutate(across(starts_with("EmpathicConcern"), as.numeric))
beginning_clean <- beginning_clean %>%
  mutate(across(starts_with("EmpathicConcern"), as.numeric))

# Q12 (negative), Q18 (positive), Q2 (no-exposure)
daily_clean <- daily_clean %>%
  mutate(across(starts_with("Q12"), as.numeric))
daily_clean <- daily_clean %>%
  mutate(across(starts_with("Q18"), as.numeric))
daily_clean <- daily_clean %>%
  mutate(across(starts_with("Q2"), as.numeric))


# Fix incorrect scoring in Male-IRI (beginning and ending) and No-Exposure (daily)
# IRI
if (fix_score) {
  apply_rule_iri <- function(gender, col) {
    if (gender == 2) {
      col <- case_when(
        col == 1 ~ 1,
        col == 16 ~ 2,
        col == 2 ~ 3,
        col == 3 ~ 4,
        col == 4 ~ 5,
        TRUE ~ col
      )
    }
    return(col)
  }

  columns_to_modify_iri <- grep("^EmpathicConcern", names(beginning_clean), value = TRUE)

  beginning_clean <- beginning_clean %>%
    mutate_at(vars(all_of(columns_to_modify_iri)), ~apply_rule_iri(Gender, .))
  ending_clean <- ending_clean %>%
    mutate_at(vars(all_of(columns_to_modify_iri)), ~apply_rule_iri(Gender, .))

# Daily
  apply_rule_daily <- function(col) {
    col <- case_when(
      col == 2 ~ 1,
      col == 3 ~ 2,
      col == 4 ~ 3,
      col == 5 ~ 4,
      col == 6 ~ 5,
      is.na(col) ~ NA_real_,
      TRUE ~ col
    )
    return(col)
  }
  columns_to_modify_daily <- grep("^Q2", names(daily_clean), value = TRUE)

  daily_clean <- daily_clean %>%
    mutate_at(vars(all_of(columns_to_modify_daily)), ~apply_rule_daily(.))

fix_score = FALSE
}

# DERS Questionnaire
# Nonacceptance of emotional responses scale. 3(11), 4(12), 11(21), 12(23), 13(25), 16(29)
# Non-acceptance M = 14.67, SD = 5.92

# calculate the nonacceptance scale for the DERS questionnaire.
ending_ders <- ending_clean %>%
  mutate(DERS_NA = Nonacceptance_3 + Nonacceptance_4 + Nonacceptance_11 +
           Nonacceptance_12 + Nonacceptance_13 + Nonacceptance_16)

beginning_ders <- beginning_clean %>%
  mutate(DERSNA = Nonacceptance_3 + Nonacceptance_4 + Nonacceptance_11 +
           Nonacceptance_12 + Nonacceptance_13 + Nonacceptance_16)
         
# Difficulty engaging in goal-directed behavior scale. 5(13), 9(18), -(20R), -(26), 18(33)
# Impulse control difficulties: -(3), 6(14), 10(19), -(24R), 14(27), -(32)
# Lack of emotional awareness: -(2R), -(6R), -(8R), -(10R), -(17R), -(34R)
# Limited access to emotion regulation strategies: 7(15), 8(16), -(22R), 15(28), 17(30), -(31), -(35),19(36)
# Lack of emotional clarity: -(1R), -(4), 1(5), -(7R), 2(9)


# IRI Questionnaire
# Perspective Taking: 3(3R), 8(8), 11(11), 15(15R), -(21), -(25), -(28)
ending_iri <- ending_ders %>%
  mutate(IRI_PT = recode(EmpathicConcern_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_8 + EmpathicConcern_11 +
           recode(EmpathicConcern_15, `1` = 5, `2` = 4, `4` = 2, `5` = 1))

beginning_iri <- beginning_ders %>%
  mutate(IRI_PT = recode(EmpathicConcern_3, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_8 + EmpathicConcern_11 +
           recode(EmpathicConcern_15, `1` = 5, `2` = 4, `4` = 2, `5` = 1))
# Fantasy Scale: 1(1), 5(5), 7(7R), 12(12R), 16(16), -(23), -(26)
ending_iri <- ending_iri %>%
  mutate(IRI_FC = recode(EmpathicConcern_7, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_1 + EmpathicConcern_5 + EmpathicConcern_16 +
           recode(EmpathicConcern_12, `1` = 5, `2` = 4, `4` = 2, `5` = 1))

beginning_iri <- beginning_iri %>%
  mutate(IRI_FC = recode(EmpathicConcern_7, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_1 + EmpathicConcern_5 + EmpathicConcern_16 +
           recode(EmpathicConcern_12, `1` = 5, `2` = 4, `4` = 2, `5` = 1))
# Emphatic Concern: 2(2), 4(4R only in ending), 9(9), 14(14), -(18R), -(20), -(22)
ending_iri <- ending_iri %>%
  mutate(IRI_EC = recode(EmpathicConcern_4, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_2 + EmpathicConcern_9 + EmpathicConcern_14)

beginning_iri <- beginning_iri %>%
  mutate(IRI_EC = EmpathicConcern_4 + EmpathicConcern_2 + EmpathicConcern_9 +
           EmpathicConcern_14)
# Personal Distress: 6(6), 10(10), 13(13R), -(17), -(19R), -(24), -(27)
ending_iri <- ending_iri %>%
  mutate(IRI_PD = recode(EmpathicConcern_13, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_6 + EmpathicConcern_10)

beginning_iri <- beginning_iri %>%
  mutate(IRI_PD = recode(EmpathicConcern_13, `1` = 5, `2` = 4, `4` = 2, `5` = 1) +
           EmpathicConcern_6 + EmpathicConcern_10)
# IRI Overall Sum
ending_iri <- ending_iri %>%
  mutate(IRI_Total = IRI_PT + IRI_FC + IRI_EC + IRI_PD)

beginning_iri <- beginning_iri %>%
  mutate(IRI_Total = IRI_PT + IRI_FC + IRI_EC + IRI_PD)

# Daily Negative Experience Questionnaire.
daily_scales <- daily_clean %>%
  mutate(Negative_Total = Q12_1 + Q12_2 + Q12_3 + Q12_4 + Q12_6 + Q12_7 + Q12_8)
# Daily Positive Experience Questionnaire.
daily_scales <- daily_scales %>%
  mutate(Positive_Total = Q18_1 + Q18_2 + Q18_3 + Q18_4 + Q18_6 + Q18_7 + Q18_8)
# Daily No Exposure Questionnaire.
daily_scales <- daily_scales %>%
  mutate(NoExposure_Total = Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_6 + Q2_7 +
           Q2_8 + Q2_9 + Q2_10 + Q2_11 + Q2_12 + Q2_13 + Q2_14)

# remove subjects that have not done the minimum requirements:
# 1. done the beginning survey.
# 2. at least 2 daily surveys.
# 3. done the ending survey
daily_twice_ids <- daily_scales %>%
  group_by(ID) %>%
  filter(n() >= 2) %>%
  distinct(ID) %>%
  pull(ID)

beginning_ending_ids <- beginning_iri %>%
  semi_join(ending_iri, by = "ID") %>%
  distinct(ID) %>%
  pull(ID)

result_ids <- intersect(daily_twice_ids, beginning_ending_ids)

beginning_final <- beginning_iri %>%
  filter(ID %in% result_ids)

ending_final <- ending_iri %>%
  filter(ID %in% result_ids)

daily_final <- daily_scales %>%
  filter(ID %in% result_ids)

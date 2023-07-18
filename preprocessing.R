rm(list = ls()) # remove all variables from environment
cat("\014") # clean console
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
beginning_unique <- beginning %>%
  group_by(ID) %>%
  filter(row_number() == n() & Progress == 100)

ending_unique <- ending %>%
  group_by(ID) %>%
  filter(row_number() == n() & Progress == 100)
  
# DERS Questionnaire
# Nonacceptance of emotional responses scale. 3(11), 4(12), 11(21), 12(23), 13(25), 16(29)
# Non-acceptance M = 14.67, SD = 5.92

# convert to numeric.
ending_ders <- ending_unique %>%
  mutate(across(starts_with("Nonacceptance"), as.numeric))
beginning_ders <- beginning_unique %>%
  mutate(across(starts_with("Nonacceptance"), as.numeric))

# calculate the nonacceptance scale for the DERS questionnaire.
ending_ders <- ending_ders %>%
  mutate(DERS_NA = Nonacceptance_3 + Nonacceptance_4 + Nonacceptance_11 +
           Nonacceptance_12 + Nonacceptance_13 + Nonacceptance_16)

beginning_ders <- beginning_ders %>%
  mutate(DERSNA = Nonacceptance_3 + Nonacceptance_4 + Nonacceptance_11 +
           Nonacceptance_12 + Nonacceptance_13 + Nonacceptance_16)
         
# Difficulty engaging in goal-directed behavior scale. 5(13), 9(18), -(20R), -(26), 18(33)
# Impulse control difficulties: -(3), 6(14), 10(19), -(24R), 14(27), -(32)
# Lack of emotional awareness: -(2R), -(6R), -(8R), -(10R), -(17R), -(34R)
# Limited access to emotion regulation strategies: 7(15), 8(16), -(22R), 15(28), 17(30), -(31), -(35),19(36)
# Lack of emotional clarity: -(1R), -(4), 1(5), -(7R), 2(9)


# IRI Questionnaire
# Convert to numeric
ending_iri <- ending_ders %>%
  mutate(across(starts_with("EmpathicConcern"), as.numeric))
# Perspective Taking: 3(3), 8(8), 11(11), 15(15R), -(21), -(25), -(28)

# Fantasy Scale: 1(1), 5(5), 7(7R), 12(12R), 16(16), -(23), -(26)

# Emphatic Concern: 2(2), 4(4), 9(9), 14(14), -(18R), -(20), -(22)

# Personal Distress: 6(6), 10(10), 13(13R), -(17), -(19R), -(24), -(27)

# IRI Overall Sum

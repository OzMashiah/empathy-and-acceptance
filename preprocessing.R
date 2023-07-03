rm(list = ls()) # remove all variables from environment
cat("\014") # clean console
# import libraries
library(readr)

#set working directory
setwd("C:/Users/Oz/Documents/Empathy And Acceptance/empathy-and-acceptance/")

# read raw data files
beginning<-read_tsv('rawdata\\Beginning Survey_July 2, 2023_04.49.tsv', locale = locale(encoding = "UTF-16"))
ending<-readr::read_tsv("rawdata\\Ending Survey_July 2, 2023_04.49.tsv", locale = locale(encoding = "UTF-16"))
daily<-readr::read_tsv("rawdata\\daily survey_July 2, 2023_04.48.tsv", locale = locale(encoding = "UTF-16"))

#

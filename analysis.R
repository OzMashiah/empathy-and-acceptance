rm(list = ls()) # remove all variables from environment
cat("\014") # clean console

# import libraries
library(ggplot2)
library(hrbrthemes)
library(gridExtra)

# set working directory
setwd("C:/Users/Oz/Documents/Empathy And Acceptance/empathy-and-acceptance/")

# read preprocessed data files
beginning<-read_csv('preprocessed\\preprocessed_beginning.csv')
ending<-readr::read_csv("preprocessed\\preprocessed_ending.csv")
daily<-readr::read_csv("preprocessed\\preprocessed_daily.csv")

# descriptive statistics for both questionnaires and each subscale with alpha


# correlation matrix with all subscales for exploration



# correlation between empathic concern and acceptance
beginning_cor <- cor.test(beginning$IRI_EC, beginning$DERS_A)
ending_cor <- cor.test(ending$IRI_EC, ending$DERS_A)

# linear trend + confidence interval
EC1A1 <- ggplot(beginning, aes(x=DERS_A, y=IRI_EC)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  ggtitle("Beginning Questionnaire") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Acceptance") +
  ylab("Empathic Concern") +
  theme(axis.title.x = element_text(size=15 ,hjust=0.5)) +
  theme(axis.title.y = element_text(size=15 ,hjust=0.5)) +
  geom_text(aes(13, 10, label=paste("r = ", format(round(beginning_cor$estimate,
                                                         3), nsmall =3))), size = 7)

EC2A2 <- ggplot(ending, aes(x=DERS_A, y=IRI_EC)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  ggtitle("Ending Questionnaire") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Acceptance") +
  ylab("Empathic Concern") +
  theme(axis.title.x = element_text(size=15 ,hjust=0.5)) +
  theme(axis.title.y = element_text(size=15 ,hjust=0.5)) +
  geom_text(aes(15, 10, label=paste("r = ", format(round(ending_cor$estimate,
                                                         3), nsmall =3))), size = 7)

grid.arrange(EC1A1, EC2A2, ncol = 2)


rm(list = ls()) # remove all variables from environment
cat("\014") # clean console

# import libraries
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(ggpubr)
library(tidyr)

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
# beginning
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
#ending
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


# t-test between before and after daily survey
# Combine dataframes
combined_data_wide <- left_join(beginning, ending, by = "ID", suffix = c("_beginning", "_ending")) %>%
  select(ID, IRI_EC_beginning, DERS_A_beginning, IRI_EC_ending, DERS_A_ending)
combined_data_long <- rbind(mutate(beginning[, c('ID', 'IRI_EC', 'DERS_A')], Group = "Beginning"),
                       mutate(ending[, c('ID', 'IRI_EC', 'DERS_A')], Group = "Ending"))

# Acceptance before after
t_test_A1A2 <- t.test(combined_data_wide$DERS_A_beginning,
                      combined_data_wide$DERS_A_ending, paired = TRUE)
A1A2 <- ggplot(combined_data_long, aes(x = Group, y = DERS_A, fill = Group)) +
  geom_boxplot() +
  labs(title = "Acceptance before and after Daily Journal",
       x = "Group",
       y = "Acceptance") +
  theme_minimal() +
  annotate("text",
           x = 1.5, y = max(combined_data_long$DERS_A)-2,
           label = paste("p-value:", format(format(round(t_test_A1A2$p.value,3),
                                                   nsmall = 3))),
           hjust = 0.5, vjust = -0.5,
           size = 4, color = "black") 

# Empathy before after
t_test_EC1EC2 <- t.test(combined_data_wide$IRI_EC_beginning,
                        combined_data_wide$IRI_EC_ending, paired = TRUE)
EC1EC2 <- ggplot(combined_data_long, aes(x = Group, y = IRI_EC, fill = Group)) +
  geom_boxplot() +
  labs(title = "Empathic Concern before and after Daily Journal",
       x = "Group",
       y = "Empathic Concern") +
  theme_minimal() +
  annotate("text",
           x = 1.5, y = max(combined_data_long$IRI_EC)-1,
           label = paste("p-value:", format(format(round(t_test_EC1EC2$p.value,3),
                                                   nsmall = 3))),
           hjust = 0.5, vjust = -0.5,
           size = 4, color = "black") 

grid.arrange(EC1EC2, A1A2, ncol = 2)


# ggpaired plot trying to beautify.
ordered_long <- combined_data_long %>%
  arrange(Group, ID) 

ggpaired(ordered_long, x = "Group", y = "IRI_EC", 
         order = c("Beginning", "Ending"),
         ylab = "Weight", xlab = "Groups")


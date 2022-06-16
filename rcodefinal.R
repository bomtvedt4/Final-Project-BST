library('mvnormtest')
library('car')
library('IDPmisc')
library(dplyr)
library("rcompanion")
install.packages('dplyr')
#libraries

#Does the amount of hours affect general anxiety positively or negatively?
#Does the amount of hours affect satisfaction with life positively or negatively?
data <- read.csv("~/Final Project BST/edited_data.csv")
View(data)
#wrangling
#handles only 5000 rows
#getting rid of two columns due to na values
#keep dep variables
data$SWL_T <- as.numeric(data$SWL_T)

#hours versus mental impact
hours_swl <- aov(data$SWL_T ~ data$Hours_BIN)
summary(hours_swl)
hours_gad <- aov(data$GAD_T ~ data$Hours_BIN)
summary(hours_gad)
hours_spi <- aov(data$SPIN_T ~ data$Hours_BIN)
summary(hours_spi)

streamsdata <- data %>% mutate(streamsbin = cut(streams, breaks=c(0, 10, 20, 30,40,50,60,70,80)))
#Stream hours versus mental impact
streams_swl <- aov(data$SWL_T ~ streamsdata$streamsbin)
summary(streams_swl)
streams_gad <- aov(data$GAD_T ~ streamsdata$streamsbin)
summary(streams_gad)
streams_spi <- aov(data$SPIN_T ~ streamsdata$streamsbin)
summary(streams_spi)

#work versus mental impact
work_swl <- aov(data$SWL_T ~ data$Work)
summary(work_swl)
work_gad <- aov(data$GAD_T ~ data$Work)
summary(work_gad)
work_spi <- aov(data$SPIN_T ~ data$Work)
summary(work_spi)

#why play versus mental impact
whyplay_swl <- aov(data$SWL_T ~ data$whyplay)
summary(whyplay_swl)
whyplay_gad <- aov(data$GAD_T ~ data$whyplay)
summary(whyplay_gad)
whyplay_spi <- aov(data$SPIN_T ~ data$whyplay)
summary(whyplay_spi)


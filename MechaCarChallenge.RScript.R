#Dependencies
library(dplyr)

#make table from imported data
mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#model linear regression
lm(mpg ~ AWD+ground_clearance+spoiler_angle+vehicle_weight+vehicle_length,mpg_table)

#summarize linear model
summary(lm(mpg ~ AWD+ground_clearance+spoiler_angle+vehicle_weight+vehicle_length,mpg_table))
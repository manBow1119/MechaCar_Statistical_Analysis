#Dependencies
library(dplyr)

#make table from imported data
mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#model linear regression
lm(mpg ~ AWD+ground_clearance+spoiler_angle+vehicle_weight+vehicle_length,mpg_table)

#summarize linear model
summary(lm(mpg ~ AWD+ground_clearance+spoiler_angle+vehicle_weight+vehicle_length,mpg_table))

#import and read csv
coil_table <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors=F)

#create total summary dataframe
total_summary <-coil_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
 
#create lot groups for summarized data
lot_summary = coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')

#t-test for stat significance
t.test(log10(coil_table$PSI), mu=mean(log10(total_summary$Mean))) 

#t-test with subset
#create subsets-- do we need these?
coil_table_lot_1 = subset(coil_table, Manufacturing_Lot == "Lot1")
coil_table_lot_2 = subset(coil_table, Manufacturing_Lot == "Lot2")
coil_table_lot_3 = subset(coil_table, Manufacturing_Lot == "Lot3")

t.test((coil_table_lot_1$PSI), mu=mean(coil_table$PSI)) 
t.test((coil_table_lot_2$PSI), mu=mean(coil_table$PSI)) 
t.test((coil_table_lot_3$PSI), mu=mean((coil_table$PSI))) 

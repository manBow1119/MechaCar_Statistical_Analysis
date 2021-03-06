# MechaCar_Statistical_Analysis
### Project Overview
The purpose of this project was to utilize statistical analysis packages available in the R language to determine the impact on various vehicular measurements on the vehicle's overall fuel-efficiency. Descriptive and inferential statistics were used to compare vehicles across one company's various lots, and recommendations for generalizing this approach to comparing competition are addressed in the summary.

### Technologies
* R
* R-libraries:
    * tidyverse, ggplot2

## Linear Regression to Predict MPG
* Using the linear fit model function in R, a linear regression was performed on vehicle's MPG (miles per gallon) compared to the drivetrain, vehicle length, wehicle weight, spoiler angle, and ground clearance. The output below clarifies which of these features impacted the fuel efficiency of the vehicle. Significance codes show that other than the intercept, ground clearance and vehicle length provided a non-random amount of variance to the relative fuel efficiency of the vehicles. Given that the coefficients for each of these significant factors is 3.5 and 6.3 respctively, the "slope" of the regression line is certainly not zero. R-squared values provide some insight as to how well the regression line fits the data. Values of .6825 and .7149 show that the linear model is a modest predictor of mpg. It might provide some general idea of trends, but certainly should not be used to make precise estimations.
* ![MPG influence of other factors](https://github.com/manBow1119/MechaCar_Statistical_Analysis/blob/main/MechaCar_linreg.png)

## Summary Statistics on Suspension Coils
* Utilizing R's summarize() function, an analysis of the Suspension Coil data revealed initial Median of 1500 psi, and Mean of 1498.78. While these two measures of central tendencey seem close there is some variance in the data creating large standard deviations as seen in the total_summary table below:
* ![Total Summary of Suspension Coil Data](https://github.com/manBow1119/MechaCar_Statistical_Analysis/blob/main/total_summary_suspension_coil.png)

* A closer examination of the suspension coil data by individual lots reveals the source of the high variance. Lot 1 has equivalent mean and median, with very low resulting variance and standard deviation(both < 1). Lot 2, although very close mean (1500.0) and median (1500.2), demonstrated greater variance (7.47) and resulting stadard deviation (2.73). Lot 3, however, demonstrated considerably more variance, higher standard deviations, and greater differences between central tendency measures. This disaggregated table reveals inconsistencies about suspension coil maintenance at these lots.
* ![Lot Summary of Suspension Coil Data](https://github.com/manBow1119/MechaCar_Statistical_Analysis/blob/main/Lot%20_summary_suspension_coil.png)

## T-Test on Suspension Coils
The breakdown of the suspension coil table revealed potential inconsistencies between manufacturing lots. To determine if the difference is significant, t-tests for each lot mean PSI was compared to population means. 
 * T-test for Lot 1 returned a p-value of 1.57E-11 (very close to 0), rejecting the "true mean" of 1498.78, which fell well outside the 95% confidence interval(1499.719-1500.281)
 * Similarly, t-test for Lot 2 gave a p-value of .0006, again rejecting the "true mean". Althought the confidence interval for this lot was larger (1499.423 - 1500.977), it also did not include the population mean.
 * The final t-test for Lot 3 returned a p-value of .16, failing to reject the mean of the population. The confidence interval (1492.431 - 1499.849) includes the population mean. These tests taken together suggest that the variance from the third lot impacts the population mean enough to be statistical significant in difference of means compared to the other two lots.

## Study Design: MechaCar vs Competition
Now that MechaCar has some analyses of its own product, it may serve them to see how they stack up against competition. With the current inflationary market, the car industry has been especially impacted. New and used vehicle prices are at all-time highs, and gas prices continue to rise. To understand how they might better appeal to consumers, it would serve MechaCar to investigate how their products compare to competitors in the areas of vehicle cost and fuel-efficiency. To ensure commensurate comparisons, vehicles would need to be grouped by similar features such as drivetrain, engine size, horse power, and size. Once these subsets are determined, two-sample t-tests can be performed to discern if the statistical means of vehicle cost and fuel-efficieny are significantly different between the two companies. In these tests, support for the null hypothesis would mean that there is no significant difference between the means of these measures for each car company. The alternative hypothesis would reveal a sginificant difference. Additionally, fuel-efficiency could be futher subgrouped to compare city and highway mileage. To be competitive in the car industry and still ensure profits, MechaCar would likely not want statistical means that are significantly less or greater than that of its competitors.

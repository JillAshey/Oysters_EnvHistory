##tests of normality for CI
CI = read.csv("ConditionIndex_HT.csv", header = T)
library(car)

##Change "Total" to column header in data file for dataset you want to analyze
##############################################

##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
#Water Quality--Set1
leveneTest(ConditionIndex~Site, data=CI) #normal 
leveneTest(DeltaCI~Site, data=CI) #normal 

###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
##example: bartlett.test(tTotalPLps~interaction(CO2,Temp), data=trial)
bartlett.test(ConditionIndex~Site, data=CI) #normal
bartlett.test(DeltaCI~Site, data=CI) #normal

###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  

fit<-lm(Gly_transform~Site, data=p)
par(mfrow=c(2,2))
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))
title("QQ-plot of residuals")
qqnorm(rstandard(fit), ylab="Residuals")
qqline(rstandard(fit))
title("QQ-plot of standardized residuals") 
plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit), abs(residuals(fit)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")

#glycogen qqplots look good 

##To identify outliers, use the Bonferroni test:
##Null for the Bonferroni adjusted outlier test is the observation of an outlier. So p<0.05 means no outliers.

library(car)
outlierTest(fit)

##To remove outliers...removed using the largest number first!!
trial.new<-trial[-31,]
trial.new<-trial.new[-17,]

trial<-read.csv("HT_Field_WaterQuality_results.csv", head=T)
head(trial)
library(car)



##Change "Total" to column header in data file for dataset you want to analyze
##############################################

##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(Temperature~Site, data=trial)

###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(tTotalPLps~interaction(CO2,Temp), data=trial)

###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  

fit_temp<-lm(Temperature~Site, data=trial)
plot()
par(mfrow=c(200,200))
qqnorm(residuals(fit_temp), ylab="Residuals")
qqline(residuals(fit_temp))
title("QQ-plot of residuals")
qqnorm(rstandard(fit_temp), ylab="Residuals")
qqline(rstandard(fit_temp))
title("QQ-plot of standardized residuals-temp") 
plot(fitted(fit_temp), residuals(fit_temp), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit_temp), abs(residuals(fit_temp)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")

##To identify outliers, use the Bonferroni test:
##Null for the Bonferroni adjusted outlier test is the observation of an outlier. So p<0.05 means no outliers.

library(car)
outlierTest(fit)

##To remove outliers...removed using the largest number first!!
trial.new<-trial[-31,]
trial.new<-trial.new[-17,]

##Analysis of water quality --set 1
setwd("~/Desktop")
set1 = read.csv("WaterQuality_Experiment_Set1_HT.csv", header=T)
head(set1)
library(car)


##Change "Total" to column header in data file for dataset you want to analyze
##############################################

##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal

##weird with DO mg
leveneTest(DO_mg~Treatment, data=set1) 

leveneTest(DO_P~Treatment, data=set1) #normal
leveneTest(pCO2~Treatment, na.action=na.exclude, data=set1) #normal


leveneTest(Temp~Treatment, data=set1) #not normal
leveneTest(Sal~Treatment, data=set1) #not normal
leveneTest(Cond~Treatment, data=set1) #not normal
leveneTest(pH~Treatment, data=set1) #not normal
leveneTest(spec_pH~Treatment, na.action=na.exclude, data=set1) #normal
leveneTest(TA~Treatment, na.action=na.exclude, data=set1) #not normal

#Tried the 1/x recriprocal transformation on those that were not normal
leveneTest(t_DO_mg~Treatment, data=set1) #normal

leveneTest(t_Temp~Treatment, data=set1) #not normal
leveneTest(t_Sal~Treatment, data=set1) #not normal
leveneTest(t_Cond~Treatment, data=set1) #not normal
leveneTest(t_pH~Treatment, data=set1) #not normal
leveneTest(t_TA~Treatment, na.action=na.exclude, data=set1) #not normal 

#using sqrt transformation
leveneTest(t_TA~Treatment, data=set1, na.action=na.exclude) #not normal
leveneTest(t_Temp~Treatment, data=set1) #not normal
leveneTest(t_Sal~Treatment, data=set1) #not normal
leveneTest(t_Cond~Treatment, data=set1) #not normal
leveneTest(t_pH~Treatment, data=set1) #not normal


#using the x^2
leveneTest(t_TA~Treatment, data=set1, na.action=na.exclude) #not normal
leveneTest(t_Temp~Treatment, data=set1) #not normal
leveneTest(t_Sal~Treatment, data=set1) #not normal
leveneTest(t_Cond~Treatment, data=set1) #not normal
leveneTest(t_pH~Treatment, data=set1) #not normal


#using the x^1/3
leveneTest(t_Temp~Treatment, data=set1) #not normal
leveneTest(t_Sal~Treatment, data=set1) #not normal
leveneTest(t_TA~Treatment, data=set1, na.action=na.exclude) #not normal 
leveneTest(t_Cond~Treatment, data=set1) #not normal
leveneTest(t_pH~Treatment, data=set1) #not normal



###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal

#some normal? some not??
bartlett.test(t_DO_P~Treatment, data=set1) #not normal
bartlett.test(Temp~Treatment, data=set1) #not normal
bartlett.test(t_Temp~Treatment, data=set1) #not normal
bartlett.test(Sal~Treatment, data=set1) #normal
bartlett.test(t_Sal~Treatment, data=set1) #not normal
bartlett.test(Cond~Treatment, data=set1) #not normal
bartlett.test(t_Cond~Treatment, data=set1) #normal
bartlett.test(pH~Treatment, data=set1) #not normal
bartlett.test(spec_pH~Treatment, data=set1) #not normal
bartlett.test(TA~Treatment, data=set1) #not normal
bartlett.test(pCO2~Treatment, data=set1) #not normal


###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  

#i think because my data is kinda weird because of the addition of all the sumps, these graphs are coming out differently?
fit<-lm(t_pCO2~Treatment, data=set1)
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

##To identify outliers, use the Bonferroni test:
##Null for the Bonferroni adjusted outlier test is the observation of an outlier. So p<0.05 means no outliers.

library(car)
outlierTest(fit)

##To remove outliers...removed using the largest number first!!
trial.new<-trial[-31,]
trial.new<-trial.new[-17,]

#Field water quality--evaluations of normality 
trial<-read.csv("HT_Field_WaterQuality_results.csv", head=T)
head(trial)
library(car)



##############################################

hist(trial$Temperature)
hist(trial$spec_pH)
hist(trial$Salinity)
hist(trial$pH)
hist(trial$DO_mg)
hist(trial$DO_Percent)
hist(trial$TA)
hist(trial$pCO2)
hist(trial$Omega_Ca)
hist(trial$Omega_Ar)


##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(spec_pH~Site, data=trial) #normal
leveneTest(Salinity~Site, data=trial) #normal
leveneTest(Temperature~Site, data=trial) #normal
leveneTest(pH~Site, data=trial) #normal    ysi
leveneTest(DO_mg~Site, data=trial) #normal
leveneTest(DO_Percent~Site, data=trial) #normal
leveneTest(TA~Site, data=trial) #normal, but barely
leveneTest(pCO2~Site, data=trial) #normal
leveneTest(Omega_Ca~Site, data=trial) #normal
leveneTest(Omega_Ar~Site, data=trial) #normal


###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(spec_pH~Site, data=trial) #not normal 
bartlett.test(Salinity~Site, data=trial) #not normal
bartlett.test(Temperature~Site, data=trial) #normal
bartlett.test(pH~Site, data=trial) #normal    ysi
bartlett.test(DO_mg~Site, data=trial) #normal
bartlett.test(DO_Percent~Site, data=trial) #normal
bartlett.test(TA~Site, data=trial) #not normal
bartlett.test(pCO2~Site, data=trial) #not normal
bartlett.test(Omega_Ca~Site, data=trial) #not normal
bartlett.test(Omega_Ar~Site, data=trial) #not normal


###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  


##qqplots were not working for me?
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
fit_temp<-lm(Temperature~Site, data=trial)
outlierTest(fit_temp) #outliers 

fit_sal<-lm(Salinity~Site, data=trial)
outlierTest(fit_sal) #no outleirs

fit_spec<-lm(spec_pH~Site, data=trial)
outlierTest(fit_spec) #no outliers 

fit_pH<-lm(pH~Site, data=trial)
outlierTest(fit_pH) #outliers 

fit_DOmg<-lm(DO_mg~Site, data=trial)
outlierTest(fit_DOmg) #no outliers

fit_DOP<-lm(DO_Percent~Site, data=trial)
outlierTest(fit_DOP) #no outliers 

fit_TA<-lm(TA~Site, data=trial)
outlierTest(fit_TA) #no outliers 

fit_pCO2<-lm(pCO2~Site, data=trial)
outlierTest(fit_pCO2) #no outliers 

fit_Ca<-lm(Omega_Ca~Site, data=trial)
outlierTest(fit_Ca) #no outliers 








##To remove outliers...removed using the largest number first!!
trial.new<-trial[-31,]
trial.new<-trial.new[-17,]

#Field water quality--evaluations of normality 
trial<-read.csv("HT_Field_WaterQuality_results.csv", head=T)
head(trial)
library(car)
library(rcompanion)


###2020
##############################################


###Temperature

hist(trial$Temperature)
plotNormalHistogram(trial$Temperature)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(Temperature~Site, data=trial) #normal
###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(Temperature~Site, data=trial) #normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(Temperature~Site, data=trial)
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
##not a great fit - S fitted, bulge in the middle

#transforming the variable using log, sqrt, cube, and reciprocal 
t_log = log(trial$Temperature)
plotNormalHistogram(t_log)
t_sqrt = sqrt(trial$Temperature)
plotNormalHistogram(t_sqrt) #could use in analyses to test how it affects the untransformed temperature variable
t_cube= sign(trial$Temperature) * abs(trial$Temperature)^(1/3)
plotNormalHistogram(t_cube) #could use in analyses to test how it affects the untransformed temperature variable
t_recp = 1/(trial$Temperature)
plotNormalHistogram(t_recp)
##sqrt and cube look sort of normal

#binding the transformed variables into the data set 
trial = cbind(trial, t_sqrt, t_cube)

leveneTest(t_sqrt~Site, data=trial) #normal
fit<-lm(t_sqrt~Site, data=trial)
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
#similar s-shape to the untransformed variable

leveneTest(t_cube~Site, data=trial) #normal
fit<-lm(t_cube~Site, data=trial)
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

##None are relevant enough to transform the data - NOT TRANSFORMED
##But is it normal?






###Salinity 
hist(trial$Salinity)
plotNormalHistogram(trial$Salinity)
leveneTest(Salinity~Site, data=trial) #normal
bartlett.test(Salinity~Site, data=trial) #not normal
fit<-lm(Salinity~Site, data=trial)
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

Slog = log(trial$Salinity) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(Slog)
Ssqrt = sqrt(trial$Salinity)
plotNormalHistogram(Ssqrt)
Scube= sign(trial$Salinity) * abs(trial$Salinity)^(1/3)
plotNormalHistogram(Scube)
Srecp = 1/(trial$Salinity)
plotNormalHistogram(Srecp)
##all look pretty similar 

trial = cbind(trial, Slog)

leveneTest(Slog~Site, data=trial) #normal - but barely
plotNormalHistogram(trial$Slog)
bartlett.test(Slog~Site, data=trial) #not normal

fit<-lm(Slog~Site, data=trial)
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
##Not enough justification to transform - NO TRANSFORM


###Spec pH
hist(trial$specpH)
plotNormalHistogram(trial$specpH)
leveneTest(specpH~trial$Site, data=trial) #normal
bartlett.test(specpH~Site, data=trial) #not normal 
##QQplots
fit<-lm(specpH~Site, data=trial)
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
##pretty good fit all things considered 

pHlog = log(trial$specpH) 
plotNormalHistogram(pHlog)
pHsqrt = sqrt(trial$specpH) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(pHsqrt)
pHcube= sign(trial$specpH) * abs(trial$specpH)^(1/3)
plotNormalHistogram(pHcube)
pHrecp = 1/(trial$specpH) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(pHrecp)

trial = cbind(trial, pHlog, pHsqrt, pHcube)

#log transform testing 
leveneTest(pHlog~Site, data=trial) #normal
bartlett.test(pHlog~Site, data=trial) #not normal 
##QQplots
fit<-lm(pHlog~Site, data=trial)
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

##sqrt transform testing
leveneTest(pHsqrt~Site, data=trial) #normal
bartlett.test(pHsqrt~Site, data=trial) #not normal but barely 
##QQplots
fit<-lm(pHsqrt~Site, data=trial)
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

##Cube transform testing 
leveneTest(pHcube~Site, data=trial) #normal
bartlett.test(pHcube~Site, data=trial) #not normal 
##QQplots
fit<-lm(pHcube~Site, data=trial)
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
##Not enough justification to transform - NO TRANSFORM


###DO mg/L
hist(trial$DO_mg)
plotNormalHistogram(trial$DO_mg)
leveneTest(DO_mg~Site, data=trial) #normal
bartlett.test(DO_mg~Site, data=trial) #normal 
##QQplots
fit<-lm(spec_pH~Site, data=trial)
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

DOlog = log(trial$DO_mg) #log transform
plotNormalHistogram(DOlog)
DOsqrt = sqrt(trial$DO_mg) #square root transform
plotNormalHistogram(DOsqrt) ##could use in analyses to test how it affects the untransformed variable
DOcube= sign(trial$DO_mg) * abs(trial$DO_mg)^(1/3) #cube root transform
plotNormalHistogram(DOcube) ##could use in analyses to test how it affects the untransformed variable
DOrecp = 1/(trial$DO_mg) #reciprocal transform
plotNormalHistogram(DOrecp)

trial = cbind(trial, DOsqrt, DOcube)

leveneTest(DOsqrt~Site, data=trial) #normal
bartlett.test(DOsqrt~Site, data=trial) #normal 
##QQplots
fit<-lm(DOsqrt~Site, data=trial)
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

leveneTest(DOcube~Site, data=trial) #normal
bartlett.test(DOcube~Site, data=trial) #normal 
##QQplots
fit<-lm(DOcube~Site, data=trial)
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
##Not enough justification to transform - NO TRANSFORM

###Total alkalinity 
hist(Water_Set1$TA)
plotNormalHistogram(Water_Set1$TA)
leveneTest(TA~Treatment, data=Water_Set1) #not normal
bartlett.test(TA~Treatment, data=Water_Set1) #not normal
fit<-lm(TA~Treatment, data=Water_Set1)
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
##decent

#transformations
TAlog1 = log(Water_Set1$TA) #log transform
plotNormalHistogram(TAlog1)##could use in analyses to test how it affects the untransformed variable
TAsqrt1 = sqrt(Water_Set1$TA) #square root transform
plotNormalHistogram(TAsqrt1) ##could use in analyses to test how it affects the untransformed variable
TAcube1 = sign(Water_Set1$TA) * abs(Water_Set1$TA)^(1/3) #cube root transform
plotNormalHistogram(TAcube1) #could use in analyses to test how it affects the untransformed variable
TArecp1 = 1/(Water_Set1$TA) #reciprocal transform
plotNormalHistogram(TArecp1)

Water_Set1 = cbind(Water_Set1, TAlog1, TAsqrt1, TAcube1)

leveneTest(TAlog1~Treatment, data=Water_Set1) #not normal
bartlett.test(TAlog1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(TAlog1~Treatment, data=Water_Set1)
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
#similar to untransformed variable

leveneTest(TAsqrt1~Treatment, data=Water_Set1) #not normal
bartlett.test(TAsqrt1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(TAsqrt1~Treatment, data=Water_Set1)
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
##similar to untransformed variable 

#cube
leveneTest(TAcube1~Treatment, data=Water_Set1) #not normal
bartlett.test(TAcube1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(TAcube1~Treatment, data=Water_Set1)
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
##similar to untransformed variable 


###pCO2
hist(trial$pCO2)
plotNormalHistogram(trial$pCO2)
leveneTest(pCO2~Site, data=trial) #normal
bartlett.test(pCO2~Site, data=trial) #not normal
##QQplots
fit<-lm(pCO2~Site, data=trial)
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

pCO2log = log(trial$pCO2) #log transform
plotNormalHistogram(pCO2log) ##could use in analyses to test how it affects the untransformed variable
pCO2sqrt = sqrt(trial$pCO2) #square root transform
plotNormalHistogram(pCO2sqrt) 
pCO2cube= sign(trial$pCO2) * abs(trial$pCO2)^(1/3) #cube root transform
plotNormalHistogram(pCO2cube) 
pCO2recp = 1/(trial$pCO2) #reciprocal transform
plotNormalHistogram(pCO2recp) #could use in analyses to test how it affects the untransformed variable

trial = cbind(trial, pCO2log, pCO2cube, pCO2recp)

leveneTest(pCO2log~Site, data=trial) #normal
bartlett.test(pCO2log~Site, data=trial) #normal 
##QQplots
fit<-lm(pCO2log~Site, data=trial)
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

leveneTest(pCO2cube~Site, data=trial) #normal
bartlett.test(pCO2cube~Site, data=trial) #normal 
##QQplots
fit<-lm(pCO2cube~Site, data=trial)
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

leveneTest(pCO2recp~Site, data=trial) #normal
bartlett.test(pCO2recp~Site, data=trial) #normal 
##QQplots
fit<-lm(pCO2recp~Site, data=trial)
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
##Not enough justification to transform - NO TRANSFORM



####Omega Ca
hist(trial$Omega_Ca)
plotNormalHistogram(trial$Omega_Ca)
leveneTest(Omega_Ca~Site, data=trial) #normal
bartlett.test(Omega_Ar~Site, data=trial) #not normal
##QQplots
fit<-lm(Omega_Ca~Site, data=trial)
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

Calog = log(trial$Omega_Ca) #log transform
plotNormalHistogram(Calog) ##could use in analyses to test how it affects the untransformed variable
Casqrt = sqrt(trial$Omega_Ca) #square root transform
plotNormalHistogram(Casqrt) #could use in analyses to test how it affects the untransformed variable
Cacube= sign(trial$Omega_Ca) * abs(trial$Omega_Ca)^(1/3) #cube root transform
plotNormalHistogram(Cacube) #could use in analyses to test how it affects the untransformed variable
Carecp = 1/(trial$Omega_Ca) #reciprocal transform
plotNormalHistogram(Carecp) 

trial = cbind(trial, Calog, Casqrt, Cacube)

leveneTest(Calog~Site, data=trial) #normal
bartlett.test(Calog~Site, data=trial) #not normal 
##QQplots
fit<-lm(Calog~Site, data=trial)
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

leveneTest(Casqrt~Site, data=trial) #normal
bartlett.test(Casqrt~Site, data=trial) #not normal 
##QQplots
fit<-lm(Casqrt~Site, data=trial)
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

leveneTest(Cacube~Site, data=trial) #normal
bartlett.test(Cacube~Site, data=trial) #not normal 
##QQplots
fit<-lm(Cacube~Site, data=trial)
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









###random notes below

##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal

###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal

###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  



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

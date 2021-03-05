##tests of normality for water quality--experiment for HT - UPDATED
##2/11/20

##Set 1 exp WQ
Water_Set1 = read.csv("WaterQuality_Experiment_Set1_HT_UPDATED_021020.csv", head = T)
Low_Set1 = Water_Set1[1:224,]
Amb_Set1 = Water_Set1[225:448,]
Water_Set1$Site=as.factor(Water_Set1$Site)
Water_Set1$Treatment=as.factor(Water_Set1$Treatment)
library(car)
library(rcompanion)

##Temperature - set 1
hist(Water_Set1$Temp)
plotNormalHistogram(Water_Set1$Temp)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(Temp~Treatment, data=Water_Set1) #not normal 
###########test 2: Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(Temp~Treatment, data=Water_Set1) #not normal 
##QQPLOTS
fit<-lm(Temp~Treatment, data=Water_Set1)
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

#transforming the variable using log, sqrt, cube, and reciprocal 
t_log1 = log(Water_Set1$Temp)
plotNormalHistogram(t_log1)
t_sqrt1 = sqrt(Water_Set1$Temp)
plotNormalHistogram(t_sqrt1) #could use in analyses to test how it affects the untransformed temperature variable
t_cube1 = sign(Water_Set1$Temp) * abs(Water_Set1$Temp)^(1/3)
plotNormalHistogram(t_cube1) #could use in analyses to test how it affects the untransformed temperature variable
t_recp1 = 1/(Water_Set1$Temp)
plotNormalHistogram(t_recp1)

#binding the transformed variables into the data set to potentially be used 
Water_Set1 = cbind(Water_Set1, t_sqrt1, t_cube1)

#sqrt temp analyses
leveneTest(t_sqrt1~Treatment, data=Water_Set1) #not normal
bartlett.test(t_sqrt1~Treatment, data=Water_Set1) #not normal 
fit<-lm(t_sqrt1~Treatment, data=Water_Set1)
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
#similar looking to the untransformed variable 

#cube temp analyses
leveneTest(t_cube1~Treatment, data=Water_Set1) #not normal
bartlett.test(t_cube1~Treatment, data=Water_Set1) #not normal 
fit<-lm(t_cube1~Treatment, data=Water_Set1)
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
##but there is no indication of normality - may have to use the Dunn test 


##Salinity 
hist(Water_Set1$Sal)
plotNormalHistogram(Water_Set1$Sal)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(Sal~Treatment, data=Water_Set1) #not normal 
###########test 2: Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(Sal~Treatment, data=Water_Set1) #not normal 
##QQPLOTS
fit<-lm(Sal~Treatment, data=Water_Set1)
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

#transforming variable to see if I can achieve normality 
Slog1 = log(Water_Set1$Sal) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(Slog1)
Ssqrt1 = sqrt(Water_Set1$Sal)
plotNormalHistogram(Ssqrt1)
Scube1= sign(Water_Set1$Sal) * abs(Water_Set1$Sal)^(1/3)
plotNormalHistogram(Scube1)
Srecp1 = 1/(Water_Set1$Sal) ##could use 
plotNormalHistogram(Srecp1)

##Binding transformed variables into dataset 
Water_Set1 = cbind(Water_Set1, Slog1, Srecp1)

#log sal analyses
leveneTest(Slog1~Treatment, data=Water_Set1) #not normal
bartlett.test(Slog1~Treatment, data=Water_Set1) #not normal 
fit<-lm(Slog1~Treatment, data=Water_Set1)
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
#similar looking to the untransformed variable, but could maybe use? 

#recp sal analyses
leveneTest(Srecp1~Treatment, data=Water_Set1) #not normal
bartlett.test(Srecp1~Treatment, data=Water_Set1) #not normal 
fit<-lm(Srecp1~Treatment, data=Water_Set1)
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
##wont be used 

##None are relevant enough to transform the data - NOT TRANSFORMED
##but there is no indication of normality - may have to use the Dunn test 


##DO
#for some reason, DO variable was read as a factor, so had to convert it to numeric variable 
Water_Set1$DO_mg = as.numeric(Water_Set1$DO_mg)
hist(Water_Set1$DO_mg)
plotNormalHistogram(Water_Set1$DO_mg)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(DO_mg~Treatment, data=Water_Set1) #not normal 
###########test 2: Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(DO_mg~Treatment, data=Water_Set1) #not normal 
##QQPLOTS
fit<-lm(DO_mg~Treatment, data=Water_Set1)
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

#transforming variable
DOlog1 = log(Water_Set1$DO_mg) #no
plotNormalHistogram(DOlog1)
DOsqrt1 = sqrt(Water_Set1$DO_mg) #square root transform
plotNormalHistogram(DOsqrt1) 
DOcube1 = sign(Water_Set1$DO_mg) * abs(Water_Set1$DO_mg)^(1/3) #cube root transform
plotNormalHistogram(DOcube1) 
DOrecp1 = 1/(Water_Set1$DO_mg) #reciprocal transform
plotNormalHistogram(DOrecp1)
##try sqrt and cube

Water_Set1 = cbind(Water_Set1, DOsqrt1, DOcube1)

#sqrt DO analyses
leveneTest(DOsqrt1~Treatment, data=Water_Set1) #not normal
bartlett.test(DOsqrt1~Treatment, data=Water_Set1) #not normal 
fit<-lm(DOsqrt1~Treatment, data=Water_Set1)
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
#qqplot looks worse 

#cube DO analyses
leveneTest(DOcube1~Treatment, data=Water_Set1) #not normal
bartlett.test(DOcube1~Treatment, data=Water_Set1) #not normal 
fit<-lm(DOcube1~Treatment, data=Water_Set1)
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
#no

##DO variable and transformed variables don't look too good 



##Spec pH
hist(Water_Set1$spec_pH)
plotNormalHistogram(Water_Set1$spec_pH)
leveneTest(spec_pH~Water_Set1$Treatment, data=Water_Set1) #normal
bartlett.test(spec_pH~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(spec_pH~Treatment, data=Water_Set1)
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
##Looks a bit wacky 

#transforming spec pH variable
pHlog1 = log(Water_Set1$spec_pH) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(pHlog1)
pHsqrt1 = sqrt(Water_Set1$spec_pH) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(pHsqrt1)
pHcube1= sign(Water_Set1$spec_pH) * abs(Water_Set1$spec_pH)^(1/3) ##could use in analyses to test how it affects the untransformed variable
plotNormalHistogram(pHcube1)
pHrecp1 = 1/(Water_Set1$spec_pH) 
plotNormalHistogram(pHrecp1)

Water_Set1 = cbind(Water_Set1, pHlog1, pHsqrt1, pHcube1)

#spec pH log transform testing 
leveneTest(pHlog1~Treatment, data=Water_Set1) #normal
bartlett.test(pHlog1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(pHlog1~Treatment, data=Water_Set1)
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
##looks similar to the untransformed variable qqplot 

##spec pH sqrt transform testing
leveneTest(pHsqrt1~Treatment, data=Water_Set1) #normal
bartlett.test(pHsqrt1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(pHsqrt1~Treatment, data=Water_Set1)
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
##plot looks similar to untransformed variable

##Cube transform testing 
leveneTest(pHcube1~Treatment, data=Water_Set1) #normal
bartlett.test(pHcube1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(pHcube1~Treatment, data=Water_Set1)
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
##similar to untransformed 

##TA
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


#pCO2
hist(Water_Set1$pCO2)
plotNormalHistogram(Water_Set1$pCO2)
leveneTest(pCO2~Treatment, data=Water_Set1) #normal
bartlett.test(pCO2~Treatment, data=Water_Set1) #not normal
##QQplots
fit<-lm(pCO2~Treatment, data=Water_Set1)
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
##goes up off the line on right hand side 

pCO2log1 = log(Water_Set1$pCO2) #log transform
plotNormalHistogram(pCO2log1) 
pCO2sqrt1 = sqrt(Water_Set1$pCO2) #square root transform
plotNormalHistogram(pCO2sqrt1) 
pCO2cube1 = sign(Water_Set1$pCO2) * abs(Water_Set1$pCO2)^(1/3) #cube root transform
plotNormalHistogram(pCO2cube1) 
pCO2recp1 = 1/(Water_Set1$pCO2) #reciprocal transform
plotNormalHistogram(pCO2recp1) 

Water_Set1 = cbind(Water_Set1, pCO2log1, pCO2recp1)

#log transform
leveneTest(pCO2log1~Treatment, data=Water_Set1) #normal
bartlett.test(pCO2log1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(pCO2log1~Treatment, data=Water_Set1)
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
##similar to untransformed variable, but worse qqplot

#recp transform
leveneTest(pCO2recp1~Treatment, data=Water_Set1) #not normal
bartlett.test(pCO2recp1~Treatment, data=Water_Set1) #not normal 
##QQplots
fit<-lm(pCO2recp1~Treatment, data=Water_Set1)
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
##no good for this one

##Omega Ca
hist(Water_Set1$OmegaCa)
plotNormalHistogram(Water_Set1$OmegaCa)
leveneTest(OmegaCa~Treatment, data=Water_Set1) #not normal
bartlett.test(OmegaCa~Treatment, data=Water_Set1) #not normal
##QQplots
fit<-lm(OmegaCa~Treatment, data=Water_Set1)
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
##pretty weird looking 

#transformations
Calog1 = log(Water_Set1$OmegaCa) #log transform
plotNormalHistogram(Calog1) #use
Casqrt1 = sqrt(Water_Set1$OmegaCa) #square root transform
plotNormalHistogram(Casqrt1)
Cacube1= sign(Water_Set1$OmegaCa) * abs(Water_Set1$OmegaCa)^(1/3) #cube root transform
plotNormalHistogram(Cacube1) #use 
Carecp1 = 1/(Water_Set1$OmegaCa) #reciprocal transform
plotNormalHistogram(Carecp1) 

Water_Set1 = cbind(Water_Set1, Calog1, Cacube1)

##log transform
leveneTest(Calog1~Treatment, data=Water_Set1) #not normal
bartlett.test(Calog1~Treatment, data=Water_Set1) #not normal
##QQplots
fit<-lm(Calog1~Treatment, data=Water_Set1)
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
#no good 

##cube transform
leveneTest(Cacube1~Treatment, data=Water_Set1) #not normal
bartlett.test(Cacube1~Treatment, data=Water_Set1) #not normal
##QQplots
fit<-lm(Cacube1~Treatment, data=Water_Set1)
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
#also not great 





#############################################################################################3


Water_Set2 = read.csv("WaterQuality_Experiment_Set2_HT_UPDATED_021020.csv", head = T)
library(car)

Low_Set1 = Water_Set2[1:224,]
Amb_Set1 = Water_Set2[225:448,]
##Change "Total" to column header in data file for dataset you want to analyze
##############################################

##Temperature 
hist(Water_Set2$Temp)
plotNormalHistogram(Water_Set2$Temp)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(Temp~Treatment, data=Water_Set2) #not normal 
bartlett.test(Temp~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(Temp~Treatment, data=Water_Set2)
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
##not great 

#transforming the variable using log, sqrt, cube, and reciprocal 
t_log2 = log(Water_Set2$Temp)
plotNormalHistogram(t_log2)
t_sqrt2 = sqrt(Water_Set2$Temp)
plotNormalHistogram(t_sqrt2) #could use in analyses to test how it affects the untransformed temperature variable
t_cube2 = sign(Water_Set2$Temp) * abs(Water_Set2$Temp)^(1/3)
plotNormalHistogram(t_cube2) #could use in analyses to test how it affects the untransformed temperature variable
t_recp2 = 1/(Water_Set2$Temp)
plotNormalHistogram(t_recp2)

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, t_log2, t_sqrt2, t_cube2, t_recp2)

##log transform
leveneTest(t_log2~Treatment, data=Water_Set2) #not normal
bartlett.test(t_log2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(t_log2~Treatment, data=Water_Set2)
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
##no good

##sqrt transform
leveneTest(t_sqrt2~Treatment, data=Water_Set2) #not normal
bartlett.test(t_sqrt2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(t_sqrt2~Treatment, data=Water_Set2)
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
##also no good 

##cube transform
leveneTest(t_cube2~Treatment, data=Water_Set2) #not normal
bartlett.test(t_cube2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(t_cube2~Treatment, data=Water_Set2)
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
##no good 

##recp transform
leveneTest(t_recp2~Treatment, data=Water_Set2) #not normal
bartlett.test(t_recp2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(t_recp2~Treatment, data=Water_Set1)
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
##bad - may just do the K-w test 



##Salinity 
hist(Water_Set2$Sal)
plotNormalHistogram(Water_Set2$Sal)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(Sal~Treatment, data=Water_Set2) #not normal 
bartlett.test(Sal~Treatment, data=Water_Set2) #normal 
##QQplots 
fit<-lm(Sal~Treatment, data=Water_Set2)
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
##eh not good

#transforming the variable using log, sqrt, cube, and reciprocal 
s_log2 = log(Water_Set2$Sal)
plotNormalHistogram(s_log2)
s_sqrt2 = sqrt(Water_Set2$Sal)
plotNormalHistogram(s_sqrt2) #could use in analyses to test how it affects the untransformed temperature variable
s_cube2 = sign(Water_Set2$Sal) * abs(Water_Set2$Sal)^(1/3)
plotNormalHistogram(s_cube2) #could use in analyses to test how it affects the untransformed temperature variable
s_recp2 = 1/(Water_Set2$Sal)
plotNormalHistogram(s_recp2)
##use log and cube

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, s_log2, s_cube2)

##log transform
leveneTest(s_log2~Treatment, data=Water_Set2) #not normal
bartlett.test(s_log2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(s_log2~Treatment, data=Water_Set2)
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
##not good

##cube transform
leveneTest(s_cube2~Treatment, data=Water_Set2) #not normal
bartlett.test(s_cube2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(s_cube2~Treatment, data=Water_Set2)
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
##not great 




##DO mg 
hist(Water_Set2$DO_mg)
plotNormalHistogram(Water_Set2$DO_mg)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(DO_mg~Treatment, data=Water_Set2) #normal 
bartlett.test(DO_mg~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(DO_mg~Treatment, data=Water_Set2)
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
##not bad

#transforming the variable using log, sqrt, cube, and reciprocal 
do_log2 = log(Water_Set2$DO_mg)
plotNormalHistogram(do_log2)
do_sqrt2 = sqrt(Water_Set2$DO_mg)
plotNormalHistogram(do_sqrt2) #could use in analyses to test how it affects the untransformed temperature variable
do_cube2 = sign(Water_Set2$DO_mg) * abs(Water_Set2$DO_mg)^(1/3)
plotNormalHistogram(do_cube2) #could use in analyses to test how it affects the untransformed temperature variable
do_recp2 = 1/(Water_Set2$DO_mg)
plotNormalHistogram(do_recp2)
##use log and cube

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, do_log2, do_sqrt2, do_cube2, do_recp2)

##log transform
leveneTest(do_log2~Treatment, data=Water_Set2) #normal
bartlett.test(do_log2~Treatment, data=Water_Set2) #normal
##QQplots
fit<-lm(do_log2~Treatment, data=Water_Set2)
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

##sqrt transform
leveneTest(do_sqrt2~Treatment, data=Water_Set2) #normal
bartlett.test(do_sqrt2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(do_sqrt2~Treatment, data=Water_Set2)
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
#Just okay

##cube transform
leveneTest(do_cube2~Treatment, data=Water_Set2) #normal
bartlett.test(do_cube2~Treatment, data=Water_Set2) #not normal
##QQplots
fit<-lm(do_cube2~Treatment, data=Water_Set2)
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
##eh

##recp
leveneTest(do_recp2~Treatment, data=Water_Set2) #normal
bartlett.test(do_recp2~Treatment, data=Water_Set2) #normal
##QQplots
fit<-lm(do_recp2~Treatment, data=Water_Set2)
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
##pretty good 



##Spec pH
hist(Water_Set2$spec_pH)
plotNormalHistogram(Water_Set2$spec_pH)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(spec_pH~Treatment, data=Water_Set2) #normal 
bartlett.test(spec_pH~Treatment, data=Water_Set2) #normal 
##QQplots 
fit<-lm(spec_pH~Treatment, data=Water_Set2)
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
##not bad

#transforming the variable using log, sqrt, cube, and reciprocal 
pH_log2 = log(Water_Set2$spec_pH)
plotNormalHistogram(pH_log2)
pH_sqrt2 = sqrt(Water_Set2$spec_pH)
plotNormalHistogram(pH_sqrt2) #could use in analyses to test how it affects the untransformed temperature variable
pH_cube2 = sign(Water_Set2$spec_pH) * abs(Water_Set2$spec_pH)^(1/3)
plotNormalHistogram(pH_cube2) #could use in analyses to test how it affects the untransformed temperature variable
pH_recp2 = 1/(Water_Set2$spec_pH)
plotNormalHistogram(pH_recp2)
#use log and sqrt

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, pH_log2, pH_sqrt2)

##pH log
leveneTest(pH_log2~Treatment, data=Water_Set2) #normal 
bartlett.test(pH_log2~Treatment, data=Water_Set2) #normal 
##QQplots 
fit<-lm(pH_log2~Treatment, data=Water_Set2)
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

##pH sqrt
leveneTest(pH_sqrt2~Treatment, data=Water_Set2) #normal 
bartlett.test(pH_sqrt2~Treatment, data=Water_Set2) #normal 
##QQplots 
fit<-lm(pH_sqrt2~Treatment, data=Water_Set2)
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



##TA 
hist(Water_Set2$TA)
plotNormalHistogram(Water_Set2$TA)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(TA~Treatment, data=Water_Set2) #not normal 
bartlett.test(TA~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(TA~Treatment, data=Water_Set2)
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
##step-like pattern

#transforming the variable using log, sqrt, cube, and reciprocal 
TA_log2 = log(Water_Set2$TA)
plotNormalHistogram(TA_log2)
TA_sqrt2 = sqrt(Water_Set2$TA)
plotNormalHistogram(TA_sqrt2) #could use in analyses to test how it affects the untransformed temperature variable
TA_cube2 = sign(Water_Set2$TA) * abs(Water_Set2$TA)^(1/3)
plotNormalHistogram(TA_cube2) #could use in analyses to test how it affects the untransformed temperature variable
TA_recp2 = 1/(Water_Set2$TA)
plotNormalHistogram(TA_recp2)
#use log and sqrt

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, TA_log2, TA_sqrt2, TA_cube2, TA_recp2)

##TA log
leveneTest(TA_log2~Treatment, data=Water_Set2) #not normal 
bartlett.test(TA_log2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(TA_log2~Treatment, data=Water_Set2)
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
##similar to original variable 

##TA sqrt
leveneTest(TA_sqrt2~Treatment, data=Water_Set2) #not normal 
bartlett.test(TA_sqrt2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(TA_sqrt2~Treatment, data=Water_Set2)
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
##similar to original variable 

##TA cube
leveneTest(TA_cube2~Treatment, data=Water_Set2) #not normal 
bartlett.test(TA_cube2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(TA_cube2~Treatment, data=Water_Set2)
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
##similar to original variable 

##TA recp
leveneTest(TA_recp2~Treatment, data=Water_Set2) #normal 
bartlett.test(TA_recp2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(TA_recp2~Treatment, data=Water_Set2)
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
##kinda similar to original just shifted down a little...





##pCO2 
hist(Water_Set2$pCO2)
plotNormalHistogram(Water_Set2$pCO2)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(pCO2~Treatment, data=Water_Set2) #normal 
bartlett.test(pCO2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(pCO2~Treatment, data=Water_Set2)
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

#transforming the variable using log, sqrt, cube, and reciprocal 
CO_log2 = log(Water_Set2$pCO2)
plotNormalHistogram(CO_log2)
CO_sqrt2 = sqrt(Water_Set2$pCO2)
plotNormalHistogram(CO_sqrt2) 
CO_cube2 = sign(Water_Set2$pCO2) * abs(Water_Set2$pCO2)^(1/3)
plotNormalHistogram(CO_cube2)
CO_recp2 = 1/(Water_Set2$pCO2)
plotNormalHistogram(CO_recp2)
##use log, sqrt, recp

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, CO_log2, CO_sqrt2, CO_recp2)

#log pCO2
leveneTest(CO_log2~Treatment, data=Water_Set2) #normal 
bartlett.test(CO_log2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(CO_log2~Treatment, data=Water_Set2)
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
#similar to original variable 

#sqrt
leveneTest(CO_sqrt2~Treatment, data=Water_Set2) #normal 
bartlett.test(CO_sqrt2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(CO_sqrt2~Treatment, data=Water_Set2)
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
#similar to original variable 

##recp
leveneTest(CO_recp2~Treatment, data=Water_Set2) #normal 
bartlett.test(CO_recp2~Treatment, data=Water_Set2) #normal 
##QQplots 
fit<-lm(CO_recp2~Treatment, data=Water_Set2)
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



##Omega Ca
hist(Water_Set2$OmegaCa)
plotNormalHistogram(Water_Set2$OmegaCa)
##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(OmegaCa~Treatment, data=Water_Set2) #not normal 
bartlett.test(OmegaCa~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(OmegaCa~Treatment, data=Water_Set2)
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
##not great

#transforming the variable using log, sqrt, cube, and reciprocal 
Ca_log2 = log(Water_Set2$OmegaCa)
plotNormalHistogram(Ca_log2)
Ca_sqrt2 = sqrt(Water_Set2$OmegaCa)
plotNormalHistogram(Ca_sqrt2) 
Ca_cube2 = sign(Water_Set2$OmegaCa) * abs(Water_Set2$OmegaCa)^(1/3)
plotNormalHistogram(Ca_cube2)
Ca_recp2 = 1/(Water_Set2$OmegaCa)
plotNormalHistogram(Ca_recp2)
##use log, sqrt, cube, recp

#binding the transformed variables into the data set to potentially be used 
Water_Set2 = cbind(Water_Set2, Ca_log2, Ca_sqrt2, Ca_cube2, Ca_recp2)

#log omega Ca
leveneTest(Ca_log2~Treatment, data=Water_Set2) #not normal 
bartlett.test(Ca_log2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(Ca_log2~Treatment, data=Water_Set2)
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
#similar to original variable - not great 

#sqrt omega Ca
leveneTest(Ca_sqrt2~Treatment, data=Water_Set2) #not normal 
bartlett.test(Ca_sqrt2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(Ca_sqrt2~Treatment, data=Water_Set2)
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
#similar to original variable 

#cube Omega Ca
leveneTest(Ca_cube2~Treatment, data=Water_Set2) #not normal 
bartlett.test(Ca_cube2~Treatment, data=Water_Set2) #not normal 
##QQplots 
fit<-lm(Ca_cube2~Treatment, data=Water_Set2)
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
#similar to original variable 

#recp Omega Ca
leveneTest(Ca_recp2~Treatment, data=Water_Set2) #not normal 
bartlett.test(Ca_recp2~Treatment, data=Water_Set2) #normal 
##QQplots 
fit<-lm(Ca_recp2~Treatment, data=Water_Set2)
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
#not as bad ! 














##########test 1: Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
#Water Quality--Set1
leveneTest(DO_mg~Treatment, data=Water_Set1) #normal for % glycogen of DW
leveneTest(Temp~Treatment, data=Water_Set1) #not normal 
leveneTest(DO_P~Treatment, data=Water_Set1) #normal
leveneTest(Sal~Treatment, data=Water_Set1) #not normal
leveneTest(Sal~Treatment, data=Water_Set1) #not normal
leveneTest(Cond~Treatment, data=Water_Set1) #not normal
leveneTest(pH~Treatment, data=Water_Set1) #not normal

#how to omit and analyze NAs?
leveneTest(na.omit(TA)~Treatment, data=Water_Set1) #not normal
leveneTest(pCO2~Treatment, data=Water_Set1) #not normal

#Water Quality--Set2
leveneTest(DO_mg~Treatment, data=Water_Set2) #normal 
leveneTest(Temp~Treatment, data=Water_Set2) #not normal 
leveneTest(DO_P~Treatment, data=Water_Set2) #not normal
leveneTest(Sal~Treatment, data=Water_Set2) #not normal
leveneTest(Cond~Treatment, data=Water_Set2) #not normal
leveneTest(pH~Treatment, data=Water_Set2) #normal
leveneTest(TA~Treatment, data=Water_Set2) #not normal
leveneTest(pCO2~Treatment, data=Water_Set2) #not normal

###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(tTotalPLps~interaction(CO2,Temp), data=trial)

#water quality --set1
bartlett.test(DO_mg~Treatment, data=Water_Set1) # not normal
bartlett.test(Temp~Treatment, data=Water_Set1) #not normal 
bartlett.test(DO_P~Treatment, data=Water_Set1) #not normal
bartlett.test(Sal~Treatment, data=Water_Set1) #not normal
bartlett.test(Cond~Treatment, data=Water_Set1) #not normal
bartlett.test(pH~Treatment, data=Water_Set1) #not normal
bartlett.test(TA~Treatment, data=Water_Set1) #not normal
bartlett.test(pCO2~Treatment, data=Water_Set1) #not normal

#water quality --set2
bartlett.test(DO_mg~Treatment, data=Water_Set2)# not normal
bartlett.test(Temp~Treatment, data=Water_Set2) #not normal 
bartlett.test(DO_P~Treatment, data=Water_Set2) #not normal
bartlett.test(Sal~Treatment, data=Water_Set2) #normal
bartlett.test(Cond~Treatment, data=Water_Set2) #not normal
bartlett.test(pH~Treatment, data=Water_Set2) #not normal
bartlett.test(TA~Treatment, data=Water_Set2) #not normal
bartlett.test(pCO2~Treatment, data=Water_Set2) #not normal


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

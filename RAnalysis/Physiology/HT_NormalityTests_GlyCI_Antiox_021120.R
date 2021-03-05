set1<-read.csv("HT_CIGly_Set1.csv", head=T)
library(car)
library(rcompanion)


####Glycogen
hist(set1$P_Glycogen_DW)
plotNormalHistogram(set1$P_Glycogen_DW)
##########Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(P_Glycogen_DW~Site, data=set1) #normal
###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(P_Glycogen_DW~Site, data=set1) #normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(P_Glycogen_DW~Site, data=set1)
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
g_log = log(set1$P_Glycogen_DW)
plotNormalHistogram(g_log)
g_sqrt = sqrt(set1$P_Glycogen_DW)
plotNormalHistogram(g_sqrt) 
g_cube= sign(set1$P_Glycogen_DW) * abs(set1$P_Glycogen_DW)^(1/3)
plotNormalHistogram(g_cube) 
g_recp = 1/(set1$P_Glycogen_DW)
plotNormalHistogram(g_recp)
#log, sqrt, cube

#binding the transformed variables into the data set 
set1 = cbind(set1, g_log, g_sqrt, g_cube)

##glycogen log 
leveneTest(g_log~Site, data=set1) #normal
fit<-lm(g_log~Site, data=set1)
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

#glycogen sqrt 
leveneTest(g_sqrt~Site, data=set1) #normal
fit<-lm(g_sqrt~Site, data=set1)
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

#glycogen cube 
leveneTest(g_cube~Site, data=set1) #normal
fit<-lm(g_cube~Site, data=set1)
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

#could use normal, log, sqrt, cube


##Condition Index
hist(set1$ConditionIndex)
plotNormalHistogram(set1$ConditionIndex)
##########Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(ConditionIndex~Site, data=set1) #normal
###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(ConditionIndex~Site, data=set1) #normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(ConditionIndex~Site, data=set1)
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
#I think this needs no transformations! :D



###Set 2
set2<-read.csv("HT_CIGly_Set2.csv", head=T)


###Glycogen
hist(set2$P_Glycogen_DW)
plotNormalHistogram(set2$P_Glycogen_DW)
##########Levene Test
###this is more robust to departures from normality than Bartlett's test. 
###p>0.05 means the variances are equal
leveneTest(P_Glycogen_DW~Site, data=set2) #normal
###########Bartlett test
###If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
###p>0.05 means the variances are equal
bartlett.test(P_Glycogen_DW~Site, data=set2) #not normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(P_Glycogen_DW~Site, data=set2)
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
g_log = log(set2$P_Glycogen_DW)
plotNormalHistogram(g_log)
g_sqrt = sqrt(set2$P_Glycogen_DW)
plotNormalHistogram(g_sqrt) 
g_cube= sign(set2$P_Glycogen_DW) * abs(set2$P_Glycogen_DW)^(1/3)
plotNormalHistogram(g_cube) 
g_recp = 1/(set2$P_Glycogen_DW)
plotNormalHistogram(g_recp)
#try all 

#binding the transformed variables into the data set 
set2 = cbind(set2, g_log, g_sqrt, g_cube, g_recp)

##glycogen log 
leveneTest(g_log~Site, data=set2) #normal
fit<-lm(g_log~Site, data=set2)
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

#glycogen sqrt 
leveneTest(g_sqrt~Site, data=set2) #normal
fit<-lm(g_sqrt~Site, data=set2)
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

#glycogen cube 
leveneTest(g_cube~Site, data=set2) #normal
fit<-lm(g_cube~Site, data=set2)
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

#glycogen recp
leveneTest(g_recp~Site, data=set2) #normal
fit<-lm(g_recp~Site, data=set2)
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
#maybe the best one?

##try taking the outlier out 
set2.new<-set2[-39,]

##glycogen without oulier from VP
plotNormalHistogram(set2.new$P_Glycogen_DW)
leveneTest(P_Glycogen_DW~Site, data=set2.new) #normal
fit<-lm(P_Glycogen_DW~Site, data=set2.new)
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
###need to check what viable glycogen values are 


###Delta Glycogen 
hist(set2$P_ChangeGly_DW)
plotNormalHistogram(set2$P_ChangeGly_DW)
##########Levene Test
leveneTest(P_ChangeGly_DW~Site, data=set2) #normal
###########Bartlett test
bartlett.test(P_ChangeGly_DW~Site, data=set2) #not normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(P_ChangeGly_DW~Site, data=set2)
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

#transforming the variable using cube and reciprocal 
##can't use log or sqrt bc it is negative 
g_cube2= sign(set2$P_ChangeGly_DW) * abs(set2$P_ChangeGly_DW)^(1/3)
plotNormalHistogram(g_cube) 
g_recp2 = 1/(set2$P_ChangeGly_DW)
plotNormalHistogram(g_recp)
#try all 

#binding the transformed variables into the data set 
set2 = cbind(set2, g_cube2, g_recp2)

#glycogen cube 
leveneTest(g_cube2~Site, data=set2) #normal
fit<-lm(g_cube2~Site, data=set2)
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
#doesn't look too good 

#glycogen recp
leveneTest(g_recp2~Site, data=set2) #normal
fit<-lm(g_recp2~Site, data=set2)
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

##glycogen without oulier from VP
plotNormalHistogram(set2.new$P_ChangeGly_DW)
leveneTest(P_ChangeGly_DW~Site, data=set2.new) #normal
fit<-lm(P_Glycogen_DW~Site, data=set2.new)
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



##CI
hist(set2$ConditionIndex)
plotNormalHistogram(set2$ConditionIndex)
##########Levene Test
leveneTest(ConditionIndex~Site, data=set2) #normal
###########Bartlett test
bartlett.test(ConditionIndex~Site, data=set2) #normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(ConditionIndex~Site, data=set2)
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

#try CI transformations - log, sqrt, cube, recp
CI_log = log(set2$ConditionIndex)
plotNormalHistogram(CI_log)
CI_sqrt = sqrt(set2$ConditionIndex)
plotNormalHistogram(CI_sqrt) 
CI_cube= sign(set2$ConditionIndex) * abs(set2$ConditionIndex)^(1/3)
plotNormalHistogram(CI_cube) 
CI_recp = 1/(set2$ConditionIndex)
plotNormalHistogram(CI_recp)
#try with log and recp

#binding the transformed variables into the data set 
set2 = cbind(set2, CI_log, CI_recp)

#Log CI - set 2
leveneTest(CI_log~Site, data=set2) #normal
fit<-lm(CI_log~Site, data=set2)
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

#Recp CI - set 2
leveneTest(CI_recp~Site, data=set2) #normal
fit<-lm(CI_recp~Site, data=set2)
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



##Delta CI
hist(set2$Delta_CI)
plotNormalHistogram(set2$Delta_CI)
##########Levene Test
leveneTest(Delta_CI~Site, data=set2) #normal
###########Bartlett test
bartlett.test(Delta_CI~Site, data=set2) #normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(Delta_CI~Site, data=set2)
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

#try CI transformations - log, sqrt, cube, recp
DeltaCI_cube= sign(set2$Delta_CI) * abs(set2$Delta_CI)^(1/3)
plotNormalHistogram(DeltaCI_cube) 
DeltaCI_recp = 1/(set2$Delta_CI)
plotNormalHistogram(DeltaCI_recp)
#maybe recp

set2 = cbind(set2, DeltaCI_recp)

#Recp Delta CI - set 2
leveneTest(DeltaCI_recp~Site, data=set2) #not normal
fit<-lm(DeltaCI_recp~Site, data=set2)
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
#nah don't look so good 



















##########################################

###Total Antiox for set 1 and 2 - evaluating normality 
##Because there was a potential interaction effect (site and treatment), need to evaluate these variables differently than those above 

###Set 1
frap<-read.csv("FRAP_HT.csv", head=T)

frap1 = frap[1:56,]
frap2 = frap[57:112,]

####Set 1 for FRAP
##FRAP - total antiox

hist(frap1$FRAP)
plotNormalHistogram(frap1$FRAP)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(FRAP ~interaction(Treatment, Site), data=frap1) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(FRAP ~interaction(Treatment, Site), data=frap1) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(FRAP ~interaction(Treatment, Site), data=frap1)
par(mfrow=c(2,2))
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))
qqnorm(rstandard(fit), ylab="Residuals")
qqline(rstandard(fit))
plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit), abs(residuals(fit)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")
##Have an outlier - MR-1-02-A --> will see how results come out with and without outlier 

##Without outlier
frapout = frap1[-2,]
hist(frapout$FRAP)
plotNormalHistogram(frapout$FRAP)
##########Levene Test
leveneTest(FRAP ~interaction(Treatment, Site), data=frapout) #normal
####Bartlett test
bartlett.test(FRAP ~interaction(Treatment, Site), data=frapout) #not normal
fit<-lm(FRAP ~interaction(Treatment, Site), data=frapout)
par(mfrow=c(2,2))
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))
qqnorm(rstandard(fit), ylab="Residuals")
qqline(rstandard(fit))
plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit), abs(residuals(fit)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")
##Looks like a better line, but not sure which to use in my analyses 

#try FRAP transformations - log, sqrt, cube, recp
F1_log = log(frap1$FRAP)
plotNormalHistogram(F1_log)
F1_sqrt = sqrt(frap1$FRAP)
plotNormalHistogram(F1_sqrt) 
F1_cube= sign(frap1$FRAP) * abs(frap1$FRAP)^(1/3)
plotNormalHistogram(F1_cube) 
F1_recp = 1/(frap1$FRAP)
plotNormalHistogram(F1_recp)

#try with log and recp
#may not need to ditch the outlier--might just need to use the transformed variable for analyses 

#binding the transformed variables into the data set 
frap1 = cbind(frap1, F1_log, F1_recp)

##Evaluating transformed variables for normality 
##FRAP - log
#Levene Test - this is more robust to departures from normality than Bartlett's test. 
leveneTest(F1_log ~interaction(Treatment, Site), data=frap1) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
bartlett.test(F1_log ~interaction(Treatment, Site), data=frap1) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(F1_log ~interaction(Treatment, Site), data=frap1)
par(mfrow=c(2,2))
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))
qqnorm(rstandard(fit), ylab="Residuals")
qqline(rstandard(fit))
plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit), abs(residuals(fit)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")

##FRAP - recp
#Levene Test - this is more robust to departures from normality than Bartlett's test. 
leveneTest(F1_recp ~interaction(Treatment, Site), data=frap1) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
bartlett.test(F1_recp ~interaction(Treatment, Site), data=frap1) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(F1_recp ~interaction(Treatment, Site), data=frap1)
par(mfrow=c(2,2))
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))
qqnorm(rstandard(fit), ylab="Residuals")
qqline(rstandard(fit))
plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit), abs(residuals(fit)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")
##All qqplots look similar to the untransformed variable 






################################################################################################################
####Set 2

hist(frap2$FRAP)
plotNormalHistogram(frap2$FRAP)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(FRAP ~interaction(Treatment, Site), data=frap2) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(FRAP ~interaction(Treatment, Site), data=frap2) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(FRAP ~interaction(Treatment, Site), data=frap2)
par(mfrow=c(2,2))
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))
qqnorm(rstandard(fit), ylab="Residuals")
qqline(rstandard(fit))
plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)
title("Residuals vs fitted")
plot(fitted(fit), abs(residuals(fit)), xlab="Fitted", ylab="Absolute residuals")
abline(h=0)
title("Absolute residuals vs fitted")
##Looks pretty good 

#try FRAP transformations - log, sqrt, cube, recp
F2_log = log(frap2$FRAP)
plotNormalHistogram(F2_log)
F2_sqrt = sqrt(frap2$FRAP)
plotNormalHistogram(F2_sqrt) 
F2_cube= sign(frap2$FRAP) * abs(frap2$FRAP)^(1/3)
plotNormalHistogram(F2_cube) 
F2_recp = 1/(frap2$FRAP)
plotNormalHistogram(F2_recp) #no
##try sqrt

frap2 = cbind(frap2, F2_sqrt)

#sqrt FRAP - set 2
leveneTest(F2_sqrt~interaction(Treatment, Site), data=frap2) #normal
bartlett.test(F2_sqrt~interaction(Treatment, Site), data=frap2) #not normal
fit<-lm(F2_sqrt~interaction(Treatment, Site), data=frap2)
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
##All qqplots look similar to the untransformed variable 



#####Delta FRAP - change in total antiox
hist(frap2$Delta_FRAP)
plotNormalHistogram(frap2$Delta_FRAP)
##########Levene Test
leveneTest(Delta_FRAP~interaction(Treatment, Site), data=frap2) #normal
###########Bartlett test
bartlett.test(Delta_FRAP~interaction(Treatment,Site), data=frap2) #not normal 
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(Delta_FRAP~interaction(Treatment,Site), data=frap2)
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
##Am not going to remove outlier because I think its relevant

#try FRAP transformations - cube, recp
##Can't use log or sqrt with this variable, as it produces NAs bc there are negative #s
DF2_cube= sign(frap2$Delta_FRAP) * abs(frap2$Delta_FRAP)^(1/3)
plotNormalHistogram(DF2_cube) 
DF2_recp = 1/(frap2$Delta_FRAP)
plotNormalHistogram(DF2_recp) 
##Try cube

frap2 = cbind(frap2, DF2_cube)

#cube Delta FRAP - set 2
leveneTest(DF2_cube~interaction(Treatment,Site), data=frap2) #normal
bartlett.test(DF2_cube~interaction(Treatment,Site), data=frap2) #normal
fit<-lm(DF2_cube~interaction(Treatment,Site), data=frap2)
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
##Will be using the original variable 






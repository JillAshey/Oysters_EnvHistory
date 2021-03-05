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








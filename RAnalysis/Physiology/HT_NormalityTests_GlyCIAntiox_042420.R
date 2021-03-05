##Normality - HT
##Glycogen, condition index, FRAP -- all sets 

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
bartlett.test(g_log~Site, data=set1) #normal 
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
bartlett.test(g_sqrt~Site, data=set1) #normal 
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
bartlett.test(g_cube~Site, data=set1) #normal 
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
##log or sqrt may be best 

#binding the transformed variables into the data set 
set2 = cbind(set2, g_log, g_sqrt, g_cube, g_recp)

##glycogen log 
leveneTest(g_log~Site, data=set2) #normal
bartlett.test(g_log~Site, data=set2) #not normal 
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
bartlett.test(g_sqrt~Site, data=set2) #not normal 
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
bartlett.test(g_cube~Site, data=set2) #not normal 
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
leveneTest(g_recp~Site, data=set2) #almost not normal
bartlett.test(g_cube~Site, data=set2) #not normal 
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
bartlett.test(g_cube~Site, data=set2) #not normal 
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
plotNormalHistogram(g_cube) 
leveneTest(g_cube2~Site, data=set2) #normal
bartlett.test(g_cube2~Site, data=set2) #normal 
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
plotNormalHistogram(g_recp2)
leveneTest(g_recp2~Site, data=set2) #normal
bartlett.test(g_recp2~Site, data=set2) #not normal 
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
bartlett.test(P_ChangeGly_DW~Site, data=set2.new) #not normal 
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

##log delta glycogen 
#Have to log set 1 and set 2 glycogen and then calculate delta glycogen from those logged values 
#Logging the glycogen set 1 values
g_log = log(set1$P_Glycogen_DW)
plotNormalHistogram(g_log)
#separating out all the sites
MRmean = set1[c(1:9),]
PRmean = set1[c(10:18),]
UCmean = set1[c(19:25),]
VPmean = set1[c(26:34),]

#calculating the mean of each site's logged glycogen values
m = mean(MRmean$g_log)
p = mean(PRmean$g_log)
u = mean(UCmean$g_log)
v = mean(VPmean$g_log)

##Now turning to set 2
#use glog in this dataset -- was already added above 
MR2 = set2[c(1:9),]
PR2 = set2[c(10:24),]
UC2 = set2[c(25:31),]
VP2 = set2[c(32:40),]

#Calculate log delta glycogen by subtracting the set 2 logged glycogen values by the set1 avg logged glycogen
MR_delta_log= MR2$g_log - m
PR_delta_log= PR2$g_log - p
UC_delta_log= UC2$g_log - u
VP_delta_log = VP2$g_log - v

#putting all the delta log values from section above into dataset because they are being stubborn and not binding properly
delta_gly_log = c(-0.04720698, -0.03445853, 0.01352916,-0.34594247, 0.05488049, 0.27855179, 0.22203951, 0.22197550, 0.45801606, #MR_delta_log
         0.4344621, 0.43331167, 0.49275674, 0.72504327, 0.23998797, -0.64988562, 0.26543797, 0.78030527, 0.21810727, 0.64109808, 0.22629845, -0.18320505, -0.08164695,  0.24340945,  0.04854268, #PR_delta_log
         -0.4816608, 0.6498008, -0.2920111, 0.3627075, -0.7176547, 0.4387437, 0.5888805, #UC_delta_log
         0.55090858, -0.02486724, -0.24480114, 0.30091429, -0.11255726, 0.46944577, 0.77778364, 1.91238112, -0.23280532) #VP_delta_log

#bind original set 2 dataset and delta glycogen log values 
set2 = cbind(set2, delta_gly_log)

##delta glycogen log
plotNormalHistogram(set2$delta_gly_log)
leveneTest(delta_gly_log~Site, data=set2) #normal
bartlett.test(delta_gly_log~Site, data=set2) #not normal 
fit<-lm(delta_gly_log~Site, data=set2)
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


##sqrt delta glycogen 
#Have to sqrt set 1 and set 2 glycogen and then calculate delta glycogen from those sqrt values 
#Logging the glycogen set 1 values
g_sqrt = sqrt(set1$P_Glycogen_DW)
plotNormalHistogram(g_sqrt)
#separating out all the sites
MRmean = set1[c(1:9),]
PRmean = set1[c(10:18),]
UCmean = set1[c(19:25),]
VPmean = set1[c(26:34),]

#calculating the mean of each site's sqrt glycogen values
m = mean(MRmean$g_sqrt)
p = mean(PRmean$g_sqrt)
u = mean(UCmean$g_sqrt)
v = mean(VPmean$g_sqrt)

##Now turning to set 2
#use g_sqrt in this dataset -- was already added above 
MR2 = set2[c(1:9),]
PR2 = set2[c(10:24),]
UC2 = set2[c(25:31),]
VP2 = set2[c(32:40),]

#Calculate sqrt delta glycogen by subtracting the set 2 sqrt glycogen values by the set1 avg sqrt glycogen
MR_delta_sqrt= MR2$g_sqrt - m
PR_delta_sqrt = PR2$g_sqrt - p
UC_delta_sqrt = UC2$g_sqrt - u
VP_delta_sqrt = VP2$g_sqrt - v

#putting all the delta sqrt values from section above into dataset because they are being stubborn and not binding properly
delta_gly_sqrt = c(-0.159011824, -0.140048948, -0.067575206, -0.570462352, -0.003713939,  0.365562283, 0.268327626, 0.268219027, 0.693210904, #MR delta gly sqrt
                   0.86898860, 0.86635005, 1.00469588, 1.58649983, 0.44383289, -1.05132941,  0.49714848, 1.73515130, 0.39853395, 1.36840657, 0.41543388, -0.35013170, -0.17462533,  0.45096119,  0.06379672, #PR delta gly sqrt
                   -0.7807793, 1.1583632, -0.5272357, 0.5582050, -1.0644907, 0.7088661, 1.0237136, #UC delta gly srt
                   0.81628360, -0.09650853, -0.38143816, 0.38753393, -0.21388196, 0.6706443, 1.25459829, 4.37518603, -0.36669194) #VP delta gly sqrt

#bind original set 2 dataset and delta glycogen sqrt values 
set2 = cbind(set2, delta_gly_sqrt)

##delta glycogen sqrt
plotNormalHistogram(set2$delta_gly_sqrt)
leveneTest(delta_gly_sqrt~Site, data=set2) #normal
bartlett.test(delta_gly_sqrt~Site, data=set2) #not normal 
fit<-lm(delta_gly_sqrt~Site, data=set2)
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










##CI set 2
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
#try with all

#binding the transformed variables into the data set 
set2 = cbind(set2, CI_log, CI_sqrt, CI_log, CI_recp)

#Log CI - set 2
plotNormalHistogram(CI_log)
leveneTest(CI_log~Site, data=set2) #normal
bartlett.test(CI_log~Site, data=set2) #normal 
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

#sqrt CI - set 2
plotNormalHistogram(CI_sqrt)
leveneTest(CI_sqrt~Site, data=set2) #normal
bartlett.test(CI_sqrt~Site, data=set2) #normal 
fit<-lm(CI_sqrt~Site, data=set2)
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

#cube CI - set 2
plotNormalHistogram(CI_cube)
leveneTest(CI_cube~Site, data=set2) #normal
bartlett.test(CI_cube~Site, data=set2) #normal 
fit<-lm(CI_cube~Site, data=set2)
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
plotNormalHistogram(CI_recp)
leveneTest(CI_recp~Site, data=set2) #normal
bartlett.test(CI_cube~Site, data=set2) #normal 
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

#try CI transformations - cube, recp
DeltaCI_cube= sign(set2$Delta_CI) * abs(set2$Delta_CI)^(1/3)
plotNormalHistogram(DeltaCI_cube) 
DeltaCI_recp = 1/(set2$Delta_CI)
plotNormalHistogram(DeltaCI_recp)
#maybe recp
##have to calculate log and sqrt below 

set2 = cbind(set2, DeltaCI_recp)

#Recp Delta CI - set 2
leveneTest(DeltaCI_recp~Site, data=set2) #not normal
bartlett.test(Delta_CI~Site, data=set2) #normal 
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

##sqrt delta CI 
#Have to sqrt set 1 and set 2 CI and then calculate delta CI from those sqrt values 
#Sqrt the CI set 1 values
CI_sqrt = sqrt(set1$ConditionIndex)
set1 = cbind(set1, CI_sqrt)
plotNormalHistogram(CI_sqrt)
#separating out all the sites
MRmean = set1[c(1:9),]
PRmean = set1[c(10:18),]
UCmean = set1[c(19:25),]
VPmean = set1[c(26:34),]

#calculating the mean of each site's sqrt glycogen values
m = mean(MRmean$CI_sqrt)
p = mean(PRmean$CI_sqrt)
u = mean(UCmean$CI_sqrt)
v = mean(VPmean$CI_sqrt)

##Now turning to set 2
#use CI_sqrt in this dataset -- was already added above 
MR2 = set2[c(1:9),]
PR2 = set2[c(10:24),]
UC2 = set2[c(25:31),]
VP2 = set2[c(32:40),]

#Calculate sqrt delta glycogen by subtracting the set 2 sqrt glycogen values by the set1 avg sqrt glycogen
MR_delta_CIsqrt= MR2$CI_sqrt - m
PR_delta_CIsqrt = PR2$CI_sqrt - p
UC_delta_CIsqrt = UC2$CI_sqrt - u
VP_delta_CIsqrt = VP2$CI_sqrt - v

#putting all the delta sqrt values from section above into dataset because they are being stubborn and not binding properly
delta_CI_sqrt = c(-0.11320893, 0.03151503, -0.29066580, -0.27353192, -0.16533572, -0.38465099, -0.23975290, -0.01502926, -0.04076458, #MR delta CI sqrt
                  -0.5805713, -0.2025258, -0.4786551, -0.2345460, -0.2933041, -0.2324185, -0.3472415, -0.4945936, -0.3925333, -0.2657775, -0.2706030, -0.1976432, -0.5057831, -0.5993100, -0.4659892, #PR delta CI sqrt
                  -0.3189829, -0.2115069, -0.1313037, -0.2832517,  0.2047194, -0.4516479, -0.4727746, #UC delta CI sqrt
                  -0.17428953, 0.49402846, 0.09745692, 0.22945002, -0.06106587, 0.11002753,  0.04434199,  0.19928984, -0.09324225) #VP delta CI sqrt

#bind original set 2 dataset and delta CI sqrt values 
set2 = cbind(set2, delta_CI_sqrt)

##delta CI sqrt
plotNormalHistogram(set2$delta_CI_sqrt)
leveneTest(delta_CI_sqrt~Site, data=set2) #normal
bartlett.test(delta_CI_sqrt~Site, data=set2) #normal 
fit<-lm(delta_CI_sqrt~Site, data=set2)
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



##log delta CI 
#Have to log set 1 and set 2 CI and then calculate delta CI from those log values 
#log the CI set 1 values
CI_log = log(set1$ConditionIndex)
set1 = cbind(set1, CI_log)
plotNormalHistogram(CI_log)
#separating out all the sites
MRmean = set1[c(1:9),]
PRmean = set1[c(10:18),]
UCmean = set1[c(19:25),]
VPmean = set1[c(26:34),]

#calculating the mean of each site's log delta CI values
m = mean(MRmean$CI_log)
p = mean(PRmean$CI_log)
u = mean(UCmean$CI_log)
v = mean(VPmean$CI_log)

##Now turning to set 2
#use CI_sqrt in this dataset -- was already added above 
MR2 = set2[c(1:9),]
PR2 = set2[c(10:24),]
UC2 = set2[c(25:31),]
VP2 = set2[c(32:40),]

#Calculate sqrt delta glycogen by subtracting the set 2 sqrt glycogen values by the set1 avg sqrt glycogen
MR_delta_CIlog= MR2$CI_log - m
PR_delta_CIlog = PR2$CI_log - p
UC_delta_CIlog = UC2$CI_log - u
VP_delta_CIlog = VP2$CI_log - v

#putting all the delta sqrt values from section above into dataset because they are being stubborn and not binding properly
delta_CI_log = c(-0.088535212, 0.031617598, -0.246452588, -0.230649773, -0.133643213, -0.335429996, -0.199855966, -0.006242688, -0.027488285, #MR delta CI log
                  -0.4898219, -0.1533173, -0.3933888, -0.1797239, -0.2291067, -0.1779585, -0.2755371, -0.4081666, -0.3153753, -0.2058203, -0.2098829, -0.1493212, -0.4186067, -0.5080697, -0.3817228, #PR delta CI log
                  -0.25384789, -0.16378983, -0.09913171, -0.22345517,  0.15110258, -0.37091383, -0.39020556, # UC delta CI log
                  -0.17417896, 0.43712900, 0.09727484, 0.21691831, -0.05656920, 0.10898270, 0.04703450, 0.19020402, -0.08929459) #VP delta CI log

#bind original set 2 dataset and delta CI log values 
set2 = cbind(set2, delta_CI_log)

##delta CI sqrt
plotNormalHistogram(set2$delta_CI_log)
leveneTest(delta_CI_log~Site, data=set2) #normal
bartlett.test(delta_CI_log~Site, data=set2) #normal 
fit<-lm(delta_CI_log~Site, data=set2)
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















##########################################

###Total Antiox for set 1 and 2 - evaluating normality 
##Because there was a potential interaction effect (site and treatment), need to evaluate these variables differently than those above 

###Set 1
frap<-read.csv("FRAP_HT.csv", head=T)

frap1 = frap[1:56,]
frap2 = frap[57:112,]



#######FRAP set 1 and 2 normality tests for FRAP/ g of tissue sample 
####Set 1 for FRAP
##FRAP - total antiox
hist(frap1$test_FRAP)
plotNormalHistogram(frap1$test_FRAP)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(test_FRAP ~interaction(Treatment, Site), data=frap1) #normal

#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(test_FRAP ~interaction(Treatment, Site), data=frap1) #variances are equal

###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(test_FRAP ~interaction(Treatment, Site), data=frap1)
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
##looks good - use original variable for FRAP set 1

####Set 2 for FRAP
hist(frap2$test_FRAP)
plotNormalHistogram(frap2$test_FRAP)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(test_FRAP ~interaction(Treatment, Site), data=frap2) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(test_FRAP ~interaction(Treatment, Site), data=frap2) #normalish
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(test_FRAP ~interaction(Treatment, Site), data=frap2)
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
##an outlier, but proceed with original variable

#transforming the variable using log, sqrt, cube, and reciprocal 
f_log = log(frap2$test_FRAP)
plotNormalHistogram(f_log)
f_sqrt = sqrt(frap2$test_FRAP)
plotNormalHistogram(f_sqrt) 
f_cube= sign(frap2$test_FRAP) * abs(frap2$test_FRAP)^(1/3)
plotNormalHistogram(f_cube) 
f_recp = 1/(frap2$test_FRAP)
plotNormalHistogram(f_recp)
#log, sqrt, cube

#binding the transformed variables into the data set 
frap2 = cbind(frap2, f_log, f_sqrt, f_cube)

###Log FRAP set 2
hist(frap2$f_log)
plotNormalHistogram(frap2$f_log)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(f_log ~interaction(Treatment, Site), data=frap2) #not normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(f_log ~interaction(Treatment, Site), data=frap2) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(f_log ~interaction(Treatment, Site), data=frap2)
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

###sqrt FRAP set 2
hist(frap2$f_sqrt)
plotNormalHistogram(frap2$f_sqrt)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(f_sqrt ~interaction(Treatment, Site), data=frap2) #not normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(f_sqrt ~interaction(Treatment, Site), data=frap2) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(f_sqrt ~interaction(Treatment, Site), data=frap2)
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

###cube FRAP set 2
hist(frap2$f_cube)
plotNormalHistogram(frap2$f_cube)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(f_cube ~interaction(Treatment, Site), data=frap2) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(f_cube ~interaction(Treatment, Site), data=frap2) #not normal
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(f_cube ~interaction(Treatment, Site), data=frap2)
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







####Set 2 for Delta FRAP
hist(frap2$test_FRAP_Delta)
plotNormalHistogram(frap2$test_FRAP_Delta)
#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal
leveneTest(test_FRAP_Delta ~interaction(Treatment, Site), data=frap2) #normal
#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
bartlett.test(test_FRAP_Delta ~interaction(Treatment, Site), data=frap2) #normalish
###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
fit<-lm(test_FRAP_Delta ~interaction(Treatment, Site), data=frap2)
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
#an outlier again, but still go with original variable for the time being

#try delta FRAP transformations - cube, recp
DeltaFRAP_cube= sign(frap2$test_FRAP_Delta) * abs(frap2$test_FRAP_Delta)^(1/3)
plotNormalHistogram(DeltaFRAP_cube) 
DeltaFRAP_recp = 1/(frap2$test_FRAP_Delta)
plotNormalHistogram(DeltaFRAP_recp)
#neither plots look good so will not proceed with either of them
##have to calculate log and sqrt below, as there are negative values in delta FRAP

##sqrt delta FRAP
#Have to sqrt set 1 and set 2 FRAP and then calculate delta FRAP from those sqrt values 
#Sqrt the FRAP set 1 values
FRAP_sqrt = sqrt(frap1$test_FRAP)
frap1 = cbind(frap1, FRAP_sqrt)
plotNormalHistogram(FRAP_sqrt)
#separating out all the sites
MRmean = frap1[c(1:7, 29:35),]
PRmean = frap1[c(8:14, 36:42),]
UCmean = frap1[c(15:21, 43:49),]
VPmean = frap1[c(22:28, 50:56),]

#calculating the mean of each site's sqrt FRAP values - set 1
m = mean(MRmean$FRAP_sqrt)
p = mean(PRmean$FRAP_sqrt)
u = mean(UCmean$FRAP_sqrt)
v = mean(VPmean$FRAP_sqrt)

##Now turning to FRAPset 2
MR2 = frap2[c(1:7, 29:35),]
PR2 = frap2[c(8:14, 36:42),]
UC2 = frap2[c(15:21, 43:49),]
VP2 = frap2[c(22:28, 50:56),]

#Calculate sqrt delta FRAP by subtracting the set 2 sqrt glycogen values by the set1 avg sqrt glycogen
MR_delta_FRAPsqrt= MR2$f_sqrt - m
PR_delta_FRAPsqrt = PR2$f_sqrt - p
UC_delta_FRAPsqrt = UC2$f_sqrt - u
VP_delta_FRAPsqrt = VP2$f_sqrt - v

#putting all the delta sqrt values from section above into dataset because they are being stubborn and not binding properly
delta_FRAP_sqrt = c(0.17206838, -0.05044782, 0.01691653, -0.21037691, 0.13972967, -0.05686258, 0.04421133, 0.01209891, -0.03846060, -0.07278596, -0.05897799, 0.02125137, -0.05177617, 0.21930397, #MR delta FRAP sqrt
                    0.02683155, -0.12198025, -0.22815118, -0.06371348, -0.01364245, -0.19558244, -0.18413657, -0.15975056, -0.15431100, -0.07864624, -0.09954941, -0.09920419, -0.21454815, -0.28150161, #PR delta FRAP sqrt
                    0.078100262, 0.022606148, 0.001823384, 0.070826855, -0.080552462, 0.065236286, -0.035135318, -0.169523157, -0.032717306, 0.072874277, -0.014629881, 0.029018503, -0.118470256, -0.045395462, #UC delta FRAP sqrt
                    0.117941208, 0.174819774, -0.003066619, -0.328436377, -0.090920880, 0.115086002, -0.210347589, -0.087326553, 0.161427407, 0.050565437, -0.103787812, 0.450993902, 0.115086002, -0.086132078) #VP delta FRAP sqrt
  
#bind original frap set 2 dataset and delta FRAP sqrt values 
frap2 = cbind(frap2, delta_FRAP_sqrt)

##delta FRAP sqrt
plotNormalHistogram(frap2$delta_FRAP_sqrt)
leveneTest(delta_FRAP_sqrt~Site, data=frap2) #normal
bartlett.test(delta_FRAP_sqrt~Site, data=frap2) #normal 
fit<-lm(delta_FRAP_sqrt~Site, data=frap2)
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

##log delta FRAP
#Have to log set 1 and set 2 FRAP and then calculate delta FRAP from those log values 
#log the FRAP set 1 values
dFRAP1_log = log(frap1$test_FRAP)
frap1 = cbind(frap1, dFRAP1_log)
plotNormalHistogram(dFRAP1_log)
#separating out all the sites
MRmean = frap1[c(1:7, 29:35),]
PRmean = frap1[c(8:14, 36:42),]
UCmean = frap1[c(15:21, 43:49),]
VPmean = frap1[c(22:28, 50:56),]

#calculating the mean of each site's sqrt FRAP values - set 1
m = mean(MRmean$dFRAP1_log)
p = mean(PRmean$dFRAP1_log)
u = mean(UCmean$dFRAP1_log)
v = mean(VPmean$dFRAP1_log)

##Now turning to FRAPset 2
MR2 = frap2[c(1:7, 29:35),]
PR2 = frap2[c(8:14, 36:42),]
UC2 = frap2[c(15:21, 43:49),]
VP2 = frap2[c(22:28, 50:56),]

#Calculate sqrt delta FRAP by subtracting the set 2 sqrt glycogen values by the set1 avg sqrt glycogen
MR_delta_FRAPlog= MR2$f_log - m
PR_delta_FRAPlog = PR2$f_log - p
UC_delta_FRAPlog = UC2$f_log - u
VP_delta_FRAPlog = VP2$f_log - v

#putting all the delta sqrt values from section above into dataset because they are being stubborn and not binding properly
delta_FRAP_log = c(0.37708033, -0.05340035, 0.08696813, -0.43235896, 0.31998532, -0.06729528, 0.14115223, 0.07725015, -0.02769087, -0.10220986, -0.07189871, 0.09567208, -0.05626976, 0.45765216, #MR delta FRAP log
                   0.05515644, -0.22694853, -0.45570217, -0.11170645, -0.01771684, -0.38270814, -0.35767511, -0.30536350, -0.29387915, -0.14061409, -0.18179451, -0.18110748, -0.42489003, -0.58134678, #PR delta FRAP log
                   0.170528105, 0.067967514, 0.028162806, 0.157381601, -0.137897150, 0.147217720, -0.044641557, -0.334237464, -0.039796470, 0.161091013, -0.003921377, 0.080090711, -0.219229474, -0.065331879, #UC delta FRAP log
                   0.2567886083, 0.3668291086, 0.0003618097, -0.9168015147, -0.2088409033, 0.2511018993, -0.5336322679, -0.1998385481, 0.3414591277, 0.1180867520, -0.2414037241, 0.8289488342, 0.2511018993, -0.1968558136) #VP delta FRAP log

#bind original frap set 2 dataset and delta FRAP log values 
frap2 = cbind(frap2, delta_FRAP_log)

##delta FRAP log
plotNormalHistogram(frap2$delta_FRAP_log)
leveneTest(delta_FRAP_log~Site, data=frap2) #normal
bartlett.test(delta_FRAP_log~Site, data=frap2) #not normal 
fit<-lm(delta_FRAP_log~Site, data=frap2)
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





# Title: FRAP stats and plotting 
# Project: Oyster Env Histrory 
# Author: J. Ashey
# Date: 20210502

#Install and Load necessary libraries
library(devtools)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(readr)
library("plotrix")
library(car)
# install.packages("devtools")---- didn't work once 
#devtools::install_github("tidyverse/reprex")
if("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 
if("emmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('emmeans') 
if("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
library(lubridate)
library(tidyverse)
library(emmeans)
library(gridExtra)

# Read in data 
FRAP <- read_csv("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/data/FRAP/HT_FRAP_bothSets_042320.csv")

#J.Ashey working off A.Schatz - working off of E. Rivest's Code
#my code
FRAP$Treatment<-as.factor(FRAP$Treatment)

FRAP_proximal = FRAP[1:56,]
FRAP_distal = FRAP[57:112,]

#test 1: Levene Test - this is more robust to departures from normality than Bartlett's test. 
#p>0.05 means the variances are equal

#my code
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_proximal) #normal
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_distal) #normal
leveneTest(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal) #normal


#Bartlett test - If the data is normally distributed, this is the best test to use. It is more likely to return a "false positive" when the data is non-normal.
#p>0.05 means the variances are equal
#my code
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_proximal) #not normal
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_distal) #not normal
bartlett.test(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal) #not normal


###Q-Q plot: tests the hypothesis that the data is normally distributed.  If the points on the plot are reasonably well approximated by a straight line, the popular Gaussian data hypothesis is plausible, while marked deviations from linearity provide evidence against this hypothesis and these data points should be removed.  
#Dont think these look good--need to discuss transformations with Emily and the bartlett test
fit<-lm(FRAP ~interaction(Treatment, Site), data=FRAP_proximal)
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

fit<-lm(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal)
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

library(gdata);library(multcomp);library(nlme);library(car);library(MASS)
library(lme4)


delta_f = Delta_FRAP[57:112,]
####PROXIMAL SET

#random effects model with lmer 
LME_FRAP_P <-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_proximal)
require(car)
Anova(LME_FRAP_P, type="III") #no significant interaction terms

#my code
#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP_P=update(LME_FRAP_P, method="ML")
ML_Second_FRAP_P=update(ML_FRAP_P, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP_P,ML_FRAP_P) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05
summary(ML_Second_FRAP_P) #if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
Anova(ML_Second_FRAP_P, type="III") #not significant

#only site is significant so run Tukey post-hoc test on site only 
test <- glht(ML_Second_FRAP_P, linfct = mcp(Site = "Tukey"))
summary(test)

####Distal set
#random effects model with lmer for distal set 
LME_FRAP_D <-lme(FRAP~Treatment*Site, random=~1|Replicate,  data=FRAP_distal)
require(car)
Anova(LME_FRAP_D, type="III") #no sig interactions

#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP_D=update(LME_FRAP_D, method="ML")
ML_Second_FRAP_D=update(ML_FRAP_D, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP_D,ML_FRAP_D) #go with reduced model
summary(ML_Second_FRAP_D)
Anova(ML_Second_FRAP_D, type="III") #no sig. interactions for this one either 
#neither is significant so dont have to run any further functions

###change in FRAP
LME_FRAP_C <-lme(Delta_FRAP~Treatment*Site, random=~1|Replicate,  data=FRAP_distal)
require(car)
Anova(LME_FRAP_C, type="III") #no sig interactions

#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP_C=update(LME_FRAP_C, method="ML")
ML_Second_FRAP_C=update(ML_FRAP_C, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP_C,ML_FRAP_C) #go with reduced model
summary(ML_Second_FRAP_C)
Anova(ML_Second_FRAP_C, type="III") #only site is significant so do Tukeys test on site 

test <- glht(ML_Second_FRAP_C, linfct = mcp(Site = "Tukey"))
summary(test)


###my code####
###running FRAP analysis with MR-1-A outlier missing 
FRAP_M=read.table("FRAP_MR_M.csv", header=T, sep=",")
FRAP_M$Treatment<-as.factor(FRAP_M$Treatment)

library(car)

FRAP_proximal_M = FRAP_M[1:55,]
FRAP_distal_M = FRAP_M[56:111,]

#levene test
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_M) #normal
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_distal_M) #normal
leveneTest(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_M) #normal
#bartlett test 
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_M) #normal
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_distal_M) #not normal
bartlett.test(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_M) #not normal

#plots
fit<-lm(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_M)
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

##proximal with MR-1-A outlier 
#random effects model
LME_FRAP_P <-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_proximal_M)
require(car)
Anova(LME_FRAP_P, type="III") 

##same result with outlier removed 

##does not affect distal measurements, so not going to redo those atm 

##change in FRAP with MR outlier removed
#random effects model
LME_FRAP_C <-lme(Delta_FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_M)
require(car)
Anova(LME_FRAP_C, type="III") 

#hmm this result is different than the one with the outlier included 


##
###running FRAP analysis with PR-2-L outlier missing 
FRAP_P=read.table("FRAP_PR_M.csv", header=T, sep=",")
FRAP_P$Treatment<-as.factor(FRAP_P$Treatment)

library(car)

FRAP_proximal_P = FRAP_P[1:56,]
FRAP_distal_P = FRAP_P[57:111,]

#levene test
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_P) #normal
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_distal_P) #normal
leveneTest(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_P) #normal
#bartlett test 
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_P) #not normal
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_distal_P) #not normal - but basically normal 0.04992
bartlett.test(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_P) #not normal - but basically normal 0.04992

#plots
fit<-lm(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_P)
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

##proximal with PR-2-L outlier gone
#random effects model
LME_FRAP_P <-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_proximal_P)
require(car)
Anova(LME_FRAP_P, type="III") 
#proximal result the same

##distal with PR-2-L outlier gone
#random effects model
LME_FRAP_P <-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_P)
require(car)
Anova(LME_FRAP_P, type="III") 

##change with PR-2-L outlier gone
#random effects model
LME_FRAP_P <-lme(Delta_FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_P)
require(car)
Anova(LME_FRAP_P, type="III") 
#sig interaction with site like original

#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP_P=update(LME_FRAP_P, method="ML")
ML_Second_FRAP_P=update(ML_FRAP_P, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP_P,ML_FRAP_P) #go with reduced model
summary(ML_Second_FRAP_P)
Anova(ML_Second_FRAP_P, type="III") 
#site significant like original 
#so do Tukey test on site only 
test <- glht(ML_Second_FRAP_P, linfct = mcp(Site = "Tukey"))
summary(test) 

##
###running FRAP analysis with BOTH outlier missing 
FRAP_B=read.table("FRAP_B.csv", header=T, sep=",")
FRAP_B$Treatment<-as.factor(FRAP_B$Treatment)

library(car)

FRAP_proximal_B = FRAP_B[1:55,]
FRAP_distal_B = FRAP_B[56:111,]

#levene test
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_B) #normal
leveneTest(FRAP ~interaction(Treatment, Site), data=FRAP_distal_B) #normal
leveneTest(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_B) #normal
#bartlett test 
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_proximal_B) #normal
bartlett.test(FRAP ~interaction(Treatment, Site), data=FRAP_distal_B) #not normal - but basically normal 0.04992
bartlett.test(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_B) #not normal - but basically normal 0.04992

#plots
fit<-lm(Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_B)
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

##proximal with BOTH outlier gone
#random effects model
LME_FRAP_B <-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_proximal_B)
require(car)
Anova(LME_FRAP_B, type="III") 

##distal with BOTH outlier gone
#random effects model
LME_FRAP_B <-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_B, na.action = na.exclude)
require(car)
Anova(LME_FRAP_B, type="III") 

##change with BOTH outlier gone
#random effects model
LME_FRAP_B <-lme(Delta_FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_B, na.action = na.exclude)
require(car)
Anova(LME_FRAP_B, type="III") 


###running FRAP analysis with SQRT TRANSFORM on all points 
FRAP_T=read.table("FRAP_T.csv", header=T, sep=",")
FRAP_T$Treatment<-as.factor(FRAP_T$Treatment)

library(car)

FRAP_proximal_T = FRAP_T[1:56,]
FRAP_distal_T = FRAP_T[57:112,]

#levene test
leveneTest(t_FRAP ~interaction(Treatment, Site), data=FRAP_proximal_T) #normal
leveneTest(t_FRAP ~interaction(Treatment, Site), data=FRAP_distal_T) #normal
leveneTest(t_Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_T) #normal
#bartlett test 
bartlett.test(t_FRAP ~interaction(Treatment, Site), data=FRAP_proximal_T) #not normal
bartlett.test(t_FRAP ~interaction(Treatment, Site), data=FRAP_distal_T) #not normal 
bartlett.test(t_Delta_FRAP ~interaction(Treatment, Site), data=FRAP_distal_T) #not normal - but really close at 0.04579

#plots
fit<-lm(t_FRAP ~interaction(Treatment, Site), data=FRAP_proximal_T)
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

##proximal with SQRT transform
#random effects model
LME_FRAP_T <-lme(t_FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_proximal_T)
require(car)
Anova(LME_FRAP_T, type="III") 

##distal with SQRT transform
#random effects model
LME_FRAP_T <-lme(t_FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_T)
require(car)
Anova(LME_FRAP_T, type="III") 

##change with SQRT transform
#random effects model
LME_FRAP_T <-lme(t_Delta_FRAP~Treatment*Site,random = ~1|Replicate, data=FRAP_distal_T)
require(car)
Anova(LME_FRAP_T, type="III")













###Code from 2/5/20*****
#################################################################################################
##FRAP 
##in case needed: frap<-read.csv("FRAP_HT.csv", head=T)
frap1 = frap[1:56,]
frap2 = frap[57:112,]
frapout = frap1[-2,]

frap2$Treatment<-as.factor(frap2$Treatment)

##FRAP - SET 1

#random effects model with lmer 
LME_FRAP1<-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME_FRAP1, type="III") #no significant interaction terms
#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP1=update(LME_FRAP1, method="ML")
ML_Second_FRAP1=update(ML_FRAP1, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP1,ML_FRAP1) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05
##p-value greater than 0.05, AIC different by 4
#if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
summary(ML_Second_FRAP1) #proceeding with reducted model
Anova(ML_Second_FRAP1, type="III") #not significant

##FRAP set 1 with outlier (MR-1-02-A) removed 
#random effects model with lmer 
LME_FRAP0<-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=frapout)
require(car)
Anova(LME_FRAP0, type="III")#no significant interaction terms
#updating model after no sig. interaction term and running Anova on both models (full and reduced - w/o interaction term) to assess which is better then running an anova on the updated model
ML_FRAP0=update(LME_FRAP0, method="ML")
ML_Second_FRAP0=update(ML_FRAP0, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP0,ML_FRAP0) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05 if going w/ reduced model 
##p-value >0.05 here means that there are no significant differences between the full and reduced models; thus, you should go with reduced model bc want to be parsimonious
##p-value greater than 0.05, AIC different by 3
#if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
summary(ML_Second_FRAP0) #proceeding with reducted model
Anova(ML_Second_FRAP0, type="III") #not significant
##Same result as FRAP w/ outlier -- will keep outlier 

##Now trying FRAP set 1 transformed variables 
##FRAP log 
LME_FRAPlog<-lme(F1_log~Treatment*Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME_FRAPlog, type="III")#no significant interaction terms
#updating model after no sig. interaction term and running Anova on both models (full and reduced - w/o interaction term) to assess which is better then running an anova on the updated model
ML_FRAPlog=update(LME_FRAPlog, method="ML")
ML_Second_FRAPlog=update(ML_FRAPlog, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAPlog,ML_FRAPlog) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05 if going w/ reduced model 
##p-value greater than 0.05, AIC different by 5
#if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
summary(ML_Second_FRAPlog) #proceeding with reducted model
Anova(ML_Second_FRAPlog, type="III") #not significant

##FRAP recp
LME_FRAPrecp<-lme(F1_recp~Treatment*Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME_FRAPrecp, type="III")#no significant interaction terms
#updating model after no sig. interaction term and running Anova on both models (full and reduced - w/o interaction term) to assess which is better then running an anova on the updated model
ML_FRAPrecp=update(LME_FRAPrecp, method="ML")
ML_Second_FRAPrecp = update(ML_FRAPrecp, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAPrecp,ML_FRAPrecp) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05 if going w/ reduced model 
##p-value greater than 0.05, AIC different by 5
#if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
summary(ML_Second_FRAPrecp) #proceeding with reducted model
Anova(ML_Second_FRAPrecp, type="III") #not significant




##FRAP - SET 2

#random effects model with lmer 
LME_FRAP2<-lme(FRAP~Treatment*Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP2, type="III") #no significant interaction terms
#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP2=update(LME_FRAP2, method="ML")
ML_Second_FRAP2=update(ML_FRAP2, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP2,ML_FRAP2) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05
###p-value greater than 0.05, AIC different by 4
#if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
summary(ML_Second_FRAP2) #proceeding with reducted model
Anova(ML_Second_FRAP2, type="III") 

##FRAP - log transformed 
#random effects model with lmer 
LME_FRAP2<-lme(F2_sqrt~Treatment*Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP2, type="III") #no significant interaction terms
#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_FRAP2=update(LME_FRAP2, method="ML")
ML_Second_FRAP2=update(ML_FRAP2, .~.-Treatment:Site, method="ML")
anova(ML_Second_FRAP2,ML_FRAP2) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05
###p-value greater than 0.05, AIC different by 6
#if pvalue is greater than 0.05, proceed with reduced model; if not, than proceed with full model above
summary(ML_Second_FRAP2) #proceeding with reducted model
Anova(ML_Second_FRAP2, type="III") 



#####DELTA FRAP

frap<-read.csv("FRAP_HT.csv", head=T)
frap$Treatment<-as.factor(frap$Treatment)

frap1 = frap[1:56,]
frap2 = frap[57:112,]


#random effects model with lmer 
LME_DeltaFRAP <-lme(Delta_FRAP~Treatment*Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_DeltaFRAP, type="III") ##only site is significant 
#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
ML_DeltaFRAP=update(LME_DeltaFRAP, method="ML")
ML_Second_DeltaFRAP=update(ML_DeltaFRAP, .~.-Treatment:Site, method="ML")
anova(ML_Second_DeltaFRAP,ML_DeltaFRAP) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05
###p-value greater than 0.05, AIC different by 2
summary(ML_Second_DeltaFRAP) #proceeding with reducted model
Anova(ML_Second_DeltaFRAP, type="III") 
##Site significant, treatment and treatment:site not significant 

#only site is significant so run Tukey post-hoc test on site only 
test <- glht(ML_Second_DeltaFRAP, linfct = mcp(Site = "Tukey"))
summary(test)


#####DELTA FRAP - cube
#random effects model with lmer 
LME_DeltaFRAP <-lme(DF2_cube~Treatment*Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_DeltaFRAP, type="III") ##only site is significant 
#updating model after no sig. interaction term and running Anova on both models to assess which is better then running an anova on the updated model
LME_DeltaFRAP=update(LME_DeltaFRAP, method="ML")
ML_Second_DeltaFRAP=update(LME_DeltaFRAP, .~.-Treatment:Site, method="ML")
anova(ML_Second_DeltaFRAP,LME_DeltaFRAP) # AIC should be lower by at least 2 for the 2nd one, pvalue should be greater than 0.05
###p-value greater than 0.05, AIC different by 2
summary(ML_Second_DeltaFRAP) #proceeding with reducted model
Anova(ML_Second_DeltaFRAP, type="III") 

#only site is significant so run Tukey post-hoc test on site only 
test <- glht(ML_Second_DeltaFRAP, linfct = mcp(Site = "Tukey"))
summary(test)



set1<-read.csv("HT_CIGly_Set1.csv", head=T)
library(car)
library(rcompanion)


##Set 1 
set1$Site<-as.factor(set1$Site)


##Glycogen
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(P_Glycogen_DW~Site, na.action = na.exclude, data=set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(g_log~Site, na.action = na.exclude, data=set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(g_sqrt~Site, na.action = na.exclude, data=set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(g_cube~Site, na.action = na.exclude, data=set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)


##Condition Index
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(ConditionIndex~Site, na.action = na.exclude, data=set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(ConditionIndex~Site, data=set1)
dunnTest(ConditionIndex~Site, data=set1) ###Not sure what method to use - automatically fills in Holm method 
##no transformations needed


















####################################################################################

set2<-read.csv("HT_CIGly_Set2.csv", head=T)
##Set 2
set2$Site<-as.factor(set2$Site)


##Glycogen
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(P_Glycogen_DW~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

kruskal.test(P_Glycogen_DW~Site, data=set2)
dunnTest(P_Glycogen_DW~Site, data=set2) 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(g_log~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(g_sqrt~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(g_cube~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(g_recp~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)



##Delta Glycogen
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(P_ChangeGly_DW~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

kruskal.test(P_ChangeGly_DW~Site, data=set2)
dunnTest(P_ChangeGly_DW~Site, data=set2) 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(g_cube2~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(g_recp2~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)


## Condition Index
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(ConditionIndex~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

kruskal.test(ConditionIndex~Site, data=set2)
dunnTest(ConditionIndex~Site, data=set2) 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(CI_log~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

fit2 <-lm(CI_recp~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)


## Delta Condition Index
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Delta_CI~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

kruskal.test(Delta_CI~Site, data=set2)
dunnTest(Delta_CI~Site, data=set2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(DeltaCI_recp~Site, na.action = na.exclude, data=set2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)













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







###differ from zero--using t-tests
#if t-test is significant, can reject null hypothesis
#null hypothesis is that the mean is equal to 0
View(dat2)

t.test(dat2$Delta_Glycogen) #significant
t.test(MR2$Delta_Glycogen) #not significant
t.test(PR2$Delta_Glycogen) #significant
t.test(UC2$Delta_Glycogen) #not significant
t.test(VP2$Delta_Glycogen) #not significant

t.test(dat2$Delta_CI) #significant
t.test(MR2$Delta_CI) #significant
t.test(PR2$Delta_CI) #significant
t.test(UC2$Delta_CI) #significant
t.test(VP2$Delta_CI) #not significant 














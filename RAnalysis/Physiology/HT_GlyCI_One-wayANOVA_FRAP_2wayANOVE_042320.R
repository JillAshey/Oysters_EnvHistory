##ANOVAs for HT
#One way anova for glycogen and condition index; two-way ANOVA for FRAP
#4/24/20

#load packages needed
library(car)
library(rcompanion)
library(multcomp)

####Set 1
#Load data set in
set1<-read.csv("HT_CIGly_Set1.csv", head=T)
View(set1)
set1$Site<-as.factor(set1$Site) #setting site as a factor


##Glycogen - Set 1 
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed glycogen variable in analysis

#make linear model with glycogen as a function of site
fit2 <-lm(P_Glycogen_DW~Site, na.action = na.exclude, data=set1)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in glycogen among sites
Anova(fit2, type="III")
#ANOVA not significant--> do not proceed 
#This means site is not significant in reference to glycogen for set 1



##Condition Index - Set 1 
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed condition index variable in analysis
#make linear model with condition index as a function of site
fit2 <-lm(ConditionIndex~Site, na.action = na.exclude, data=set1)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in CI among sites
Anova(fit2, type="III")
#ANOVA is significant by site, so move on to Tukey's post-hoc test 
#Tukey's post-hoc test compares all sites against one another individually, so I can see which sites differed from others in condition index
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)
##VP differed from MR, PR, UC



##FRAP - set 1
#Load data
frap<-read.csv("FRAP_HT.csv", head=T)
frap$Treatment<-as.factor(frap$Treatment)
###In this data set, I'm using the variables test_FRAP and test_FRAP_delta--> changed units on 4/32/20
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed FRAP variable in analysis

#subset data set into FRAP set 1 & 2
frap1 = frap[1:56,]
frap2 = frap[57:112,]

##In this model, I have two independent variables - treatment and site. 
#making full model with treatment and site; not using interactive method (treatment*site) because Emily said that it was okay to remove interaction term, provided models were not significantly different
##Comparing the model with interaction term to model without interaction term
#model w/o interaction term
LME_FRAP <-lme(test_FRAP~Treatment+Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME_FRAP, type="III") ##site significant
#model w/ interaction term
LME <-lme(test_FRAP~Treatment*Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME, type="III") #no significance - site a little sig.
#Comparing model with interaction term to model w/o interaction term
anova(LME_FRAP, LME)
#p-value >0.05, AIC differs by 9 = move forward with more parsimonious model aka reduced model

##Proceed with model w/o interaction term 
LME_FRAP <-lme(test_FRAP~Treatment+Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME_FRAP, type="III") ##site significant
#Because only site was significant in above model, make a reduced model with only site in it 
reduced_FRAP = lme(test_FRAP~Site,random = ~1|Replicate, data=frap1)
Anova(reduced_FRAP, type = "III") #site significant 
#compare full and reduced model 
anova(LME_FRAP, reduced_FRAP)
##pvalue > 0.05, AIC differs by 4 = move forward with more parsimonious model aka reduced model 
summary(reduced_FRAP)
#only site is significant in reduced model so run Tukey post-hoc test on site only with reduced model
test <- glht(reduced_FRAP, linfct = mcp(Site = "Tukey"))
summary(test)
##VP differed from PR and UC






#####


#######Set 2
#load data in
set2<-read.csv("HT_CIGly_Set2.csv", head=T)
set2$Site<-as.factor(set2$Site)


##Glycogen - Set 2
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the log transformed glycogen variable in analysis

#make linear model with glycogen as a function of site
fit2 <-lm(g_log~Site, na.action = na.exclude, data=set2)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in glycogen among sites
Anova(fit2, type="III")
#ANOVA significant by site, so move on to tukey test
#Tukey's post-hoc test compares all sites against one another individually, so I can see which sites differed from others in glycogen
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)
##MR and PR differ 





##Condition Index - Set 2
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed condition index variable in analysis
#make linear model with condition index as a function of site
fit2 <-lm(ConditionIndex~Site, na.action = na.exclude, data=set2)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in CI among sites
Anova(fit2, type="III")
#ANOVA is significant by site, so move on to Tukey's post-hoc test 
#Tukey's post-hoc test compares all sites against one another individually, so I can see which sites differed from others in condition index
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)
#VP differs from UC



##FRAP - set 2
#Load data
frap<-read.csv("FRAP_HT.csv", head=T)
frap$Treatment<-as.factor(frap$Treatment)
###In this data set, I'm using the variables test_FRAP and test_FRAP_delta--> changed units on 4/32/20
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed FRAP variable in analysis

#subset data set into FRAP set 1 & 2
frap1 = frap[1:56,]
frap2 = frap[57:112,]

##FRAP set 2
##In this model, I have two independent variables - treatment and site. 
#making full model with treatment and site; not using interactive method (treatment*site) because Emily said that it was okay to remove interaction term, provided models were not significantly different
##Comparing the model with interaction term to model without interaction term
#model w/o interaction term
LME_FRAP <-lme(test_FRAP~Treatment+Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP, type="III") ##no significance
#model w/ interaction term
LME <-lme(test_FRAP~Treatment*Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME, type="III") #no significance - site a little sig.
#Comparing model with interaction term to model w/o interaction term
anova(LME_FRAP, LME)
#p-value >0.05, AIC differs by 9 = move forward with more parsimonious model aka reduced model

##Proceed with model w/o interaction term 
LME_FRAP <-lme(test_FRAP~Treatment+Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP, type="III") ##no signifiance
##Because there is no significance with either of the independent variables, do not proceed 





###




##Delta phys variables 
#load data in
set2<-read.csv("HT_CIGly_Set2.csv", head=T) #delta variables in here
set2$Site<-as.factor(set2$Site)


##Delta Glycogen
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the cube OR log transformed delta glycogen variable in analysis
#Will assess both cube and log transformed delta gly here--g_cube2 and delta_gly_log

#make linear model with cube delta glycogen as a function of site
fit2 <-lm(g_cube2~Site, na.action = na.exclude, data=set2)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in cube delta glycogen among sites
Anova(fit2, type="III")
#ANOVA not significant--> do not proceed 
#This means site is not significant in reference to cube delta glycogen

##Will now evaluate delta gly log
#make linear model with log delta glycogen as a function of site
fit2 <-lm(delta_gly_log~Site, na.action = na.exclude, data=set2)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in log delta glycogen among sites
Anova(fit2, type="III")
#ANOVA not significant--> do not proceed 
#This means site is not significant in reference to log delta glycogen

##Both cube and log delta glycogen yield similar results -- no significance 
#Going to go with log delta glycogen, but could be either 



##Delta Condition index
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed OR log transformed delta CI variable in analysis
#Will assess both untransformed and log transformed delta gly here--Delta_CI and delta_CI_log

#make linear model with condition index as a function of site - untransformed delta CI
fit2 <-lm(Delta_CI~Site, na.action = na.exclude, data=set2)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in CI among sites
Anova(fit2, type="III")
#ANOVA is significant by site, so move on to Tukey's post-hoc test 
#Tukey's post-hoc test compares all sites against one another individually, so I can see which sites differed from others in delta condition index
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)
#VP differs from MR, PR, UC; MR differs from PR

#make linear model with condition index as a function of site - log delta CI
fit2 <-lm(delta_CI_log~Site, na.action = na.exclude, data=set2)
summary(fit2) #model summary
require(car)
#ANOVA to see if there are any significant differences in CI among sites
Anova(fit2, type="III")
#ANOVA is significant by site, so move on to Tukey's post-hoc test 
#Tukey's post-hoc test compares all sites against one another individually, so I can see which sites differed from others in log delta condition index
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)
#VP differs from MR, PR, UC

##For delta CI, will be going with untransformed delta CI based on qqplots and histograms 




##Delta FRAP
#Load data
frap<-read.csv("FRAP_HT.csv", head=T)
frap$Treatment<-as.factor(frap$Treatment)
###In this data set, I'm using the variables test_FRAP and test_FRAP_delta--> changed units on 4/32/20
#Based on normality assessments (see R file normalityTests and/or Normality excel file), I'll be using the untransformed delta FRAP variable in analysis
#test_FRAP_Delta 

#subset data set into FRAP set 1 & 2
frap1 = frap[1:56,]
frap2 = frap[57:112,]

##delta FRAP untransformed
##In this model, I have two independent variables - treatment and site. 
#making full model with treatment and site; not using interactive method (treatment*site) because Emily said that it was okay to remove interaction term, provided models were not significantly different
##Comparing the model with interaction term to model without interaction term
#model w/o interaction term
LME_FRAP <-lme(test_FRAP_Delta~Treatment+Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP, type="III") ##site significance
#model w/ interaction term
LME <-lme(test_FRAP_Delta~Treatment*Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME, type="III") #no significance - treatment:site a little sig.
#Comparing model with interaction term to model w/o interaction term
anova(LME_FRAP, LME)
#p-value >0.05, AIC differs by 4 = move forward with more parsimonious model aka reduced model

##Proceed with model w/o interaction term 
LME_FRAP <-lme(test_FRAP_Delta~Treatment+Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP, type="III") ##site signifiance
#Because only site was significant in above model, make a reduced model with only site in it - get rid of treatment
reduced_FRAP = lme(test_FRAP_Delta~Site,random = ~1|Replicate, data=frap2)
Anova(reduced_FRAP, type = "III") #site significant 
#compare full and reduced model 
anova(LME_FRAP, reduced_FRAP)
##pvalue > 0.05, AIC differs by 5 = move forward with more parsimonious model aka reduced model 
summary(reduced_FRAP)
#only site is significant in reduced model so run Tukey post-hoc test on site only with reduced model
test <- glht(reduced_FRAP, linfct = mcp(Site = "Tukey"))
summary(test)
##PR differs form MR and VP
















###Previous work
#########3/15/20
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
frap<-read.csv("FRAP_HT.csv", head=T)
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









#4/23/20
##Anova using the FRAP/g of tissue sample
#Doing this because there is a strange outlier in my FRAP data using the umol/g values
#To investigate this, Emily told me to try the FRAP/g tissue sample data instead because that had no weird outlier that was first noticeable
#She thought the issue may have been the protein / FRAP tissue sample that I measured was wonky and messed up to get that particular outlier for FRAP umol/g
frap<-read.csv("FRAP_HT.csv", head=T)
frap$Treatment<-as.factor(frap$Treatment)

frap1 = frap[1:56,]
frap2 = frap[57:112,]

##Set 1 - frap
#making full model with treatment and site 
LME_FRAP <-lme(test_FRAP~Treatment+Site,random = ~1|Replicate, data=frap1)
require(car)
Anova(LME_FRAP, type="III") ##site significant
#bc only site was significant, make a reduced model with only site in it 
reduced_FRAP = lme(test_FRAP~Site,random = ~1|Replicate, data=frap1)
Anova(reduced_FRAP, type = "III") #site significant 
#compare full and reduced model 
anova(LME_FRAP, reduced_FRAP)
##pvalue > 0.05, AIC differs by 4 = move forward with more parsimonious model aka reduced model 
summary(reduced_FRAP)
#only site is significant so run Tukey post-hoc test on site only with reduced model
test <- glht(reduced_FRAP, linfct = mcp(Site = "Tukey"))
summary(test)

#Set 2 - frap
#making full model with treatment and site 
LME_FRAP <-lme(test_FRAP~Treatment+Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(LME_FRAP, type="III")
#no significance for treatment or site, so do not move forward with any models 

#####DELTA FRAP
#random effects model with lmer 
lm_deltaFRAP <-lme(test_FRAP_Delta~Treatment+Site,random = ~1|Replicate, data=frap2)
require(car)
Anova(lm_deltaFRAP, type="III") #only site significant 
#bc only site was significant, make a reduced model with only site in it 
reduced_DeltaFRAP = lme(test_FRAP_Delta~Site,random = ~1|Replicate, data=frap2)
Anova(reduced_DeltaFRAP, type = "III") #site significant 
#compare full and reduced model to determine which is best to move forward with in analysis
anova(lm_deltaFRAP, reduced_DeltaFRAP)
##pvalue > 0.05, AIC differs by 4 = move forward with more parsimonious model aka reduced model 
summary(reduced_DeltaFRAP)
#only site is significant so run Tukey post-hoc test on site only with reduced model
test <- glht(reduced_DeltaFRAP, linfct = mcp(Site = "Tukey"))
summary(test)

##For some reason, Emily had me run the FRAP/g tissue samples using the model syntax: Treatment + Site 
#In models with FRAP umol/g above, I ran models with an interaction term (Treatment*Site, which translates to Treatment + Site + Treatment*Site) and then parsed the models from there
#not sure why she wanted me to run it without the interaction term this time












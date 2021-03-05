#Script for running one-way ANOVA with cohort as a random effect
#102815
#Emily Rivest

#One-way ANOVA for field water quality


#Set up working directory.  For me, the R directory is in My Documents in a folder titled "R data files"
#Data file must be .csv with only 1 row of column headings. No other text. Categorical variable columns must contain no numbers.

setwd("/Users/jillashey/Desktop")

library(gdata);library(multcomp);library(nlme);library(car);library(MASS)

library(lme4)
p=read.table("HT_Field_WaterQuality_results.csv", header=T, sep=",")
p$Site<-as.factor(p$Site)

MR = p[1:10,]
PR = p[11:20,]
UC = p[21:30,]
VP = p[31:39,]
VP2 = p[40:44,]

AllSite = rbind(MR,PR,UC,VP)
MR_VP2 = rbind(MR, VP2)
PR_VP2 = rbind(PR,VP2)
UC_VP2 = rbind(UC,VP2)
VP_VP2 = rbind(VP, VP2)

getwd()


####ALL SITES WITHOUT COMMON GARDEN 
#random effects model with lmer
# nest treatment in tube (since tubes were within the same treatment chamber) 
# compare variation among treatments and within tanks, Sanford 2008 (I think)
#is an example of this
#Emily's model
#fit <-lmer(O2protein~CO2*Temp*Day+(1|Tube), data=p)
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=p)


#Dan's model
#fit2 <-lmer(aTP~CO2+Temp+Day+CO2:Day+Temp:Day+(1|Tube), data=p, REML=F)
#AIC(fit,fit2)


summary(fit2)
#summary has no p-values, but in general signficance is when the tvalue ~ 2
# could test using Anova
require(car)
Anova(fit2, type="III")
#but the safest option is to drop terms and compare using anova (Zuur 2009); fit using ml
#be clear this isn't fishing or just backwards selection; you've added appropriate terms and random
#YOU SHOULDN'T REALLY NEED TO DO THIS FOR AN ANOVA (WITH FACTORS)EXCEPT FOR REMOVING THE INTERACTION SINCE THE DESIGN IS ORTHOGONAL
#BUT THIS IS HOW YOU WOULD HANDLE MULTLIPLE REGRESSION
#AND THIS IS WHAT YOU THE LMERCONVENIENCEFUNCTIONS ACTUALLY DO
#variables




#for lme
#fitml=update(fit2, method="ML")
#fit1ml=update(fitml, .~.-Treatment:Group, method="ML")
#anova(fit1ml,fitml)
#summary(fit1ml)
#Anova(fit1ml, type="III")

#fit2 <-lm(Percent_Growth_Bill~Treatment+Group+Treatment:Group, random=~1|Jar, na.action = na.exclude, data=p, method="ML")
#fit2reml.step=update(fit2, method="REML")
#cmCGrowth <- read.csv("cmCGrowth.csv")
#cm2CGrowth <- as.matrix(cmCGrowth[,2:ncol(cmCGrowth)],ncol=(ncol(cmCGrowth)-1))
#row.names(cm2CGrowth) <- cmCGrowth$CGrowth 
#head(cm2CGrowth)
#library(multcomp);library(nlme)
#length(fixef(fit2reml.step))==ncol(cm2CGrowth) #verifies number of model parameters = number of matrix columns
#summary(glht(fit2reml.step, linfct= cm2CGrowth, alternative= "two.sided"), test= adjusted(type="bonferroni"))

#fit2 <-lme(FRAPps~CO2+Temp+Day+CO2:Temp+CO2:Day+Temp:Day+CO2:Temp:Day, random=~1|Tank, na.action = na.exclude, data=p, method="ML")
#fit2reml.step=update(fit2, method="REML")
#cmFRAPps <- read.csv("cmFRAPps.csv")
#cm2FRAPps <- as.matrix(cmFRAPps[,2:ncol(cmFRAPps)],ncol=(ncol(cmFRAPps)-1))
#row.names(cm2FRAPps) <- cmFRAPps$FRAPps 
#head(cm2FRAPps)
#library(multcomp);library(nlme)
#length(fixef(fit2reml.step))==ncol(cm2FRAPps) #verifies number of model parameters = number of matrix columns
#summary(glht(fit2reml.step, linfct= cm2FRAPps, alternative= "two.sided"), test= adjusted(type="bonferroni"))

#fit2 <-lme(Symbpl~CO2+Temp+Day+CO2:Temp+CO2:Day+Temp:Day+CO2:Temp:Day, random=~1|Tank, na.action = na.exclude, data=p, method="ML")
#fit2reml.step=update(fit2, method="REML")
#cmSymbpl <- read.csv("cmSymbpl.csv")
#cm2Symbpl <- as.matrix(cmSymbpl[,2:ncol(cmSymbpl)],ncol=(ncol(cmSymbpl)-1))
#row.names(cm2Symbpl) <- cmSymbpl$Symbpl 
#head(cm2Symbpl)
#library(multcomp);library(nlme)
#length(fixef(fit2reml.step))==ncol(cm2Symbpl) #verifies number of model parameters = number of matrix columns
#summary(glht(fit2reml.step, linfct= cm2Symbpl, alternative= "two.sided"), test= adjusted(type="bonferroni"))

#fit2reml.step=update(fit2, method="REML")
#cmTotalWEpp <- read.csv("cmTotalWEpp.csv")
#cm2TotalWEpp <- as.matrix(cmTotalWEpp[,2:ncol(cmTotalWEpp)],ncol=(ncol(cmTotalWEpp)-1))
#row.names(cm2TotalWEpp) <- cmTotalWEpp$TotalWEpp 
#head(cm2TotalWEpp)

#library(multcomp);library(nlme)
#length(fixef(fit2reml.step))==ncol(cm2TotalWEpp)
#summary(glht(fit2reml.step, linfct= cm2TotalWEpp, alternative= "two.sided"))#, test= adjusted(type="bonferroni"))


###Perform a post-hoc comparison using multcomp glht and Tukey's HSD


##for temp
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##for spec_pH
fit_spec <-lm(spec_pH~Site, na.action = na.exclude, data=AllSite)
summary(fit_spec)
require(car)
Anova(fit_spec, type="III")
test_spec <- glht(fit_spec, linfct = mcp(Site = "Tukey"))
summary(test_spec)

##for salinity
fit_sal <-lm(Salinity~Site, na.action = na.exclude, data=AllSite)
summary(fit_sal)
require(car)
Anova(fit_sal, type="III")
test_sal <- glht(fit_sal, linfct = mcp(Site = "Tukey"))
summary(test_sal)

##for DOmg
fit_DOm <-lm(DO_mg~Site, na.action = na.exclude, data=AllSite)
summary(fit_DOm)
require(car)
Anova(fit_DOm, type="III")
test_DOm <- glht(fit_DOm, linfct = mcp(Site = "Tukey"))
summary(test_DOm)

##for pH
fit_ph <-lm(pH~Site, na.action = na.exclude, data=AllSite)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for TA
fit_TA <-lm(TA~Site, na.action = na.exclude, data=AllSite)
summary(fit_TA)
require(car)
Anova(fit_TA, type="III")
test_TA <- glht(fit_TA, linfct = mcp(Site = "Tukey"))
summary(test_TA)

##for conductivity
fit_cond <-lm(Conductivity~Site, na.action = na.exclude, data=AllSite)
summary(fit_cond)
require(car)
Anova(fit_cond, type="III")
test_cond <- glht(fit_cond, linfct = mcp(Site = "Tukey"))
summary(test_cond)

##for DO_Percent
fit_DOp <-lm(DO_Percent~Site, na.action = na.exclude, data=AllSite)
summary(fit_DOp)
require(car)
Anova(fit_DOp, type="III")
test_DOp <- glht(fit_DOp, linfct = mcp(Site = "Tukey"))
summary(test_DOp)

##for YSI pH
fit_ph <-lm(pH~Site, na.action = na.exclude, data=p)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for pCO2
fit_CO2 <-lm(pCO2~Site, na.action = na.exclude, data=AllSite)
summary(fit_CO2)
require(car)
Anova(fit_CO2, type="III")
test_CO2 <- glht(fit_CO2, linfct = mcp(Site = "Tukey"))
summary(test_CO2)

##for Ca
fit_Ca <-lm(Omega_Ca~Site, na.action = na.exclude, data=AllSite)
summary(fit_Ca)
require(car)
Anova(fit_Ca, type="III")
test_Ca <- glht(fit_Ca, linfct = mcp(Site = "Tukey"))
summary(test_Ca)

##for Ar
fit_Ar <-lm(Omega_Ar~Site, na.action = na.exclude, data=AllSite)
summary(fit_Ar)
require(car)
Anova(fit_Ar, type="III")
test_Ar <- glht(fit_Ar, linfct = mcp(Site = "Tukey"))
summary(test_Ar)



###VP2 and MR
#for temp
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##for spec_pH
fit_spec <-lm(spec_pH~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_spec)
require(car)
Anova(fit_spec, type="III")
test_spec <- glht(fit_spec, linfct = mcp(Site = "Tukey"))
summary(test_spec)

##for salinity
fit_sal <-lm(Salinity~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_sal)
require(car)
Anova(fit_sal, type="III")
test_sal <- glht(fit_sal, linfct = mcp(Site = "Tukey"))
summary(test_sal)

##for DOmg
fit_DOm <-lm(DO_mg~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_DOm)
require(car)
Anova(fit_DOm, type="III")
test_DOm <- glht(fit_DOm, linfct = mcp(Site = "Tukey"))
summary(test_DOm)

##for pH YSI
fit_ph <-lm(pH~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for TA
fit_TA <-lm(TA~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_TA)
require(car)
Anova(fit_TA, type="III")
test_TA <- glht(fit_TA, linfct = mcp(Site = "Tukey"))
summary(test_TA)

##for conductivity
fit_cond <-lm(Conductivity~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_cond)
require(car)
Anova(fit_cond, type="III")
test_cond <- glht(fit_cond, linfct = mcp(Site = "Tukey"))
summary(test_cond)

##for DO_Percent
fit_DOp <-lm(DO_Percent~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_DOp)
require(car)
Anova(fit_DOp, type="III")
test_DOp <- glht(fit_DOp, linfct = mcp(Site = "Tukey"))
summary(test_DOp)

##for YSI pH
fit_ph <-lm(pH~Site, na.action = na.exclude, data=p)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for pCO2
fit_CO2 <-lm(pCO2~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_CO2)
require(car)
Anova(fit_CO2, type="III")
test_CO2 <- glht(fit_CO2, linfct = mcp(Site = "Tukey"))
summary(test_CO2)

##for Ca
fit_Ca <-lm(Omega_Ca~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_Ca)
require(car)
Anova(fit_Ca, type="III")
test_Ca <- glht(fit_Ca, linfct = mcp(Site = "Tukey"))
summary(test_Ca)

##for Ar
fit_Ar <-lm(Omega_Ar~Site, na.action = na.exclude, data=MR_VP2)
summary(fit_Ar)
require(car)
Anova(fit_Ar, type="III")
test_Ar <- glht(fit_Ar, linfct = mcp(Site = "Tukey"))
summary(test_Ar)


###For VP2 and PR
#for temp
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##for spec_pH
fit_spec <-lm(spec_pH~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_spec)
require(car)
Anova(fit_spec, type="III")
test_spec <- glht(fit_spec, linfct = mcp(Site = "Tukey"))
summary(test_spec)

##for salinity
fit_sal <-lm(Salinity~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_sal)
require(car)
Anova(fit_sal, type="III")
test_sal <- glht(fit_sal, linfct = mcp(Site = "Tukey"))
summary(test_sal)

##for DOmg
fit_DOm <-lm(DO_mg~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_DOm)
require(car)
Anova(fit_DOm, type="III")
test_DOm <- glht(fit_DOm, linfct = mcp(Site = "Tukey"))
summary(test_DOm)

##for pH YSI
fit_ph <-lm(pH~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for TA
fit_TA <-lm(TA~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_TA)
require(car)
Anova(fit_TA, type="III")
test_TA <- glht(fit_TA, linfct = mcp(Site = "Tukey"))
summary(test_TA)

##for conductivity
fit_cond <-lm(Conductivity~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_cond)
require(car)
Anova(fit_cond, type="III")
test_cond <- glht(fit_cond, linfct = mcp(Site = "Tukey"))
summary(test_cond)

##for DO_Percent
fit_DOp <-lm(DO_Percent~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_DOp)
require(car)
Anova(fit_DOp, type="III")
test_DOp <- glht(fit_DOp, linfct = mcp(Site = "Tukey"))
summary(test_DOp)

##for YSI pH
fit_ph <-lm(pH~Site, na.action = na.exclude, data=p)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for pCO2
fit_CO2 <-lm(pCO2~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_CO2)
require(car)
Anova(fit_CO2, type="III")
test_CO2 <- glht(fit_CO2, linfct = mcp(Site = "Tukey"))
summary(test_CO2)

##for Ca
fit_Ca <-lm(Omega_Ca~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_Ca)
require(car)
Anova(fit_Ca, type="III")
test_Ca <- glht(fit_Ca, linfct = mcp(Site = "Tukey"))
summary(test_Ca)

##for Ar
fit_Ar <-lm(Omega_Ar~Site, na.action = na.exclude, data=PR_VP2)
summary(fit_Ar)
require(car)
Anova(fit_Ar, type="III")
test_Ar <- glht(fit_Ar, linfct = mcp(Site = "Tukey"))
summary(test_Ar)


##for VP2 and UC
#for temp
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##for spec_pH
fit_spec <-lm(spec_pH~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_spec)
require(car)
Anova(fit_spec, type="III")
test_spec <- glht(fit_spec, linfct = mcp(Site = "Tukey"))
summary(test_spec)

##for salinity
fit_sal <-lm(Salinity~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_sal)
require(car)
Anova(fit_sal, type="III")
test_sal <- glht(fit_sal, linfct = mcp(Site = "Tukey"))
summary(test_sal)

##for DOmg
fit_DOm <-lm(DO_mg~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_DOm)
require(car)
Anova(fit_DOm, type="III")
test_DOm <- glht(fit_DOm, linfct = mcp(Site = "Tukey"))
summary(test_DOm)

##for pH YSI
fit_ph <-lm(pH~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for TA
fit_TA <-lm(TA~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_TA)
require(car)
Anova(fit_TA, type="III")
test_TA <- glht(fit_TA, linfct = mcp(Site = "Tukey"))
summary(test_TA)

##for conductivity
fit_cond <-lm(Conductivity~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_cond)
require(car)
Anova(fit_cond, type="III")
test_cond <- glht(fit_cond, linfct = mcp(Site = "Tukey"))
summary(test_cond)

##for DO_Percent
fit_DOp <-lm(DO_Percent~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_DOp)
require(car)
Anova(fit_DOp, type="III")
test_DOp <- glht(fit_DOp, linfct = mcp(Site = "Tukey"))
summary(test_DOp)

##for YSI pH
fit_ph <-lm(pH~Site, na.action = na.exclude, data=p)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for pCO2
fit_CO2 <-lm(pCO2~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_CO2)
require(car)
Anova(fit_CO2, type="III")
test_CO2 <- glht(fit_CO2, linfct = mcp(Site = "Tukey"))
summary(test_CO2)

##for Ca
fit_Ca <-lm(Omega_Ca~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_Ca)
require(car)
Anova(fit_Ca, type="III")
test_Ca <- glht(fit_Ca, linfct = mcp(Site = "Tukey"))
summary(test_Ca)

##for Ar
fit_Ar <-lm(Omega_Ar~Site, na.action = na.exclude, data=UC_VP2)
summary(fit_Ar)
require(car)
Anova(fit_Ar, type="III")
test_Ar <- glht(fit_Ar, linfct = mcp(Site = "Tukey"))
summary(test_Ar)


###For VP2 and VP
#for temp
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##for spec_pH
fit_spec <-lm(spec_pH~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_spec)
require(car)
Anova(fit_spec, type="III")
test_spec <- glht(fit_spec, linfct = mcp(Site = "Tukey"))
summary(test_spec)

##for salinity
fit_sal <-lm(Salinity~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_sal)
require(car)
Anova(fit_sal, type="III")
test_sal <- glht(fit_sal, linfct = mcp(Site = "Tukey"))
summary(test_sal)

##for DOmg
fit_DOm <-lm(DO_mg~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_DOm)
require(car)
Anova(fit_DOm, type="III")
test_DOm <- glht(fit_DOm, linfct = mcp(Site = "Tukey"))
summary(test_DOm)

##for pH YSI
fit_ph <-lm(pH~Site, na.action = na.exclude, data=(VP_VP2))
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for TA
fit_TA <-lm(TA~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_TA)
require(car)
Anova(fit_TA, type="III")
test_TA <- glht(fit_TA, linfct = mcp(Site = "Tukey"))
summary(test_TA)

##for conductivity
fit_cond <-lm(Conductivity~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_cond)
require(car)
Anova(fit_cond, type="III")
test_cond <- glht(fit_cond, linfct = mcp(Site = "Tukey"))
summary(test_cond)

##for DO_Percent
fit_DOp <-lm(DO_Percent~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_DOp)
require(car)
Anova(fit_DOp, type="III")
test_DOp <- glht(fit_DOp, linfct = mcp(Site = "Tukey"))
summary(test_DOp)

##for YSI pH
fit_ph <-lm(pH~Site, na.action = na.exclude, data=p)
summary(fit_ph)
require(car)
Anova(fit_ph, type="III")
test_ph <- glht(fit_ph, linfct = mcp(Site = "Tukey"))
summary(test_ph)

##for pCO2
fit_CO2 <-lm(pCO2~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_CO2)
require(car)
Anova(fit_CO2, type="III")
test_CO2 <- glht(fit_CO2, linfct = mcp(Site = "Tukey"))
summary(test_CO2)

##for Ca
fit_Ca <-lm(Omega_Ca~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_Ca)
require(car)
Anova(fit_Ca, type="III")
test_Ca <- glht(fit_Ca, linfct = mcp(Site = "Tukey"))
summary(test_Ca)

##for Ar
fit_Ar <-lm(Omega_Ar~Site, na.action = na.exclude, data=VP_VP2)
summary(fit_Ar)
require(car)
Anova(fit_Ar, type="III")
test_Ar <- glht(fit_Ar, linfct = mcp(Site = "Tukey"))
summary(test_Ar)

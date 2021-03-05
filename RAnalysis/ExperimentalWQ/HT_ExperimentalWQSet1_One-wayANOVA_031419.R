#Script for running one-way ANOVA --Water quality set 2 experiment 
#03/02/19

#Script for running one-way ANOVA with cohort as a random effect
#102815
#Emily Rivest


#Set up working directory.  For me, the R directory is in My Documents in a folder titled "R data files"
#Data file must be .csv with only 1 row of column headings. No other text. Categorical variable columns must contain no numbers.

setwd("/Users/jillashey/Desktop")
library(gdata);library(multcomp);library(nlme);library(car);library(MASS)

library(lme4)
set2 = read.csv("WaterQuality_Experiment_Set2_HT.csv", header=T)

##doing K-W test on ones that were not normally distributed / could not get to be that way
kruskal.test(Temp~Treatment, data=set2)
kruskal.test(Sal~Treatment, data=set2)
kruskal.test(DO_P~Treatment, data=set2)
kruskal.test(pCO2~Treatment, data=set2)

#random effects model with lmer
# nest treatment in tube (since tubes were within the same treatment chamber) 
# compare variation among treatments and within tanks, Sanford 2008 (I think)
#is an example of this
#Emily's model
#fit <-lmer(O2protein~CO2*Temp*Day+(1|Tube), data=p)
fit2 <-lm(DO_mg~Treatment, na.action = na.exclude, data=set2)


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
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

#for pH 
fit2 <-lm(pH~Treatment, na.action = na.exclude, data=set2)
summary(fit2)
#summary has no p-values, but in general signficance is when the tvalue ~ 2
# could test using Anova
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

#spec pH
fit2 <-lm(spec_pH~Treatment, na.action = na.exclude, data=set2)
summary(fit2)
#summary has no p-values, but in general signficance is when the tvalue ~ 2
# could test using Anova
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

#TA - transformed with reciprocal
fit2 <-lm(t_TA~Treatment, na.action = na.exclude, data=set2)
summary(fit2)
#summary has no p-values, but in general signficance is when the tvalue ~ 2
# could test using Anova
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

#t_Cond - transformed with sqrt
fit2 <-lm(t_Cond~Treatment, na.action = na.exclude, data=set2)
summary(fit2)
#summary has no p-values, but in general signficance is when the tvalue ~ 2
# could test using Anova
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

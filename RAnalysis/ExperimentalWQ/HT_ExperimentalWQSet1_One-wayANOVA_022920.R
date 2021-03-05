#Script for running one-way ANOVA --Water quality set 1 experiment 
#03/02/19


#Set up working directory.  

setwd("/Users/jillashey/Desktop")
library(gdata);library(multcomp);library(nlme);library(car);library(MASS) 

library(lme4)
trial = read.csv("WaterQuality_Experiment_Set1_HT.csv", head = T)


####from just comparing low and ambient
###running variables from water quality set 1 that were not normal 
#using the K-W test 
kruskal.test(Temp~Treatment, data=trial) #p value of 0.019
kruskal.test(Cond~Treatment, data=trial) #p value of <0.001
kruskal.test(DO_mg~Treatment, data=trial) #p value of <0.001
kruskal.test(pH~Treatment, data=trial) #p value of <0.001
kruskal.test(spec_pH~Treatment, data=trial) #p value of <0.001

#random effects model with lmer
fit2 <-lm(Temp~Treatment, na.action = na.exclude, data=trial)
summary(fit2)
#summary has no p-values, but in general signficance is when the tvalue ~ 2
#could NOT use ANOVA in this example with temperature bc it is not 
#normally distributed but just seeing how the test turns out 

require(car)
Anova(fit2, type="III")

###Perform a post-hoc comparison using multcomp glht and Tukey's HSD
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)


####comparing low and the 3 individual ambient treatments 
fit2 <-lm(Temp~Treatment, na.action = na.exclude, data=trial)


#Dan's model
#fit2 <-lmer(aTP~CO2+Temp+Day+CO2:Day+Temp:Day+(1|Tube), data=p, REML=F)
#AIC(fit,fit2)

##DO%
fit2 <-lm(DO_P~Treatment, na.action = na.exclude, data=trial)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##DO mg
fit2 <-lm(t_DO_mg~Treatment, na.action = na.exclude, data=trial)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##pCO2
fit2 <-lm(pCO2~Treatment, na.action = na.exclude, data=trial)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##using the K-W test with low and 3 ambient treatments separated 
kruskal.test(Temp~Treatment, data=trial)
kruskal.test(Cond~Treatment, data=trial)
kruskal.test(Sal~Treatment, data=trial)
kruskal.test(pH~Treatment, data=trial)
kruskal.test(TA~Treatment, data=trial, na.action = na.exclude)

#how to do tukey post hoc test on non normal values?
#is it even possible?



blah = read.csv("WaterQuality_Experiment_Set2_HT.csv", head = T)
fit2 <-lm(DO_mg~Treatment, na.action = na.exclude, data=blah)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

fit2 <-lm(t_DO_P~Treatment, na.action = na.exclude, data=blah)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

kruskal.test(t_DO_P~Treatment, na.action = na.exclude, data=blah)



####Dunn test 
install.packages("FSA")
library(FSA)

#DUNN test for Temp
PT = dunnTest(Temp ~ Treatment,
              data=trial,
              method="bh")
PT

#DUNN test for Salinity
PS = dunnTest(Sal ~ Treatment,
              data=trial,
              method="bh")
PS

#DUNN test for Cond
PC = dunnTest(Cond ~ Treatment,
              data=trial,
              method="bh")
PC


#DUNN test for pH
pH = dunnTest(pH ~ Treatment,
              data=trial,
              method="bh")
pH

#DUNN test for TA
TA = dunnTest(TA ~ Treatment, na.action = na.exclude,
              data=trial,
              method="bh")
TA

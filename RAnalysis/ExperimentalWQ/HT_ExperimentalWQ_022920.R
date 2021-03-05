###Script for running one-way ANOVA --Water quality experiment HT
##022820

library(car)
library(rcompanion)
library(gdata);library(multcomp);library(nlme);library(car);library(MASS)
library(lme4)
library(FSA)

##Set 1 exp WQ
Water_Set1 = read.csv("WaterQuality_Experiment_Set1_HT_UPDATED_021020.csv", head = T)
Low_Set1 = Water_Set1[1:224,]
Amb_Set1 = Water_Set1[225:448,]
Water_Set1$Site=as.factor(Water_Set1$Site)
Water_Set1$Treatment=as.factor(Water_Set1$Treatment)
##Make sure WQ data sets include the transformed variables that were binded in during normality analyses 
#if not, go back to normality R script and run the column binding code 


##Temperature -- Set 1

##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temp~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)
#some significance 

kruskal.test(Temp~Treatment, data=Water_Set1) #significant 
#DUNN test for Temp
PT = dunnTest(Temp ~ Treatment,
              data=Water_Set1,
              method="bh")
PT
##different result than what I got on my HT.....


##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
##sqrt
fit2 <-lm(t_sqrt1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

fit2 <-lm(t_cube1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)




###salinity - set 1 
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Sal~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test_sal)

kruskal.test(Sal~Treatment, data=Water_Set1)
#DUNN test for Salinity
PS = dunnTest(Sal ~ Treatment,
              data=Water_Set1,
              method="bh")
PS

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
##log
fit2 <-lm(Slog1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##recp
fit2 <-lm(Srecp1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)




##DO - set 1
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test_sal)

kruskal.test(DO_mg~Treatment, data=Water_Set1)
#DUNN test for Salinity
DO = dunnTest(DO_mg ~ Treatment,
              data=Water_Set1,
              method="bh")
DO

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
##sqrt
fit2 <-lm(DOsqrt1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##cube 
fit2 <-lm(DOcube1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)



##pH set 1
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(spec_pH~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test_sal)

kruskal.test(spec_pH~Treatment, data=Water_Set1)
#DUNN test for Salinity
pH = dunnTest(spec_pH ~ Treatment, na.action = na.exclude, data=Water_Set1,method="bh")
pH

##log
fit2 <-lm(pHlog1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##sqrt
fit2 <-lm(pHsqrt1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##cube
fit2 <-lm(pHcube1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)



###TA
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test_sal)

kruskal.test(TA~Treatment, data=Water_Set1)
#DUNN test for Salinity
TA = dunnTest(TA ~ Treatment, data=Water_Set1, method="bh")
TA

##log
fit2 <-lm(TAlog1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##sqrt
fit2 <-lm(TAsqrt1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

##cube
fit2 <-lm(TAcube1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)



##pCO2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test_sal)

kruskal.test(pCO2~Treatment, data=Water_Set1)
#DUNN test for pCO2
pCO2 = dunnTest(pCO2 ~ Treatment, data=Water_Set1, method="bh")
pCO2

##pCO2 transformations 
#log 
fit2 <-lm(pCO2log1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

#recp
fit2 <-lm(pCO2recp1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)



##Omega Ca
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(OmegaCa~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test_sal)

kruskal.test(OmegaCa~Treatment, data=Water_Set1)
#DUNN test for OmegaCa
Ca = dunnTest(OmegaCa ~ Treatment, data=Water_Set1, method="bh")
Ca

##transformations 
#log
fit2 <-lm(Calog1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)

#cube
fit2 <-lm(Cacube1~Treatment, na.action = na.exclude, data=Water_Set1)
summary(fit2)
require(car)
Anova(fit2, type="III")
test<- glht(fit2, linfct = mcp(Treatment = "Tukey"))
summary(test)







###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
#Set 2 experimental WQ

#Set WD as ~/Desktop/R/HT/csv_files/ExperimentalWQ
Water_Set2 = read.csv("HT_WaterQuality_Experiment_Set2_021020.csv", head = T)
library(car)

Low_Set1 = Water_Set2[1:224,]
Amb_Set1 = Water_Set2[225:448,]
##make sure that all transformed variable are bound with the data table prior to analyses




##Temperature -- Set 2

##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temp~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(Temp~Treatment, data=Water_Set2) #significant 
##only need to do initial anova and K-W tests bc I'm only comparing one thing 

###Transformations 
#log
fit2 <-lm(t_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#sqrt
fit2 <-lm(t_sqrt2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#cube
fit2 <-lm(t_cube2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#recp 
fit2 <-lm(t_recp2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")


##Salinity - set 2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Sal~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(Sal~Treatment, data=Water_Set2) 

##transformations 
#log 
fit2 <-lm(s_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#cube
fit2 <-lm(s_cube2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 



##DO - set 2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(DO_mg~Treatment, data=Water_Set2)

##transformations 
#log 
fit2 <-lm(do_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#sqrt
fit2 <-lm(do_sqrt2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#cube
fit2 <-lm(do_cube2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#recp
fit2 <-lm(do_recp2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")




##pH - set 2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(spec_pH~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(spec_pH~Treatment, data=Water_Set2) 

##transformations
#log
fit2 <-lm(pH_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#sqrt
fit2 <-lm(pH_sqrt2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 




##TA - set 2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(TA~Treatment, data=Water_Set2) 

##transformations
#log
fit2 <-lm(TA_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#sqrt
fit2 <-lm(TA_sqrt2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#cube
fit2 <-lm(TA_cube2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#recp
fit2 <-lm(TA_recp2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")




##pCO2 - set 2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(pCO2~Treatment, data=Water_Set2) 

##transformations
#log
fit2 <-lm(CO_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#sqrt
fit2 <-lm(CO_sqrt2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#recp
fit2 <-lm(CO_recp2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 



##Omega Ca - set 2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(OmegaCa~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") #significant

kruskal.test(OmegaCa~Treatment, data=Water_Set2) 

##transformations
#log
fit2 <-lm(Ca_log2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#sqrt
fit2 <-lm(Ca_sqrt2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")

#cube
fit2 <-lm(Ca_cube2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III") 

#recp
fit2 <-lm(Ca_recp2~Treatment, na.action = na.exclude, data=Water_Set2)
summary(fit2)
require(car)
Anova(fit2, type="III")








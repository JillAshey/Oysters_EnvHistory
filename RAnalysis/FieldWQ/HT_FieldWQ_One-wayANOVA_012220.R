#One-way ANOVA for field water quality


#Set up working directory.  For me, the R directory is in My Documents in a folder titled "R data files"
#Data file must be .csv with only 1 row of column headings. No other text. Categorical variable columns must contain no numbers.

setwd("/Users/jillashey/Desktop")

library(gdata);library(multcomp);library(nlme);library(car);library(MASS)

library(lme4)
library(FSA)


###01/22/20
###Using 'trial' data frame with all the different transformed WQ variables, as established in R script with normality of WQ variables 
trial$Site<-as.factor(trial$Site)

MR = trial[1:10,]
PR = trial[11:20,]
UC = trial[21:30,]
VP = trial[31:39,]
VP2 = trial[40:44,]

AllSite = rbind(MR,PR,UC,VP)
MR_VP2 = rbind(MR, VP2)
PR_VP2 = rbind(PR,VP2)
UC_VP2 = rbind(UC,VP2)
VP_VP2 = rbind(VP, VP2)

########Original Sites

##Temperature 

##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
#not significant

kruskal.test(Temperature~Site, data=AllSite)
#not significant

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(t_sqrt~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")

fit2 <-lm(t_cube~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")



##Salinity 
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Salinity~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)

kruskal.test(Salinity~Site, data=AllSite)
dunnTest(Salinity~Site, data=AllSite) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Slog~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test_sal <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test_sal)


##DO mg
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(DO_mg~Site, data=AllSite)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(DOsqrt~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")

fit2 <-lm(DOcube~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")



##TA
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(TA~Site, data=AllSite)
dunnTest(TA~Site, data=AllSite) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(TAsqrt~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(TAcube~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)


##spec pH
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(specpH~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(specpH~Site, data=AllSite)
dunnTest(specpH~Site, data=AllSite) ###Not sure what method to use - automatically fills in Holm method 


##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pHlog~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHsqrt~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHcube~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##pCO2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(pCO2~Site, data=AllSite)
dunnTest(pCO2~Site, data=AllSite) ###Not sure what method to use - automatically fills in Holm method 


##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pCO2log~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2cube~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2recp~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Omega Ca
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Omega_Ca~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Omega_Ca~Site, data=AllSite)
dunnTest(TA~Site, data=AllSite) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Calog~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Casqrt~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Cacube~Site, na.action = na.exclude, data=AllSite)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)




###For VP2 and MR
#Temperature
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Temperature~Site, data=MR_VP2)
dunnTest(Temperature~Site, data=MR_VP2) ##incorrect # of dimensions

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(t_sqrt~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(t_cube~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Salinity
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Salinity~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Salinity~Site, data=MR_VP2)
dunnTest(Salinity~Site, data=MR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Slog~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##DO mg
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(DO_mg~Site, data=MR_VP2)
dunnTest(DO_mg~Site, data=MR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(DOsqrt~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(DOcube~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##spec pH
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(specpH~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(specpH~Site, data=MR_VP2)
dunnTest(specpH~Site, data=MR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pHlog~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHsqrt~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHcube~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##TA
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(TA~Site, data=MR_VP2)
dunnTest(TA~Site, data=MR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(TAsqrt~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(TAcube~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)


##pCO2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(pCO2~Site, data=MR_VP2)
dunnTest(pCO2~Site, data=MR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pCO2log~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2cube~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2recp~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Omega Ca
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Omega_Ca~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Omega_Ca~Site, data=MR_VP2)
dunnTest(TA~Site, data=MR_VP2) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Calog~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Casqrt~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Cacube~Site, na.action = na.exclude, data=MR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)






##PR and VP2
#Temperature
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Temperature~Site, data=PR_VP2)
dunnTest(Temperature~Site, data=PR_VP2) ##incorrect # of dimensions

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(t_sqrt~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(t_cube~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Salinity 
##Salinity
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Salinity~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Salinity~Site, data=PR_VP2)
dunnTest(Salinity~Site, data=PR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Slog~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##DO mg
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(DO_mg~Site, data=PR_VP2)
dunnTest(DO_mg~Site, data=PR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(DOsqrt~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(DOcube~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##spec pH
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(specpH~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(specpH~Site, data=PR_VP2)
dunnTest(specpH~Site, data=PR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pHlog~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHsqrt~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHcube~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##TA
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(TA~Site, data=PR_VP2)
dunnTest(TA~Site, data=PR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(TAsqrt~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(TAcube~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##pCO2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(pCO2~Site, data=PR_VP2)
dunnTest(pCO2~Site, data=PR_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pCO2log~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2cube~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2recp~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Omega Ca
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Omega_Ca~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Omega_Ca~Site, data=PR_VP2)
dunnTest(TA~Site, data=PR_VP2) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Calog~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Casqrt~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Cacube~Site, na.action = na.exclude, data=PR_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)







##UC and VP2
#Temperature
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Temperature~Site, data=UC_VP2)
dunnTest(Temperature~Site, data=UC_VP2) ##incorrect # of dimensions

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(t_sqrt~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(t_cube~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Salinity 
##Salinity
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Salinity~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Salinity~Site, data=UC_VP2)
dunnTest(Salinity~Site, data=UC_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Slog~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##DO mg
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(DO_mg~Site, data=UC_VP2)
dunnTest(DO_mg~Site, data=UC_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(DOsqrt~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(DOcube~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##spec pH
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(specpH~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(specpH~Site, data=UC_VP2)
dunnTest(specpH~Site, data=UC_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pHlog~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHsqrt~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHcube~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##TA
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(TA~Site, data=UC_VP2)
dunnTest(TA~Site, data=UC_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(TAsqrt~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(TAcube~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##pCO2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(pCO2~Site, data=UC_VP2)
dunnTest(pCO2~Site, data=UC_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pCO2log~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2cube~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2recp~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Omega Ca
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Omega_Ca~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Omega_Ca~Site, data=UC_VP2)
dunnTest(TA~Site, data=UC_VP2) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Calog~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Casqrt~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Cacube~Site, na.action = na.exclude, data=UC_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)




##VP and VP2
#Temperature
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Temperature~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Temperature~Site, data=VP_VP2)
dunnTest(Temperature~Site, data=VP_VP2) ##incorrect # of dimensions

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(t_sqrt~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(t_cube~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Salinity 
##Salinity
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Salinity~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Salinity~Site, data=VP_VP2)
dunnTest(Salinity~Site, data=VP_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Slog~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##DO mg
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(DO_mg~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(DO_mg~Site, data=VP_VP2)
dunnTest(DO_mg~Site, data=VP_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(DOsqrt~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(DOcube~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##spec pH
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(specpH~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(specpH~Site, data=VP_VP2)
dunnTest(specpH~Site, data=VP_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pHlog~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHsqrt~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pHcube~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##TA
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(TA~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(TA~Site, data=VP_VP2)
dunnTest(TA~Site, data=VP_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(TAsqrt~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(TAcube~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##pCO2
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(pCO2~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(pCO2~Site, data=VP_VP2)
dunnTest(pCO2~Site, data=VP_VP2)

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(pCO2log~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2cube~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(pCO2recp~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

##Omega Ca
##First, I will do the ANOVA test and the Kruskal-Wallis test (both using the not transformed variable) to compare results 
fit2 <-lm(Omega_Ca~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

kruskal.test(Omega_Ca~Site, data=VP_VP2)
dunnTest(TA~Site, data=VP_VP2) ###Not sure what method to use - automatically fills in Holm method 

##Second, I will do the ANOVA test using the transformed variables to compare them to the untransformed 
fit2 <-lm(Calog~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Casqrt~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)

fit2 <-lm(Cacube~Site, na.action = na.exclude, data=VP_VP2)
summary(fit2)
require(car)
Anova(fit2, type="III")
test <- glht(fit2, linfct = mcp(Site = "Tukey"))
summary(test)












###done during 2019
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

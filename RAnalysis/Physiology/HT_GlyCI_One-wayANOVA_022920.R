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


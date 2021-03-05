##T-tests
##Honors thesis - manuscript 
#4/23/20

#Student's t-test for one sample: use when you have one measurement variable and a theoretical expectation of what the mean should be under null
#In this case, I am using t-tests to assess if the changes in phys. variables differed from 0
#Theoretical mean = 0; observed mean = avg of phys variable by site 
#Null hypothesis is that the mean change of phys variable is equal to 0
#If t-test is significant, can reject null--the mean change in phys. variable is significantly different from 0

##Only doing this for the change in phys. variables, so only need set 2 data
#Read in data set w/ delta glycogen and CI
dat2 = read.csv(file="PhysWQData_HT_SET2_Oysters.csv", header=TRUE)
View(dat2)
dat2 = cbind(dat2, delta_gly_log)



##Subset full data set into datasets by site 
MR2 = dat2[c(1:9),]
PR2 = dat2[c(10:24),]
UC2 = dat2[c(25:31),]
VP2 = dat2[c(32:40),]


##Set 2 - change in glycogen t-tests
#Do t-tests by site, as we will compare among sites 
t.test(MR2$delta_gly_log) #not significant 
t.test(PR2$delta_gly_log) #significant
t.test(UC2$delta_gly_log) #not significant
t.test(VP2$delta_gly_log) #not significant
#so PR oysters were the only ones to significantly change from 0 during common garden conditions

##Set 2 - change in condition index t-tests
t.test(MR2$Delta_CI) #significant
t.test(PR2$Delta_CI) #significant
t.test(UC2$Delta_CI) #significant
t.test(VP2$Delta_CI) #not significant 
#MR, PR, UC oysters all significantly changed from 0 during CG conditions 







frap<-read.csv("FRAP_HT.csv", head=T)
View(frap)

frap1 = frap[1:56,]
frap2 = frap[57:112,]
frap2

MR2 = frap2[c(1:7, 29:35),]
PR2 = frap2[c(8:14, 36:42),]
UC2 = frap2[c(15:21, 43:49),]
VP2 = frap2[c(22:28, 50:56),]


###Wait to combine the two treatments

##T-test without combining treatments 
##Set 2 - change in FRAP t-tests
t.test(MR2$test_FRAP_Delta) #not significant 
t.test(PR2$test_FRAP_Delta) #significant
t.test(UC2$test_FRAP_Delta) #not significant
t.test(VP2$test_FRAP_Delta) #not significant 




##t.test(observed, mu=theoretical, conf.int=0.95)
#In this case, I only need to put in the observed values because the function's default is mu=0 and conf.int=0.95



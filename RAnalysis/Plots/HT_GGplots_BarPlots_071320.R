##GGplots for honors thesis into manuscript
#040220



##Set 1
#reading in data
dat1 = read.csv(file="~/Desktop/Coding/R/HT/csv_files/Physiology_WQ/HT_PhysWQ_Set1_051620.csv", header=TRUE) #file is now likely named HT_PhysWQ_Set1_051620
View(dat1)

#need to run ggplot
install.packages("devtools")
install.packages("ggpubr")
library(devtools)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library("plotrix")
# install.packages("devtools")---- didn't work once 
devtools::install_github("tidyverse/reprex")

##Set 1 - Glycogen
# calculate the glycogen average and SE to include in data frame to be put into ggplot

MR = subset(dat1, Site=="Merroir")
MR_gly_mean = mean(MR$P_Glycogen_DW)
MR_gly_se = std.error(MR$P_Glycogen_DW)

PR = subset(dat1, Site=="Piankatank")
PR_gly_mean = mean(PR$P_Glycogen_DW)
PR_gly_se = std.error(PR$P_Glycogen_DW)

UC = subset(dat1, Site=="Urbana Creek")
UC_gly_mean = mean(UC$P_Glycogen_DW)
UC_gly_se = std.error(UC$P_Glycogen_DW)

VP = subset(dat1, Site=="VIMS Pier")
VP_gly_mean = mean(VP$P_Glycogen_DW)
VP_gly_se = std.error(VP$P_Glycogen_DW)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set1Gly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(MR_gly_mean, PR_gly_mean, UC_gly_mean, VP_gly_mean), # mean
                     se = c(MR_gly_se, PR_gly_se, UC_gly_se, VP_gly_se)) # standard error

#Bar plot for glycogenSet1
p1 = ggplot(set1Gly, aes(x=name, y=value)) + #establishing what's going on the plot
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) + #its going to be a bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bars in specific size, shape, color, etc
  ylab("Glycogen (% of DW)") + xlab("") + #x and y axes titles 
  ggtitle("Glycogen - Set1") + #title 
  scale_y_continuous(expand = c(0,0)) + #making the bars sit right on the x axis-- no white space bt the bars and the actual axis
  theme_minimal() + #setting the background theme
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels
p1
# no significance 

##Set 1 - Condition index
# calculate the CI average and SE to include in data frame to be put into ggplot

MR_CI_mean = mean(MR$ConditionIndex)
MR_CI_se = std.error(MR$ConditionIndex)

PR_CI_mean = mean(PR$ConditionIndex)
PR_CI_se = std.error(PR$ConditionIndex)

UC_CI_mean = mean(UC$ConditionIndex)
UC_CI_se = std.error(UC$ConditionIndex)

VP_CI_mean = mean(VP$ConditionIndex)
VP_CI_se = std.error(VP$ConditionIndex)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set1CI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(MR_CI_mean, PR_CI_mean, UC_CI_mean, VP_CI_mean), # mean
                    se = c(MR_CI_se, PR_CI_se, UC_CI_se, VP_CI_se)) #std error
                       
## bar plot for CI set1
p2 = ggplot(set1CI, aes(x=name, y=value)) + #establishing what's going on the plot
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) + # bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bar in specific size, shape, color, etc
  geom_text(label = c("A", "A", "A", "B"), aes(y = c(0.1, 0.1, 0.1, 1.0), x = name), size = 4) + # adding letters to indicate significant differences 
  ylab("Condition Index") + xlab("") + # titles for x and y axes
  ggtitle("Condition Index - Set1") + # figure title 
  scale_y_continuous(expand = c(0,0)) + #making the bars sit right on the x axis-- no white space bt the bars and the actual axis
  theme_minimal() + #setting the background theme
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels
p2
# significance labels: A, A, A, B





###########################################################################

#Set 2

#reading in data
dat2 = read.csv(file="~/Desktop/Coding/R/HT/csv_files/Physiology_WQ/HT_PhysWQ_Set2_051620.csv", header=TRUE) #file is now likely named HT_PhysWQ_Set1_051620
View(dat2)


##Glycogen set 2
# calculate glycogen set 2 mean and se from dat2
MR2 = subset(dat2, Site=="Merroir")
MR2_gly_mean = mean(MR2$P_Glycogen_DW)
MR2_gly_se = std.error(MR2$P_Glycogen_DW)

PR2 = subset(dat2, Site=="Piankatank")
PR2_gly_mean = mean(PR2$P_Glycogen_DW)
PR2_gly_se = std.error(PR2$P_Glycogen_DW)

UC2 = subset(dat2, Site=="Urbana Creek")
UC2_gly_mean = mean(UC2$P_Glycogen_DW)
UC2_gly_se = std.error(UC2$P_Glycogen_DW)

VP2 = subset(dat2, Site=="VIMS Pier")
VP2_gly_mean = mean(VP2$P_Glycogen_DW)
VP2_gly_se = std.error(VP2$P_Glycogen_DW)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set2Gly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(MR2_gly_mean, PR2_gly_mean, UC2_gly_mean, VP2_gly_mean), # mean
                     se = c(MR2_gly_se, PR2_gly_se, UC2_gly_se, VP2_gly_se)) # std error 

#bar plot for glycogen set 2
ggplot(set2Gly, aes(x=name, y=value)) + # establishes whats going on in plot 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.75) + # bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bar in specific size, shape, color, etc
  ylab("Glycogen Content") + xlab("") + # titles for x and y axes
  ggtitle("Glycogen - Set2") + # figure title 
  scale_y_continuous(expand = c(0,0)) + # making the bars sit flush with the x axis
  theme_minimal() + # setting background theme
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels


##Delta glycogen
#Getting averages of specific physiological variable
MR2_delta_gly_mean = mean(MR2$Delta_Glycogen)
MR2_delta_gly_se = std.error(MR2$Delta_Glycogen)

PR2_delta_gly_mean = mean(PR2$Delta_Glycogen)
PR2_delta_gly_se = std.error(PR2$Delta_Glycogen)

UC2_delta_gly_mean = mean(UC2$Delta_Glycogen)
UC2_delta_gly_se = std.error(UC2$Delta_Glycogen)

VP2_delta_gly_mean = mean(VP2$Delta_Glycogen)
VP2_delta_gly_se = std.error(VP2$Delta_Glycogen)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set2DeltaGly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(MR2_delta_gly_mean, PR2_delta_gly_mean, UC2_delta_gly_mean, VP2_delta_gly_mean), # mean
                     se = c(MR2_delta_gly_se, PR2_delta_gly_se, UC2_delta_gly_se, VP2_delta_gly_se))

#Bar plot of delta glycogen
p3 = ggplot(set2DeltaGly, aes(x=name, y=value)) + # establishes whats going on in plot 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.75) + # bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bar in specific size, shape, color, etc
  #geom_text(label = c("A", "A", "A", "A"), aes(y = c(1.5, 7.3, 4.0, 11.0), x = name), size = 6) +
  ylab("Change in Glycogen Content") + xlab("") + # titles for x and y axes
  ggtitle("Delta Glycogen") + # figure title 
  geom_hline(aes(yintercept = 0)) + # making the bars sit flush with the x axis
  theme_minimal() + # setting background theme
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels
#do I need to include the letter designations in a case where nothing is significant? -- NO
p3


#Condition index set2
##calculate CI set 2 mean and se from dat2
MR2_CI_mean = mean(MR2$ConditionIndex)
MR2_CI_se = std.error(MR2$ConditionIndex)

PR2_CI_mean = mean(PR2$ConditionIndex)
PR2_CI_se = std.error(PR2$ConditionIndex)

UC2_CI_mean = mean(UC2$ConditionIndex)
UC2_CI_se = std.error(UC2$ConditionIndex)

VP2_CI_mean = mean(VP2$ConditionIndex)
VP2_CI_se = std.error(VP2$ConditionIndex)


#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set2CI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(MR2_CI_mean, PR2_CI_mean, UC2_CI_mean, VP2_CI_mean), # mean
                    se = c(MR2_CI_se, PR2_CI_se, UC2_CI_se, VP2_CI_se)) # std error

#Bar plot of CI set2
ggplot(set2CI, aes(x=name, y=value)) + # establishing whats going on in plot
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) +  # bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bar in specific size, shape, color, etc
  ylab("Condition Index") + xlab("") + # x and y axes titles 
  ggtitle("Condition Index Set2") + # figure title 
  scale_y_continuous(expand = c(0,0)) + # making the bars sit flush with the x axis
  theme_minimal() + # setting background theme 
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels


##Delta CI
#Getting averages and std error of specific physiological variable
MR2_delta_CI_mean = mean(MR2$Delta_CI)
MR2_delta_CI_se = std.error(MR2$Delta_CI)

PR2_delta_CI_mean = mean(PR2$Delta_CI)
PR2_delta_CI_se = std.error(PR2$Delta_CI)

UC2_delta_CI_mean = mean(UC2$Delta_CI)
UC2_delta_CI_se = std.error(UC2$Delta_CI)

VP2_delta_CI_mean = mean(VP2$Delta_CI)
VP2_delta_CI_se = std.error(VP2$Delta_CI)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set2DeltaCI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                          value = c(MR2_delta_CI_mean, PR2_delta_CI_mean, UC2_delta_CI_mean, VP2_delta_CI_mean), # mean
                         se = c(MR2_delta_CI_se, PR2_delta_CI_se, UC2_delta_CI_se, VP2_delta_CI_se))
                            
# Bar plot of delta CI
p4 = ggplot(set2DeltaCI, aes(x=name, y=value)) + # establishing whats going on in plot
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) + # bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bar in specific size, shape, color, etc
  geom_text(label = c("A", "B", "AB", "C"), aes(y = c(0.1, 0.1, 0.1, 1.0), x = name), size = 4) + # adding letters to indicate significance 
  ylab("Change in Condition Index") + xlab("") + # x and y axes titles
  ggtitle("Delta Condition Index") + # figure title 
  geom_hline(aes(yintercept = 0)) + # making the bars sit flush with the x axis
  theme_minimal() + # setting background theme
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels
p4
#signifance indicators: A B AB C


##putting graphs into grid format by physiological variable
# Glycogen - gly set1 and delta gly
ggarrange(p1, p3, labels = c("A", "B"))
ggsave("~/Desktop/HT_gly_Set1_delta_plot", device = "png", width = 6, height = 7, dpi = 500)

# Glycogen - gly set1 and delta gly
ggarrange(p2, p4, labels = c("A", "B"))
ggsave("~/Desktop/HT_CI_Set1_delta_plot", device = "png", width = 6, height = 7, dpi = 500)


#####FRAP ggplots 

##FRAP - set 1
##only need the averages for the plots 
#Load data file with only avg and SE for each site. If I need all the points I can use HT_FRAP_bothSets_042320.csv
FRAP <- read.csv("~/Desktop/Coding/R/HT/csv_files/FRAP/HT_FRAP_Siteavgs_043020.csv")
View(FRAP)
#gives me this warning when I load data file:
# Warning message:
#   In read.table(file = file, header = header, sep = sep, quote = quote,  :
#                   incomplete final line found by readTableHeader on '~/Desktop/Coding/R/HT/csv_files/FRAP/HT_FRAP_Siteavgs_043020.csv'
#not sure what it means, but did not affect plots

##FRAP - set 1
f_p1 = ggplot(data = FRAP, aes(x=Site, y=FRAPavg1)) +
  geom_bar(stat = "identity", position= "dodge", width = 0.75) +
  labs(y="FRAP", x="") +
  ggtitle("FRAP Set1") +
  geom_errorbar(aes(ymin = FRAPavg1 - FRAPse1, ymax = FRAPavg1 + FRAPse1), width = 0.2, position = position_dodge(0.75), size = 0.5) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))


##FRAP - set 2
ggplot(data = FRAP, aes(x=Site, y=FRAPavg2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  geom_errorbar(aes(ymin = FRAPavg2 - FRAPse2, ymax = FRAPavg2 + FRAPse2), width = 0.2, position = position_dodge(0.75), size = 0.5) +
  labs(x="", y="FRAP") + 
  ggtitle("FRAP Set2") +
  scale_y_continuous(expand = c(0,0)) + 
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle = 30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, colour = "black"), axis.title.y = element_text(size = 15))


##delta FRAP
f_p2 = ggplot(data = FRAP, aes(x=Site, y=dFRAPavg,
                            ymin = dFRAPavg - dFRAPse, ymax = dFRAPavg + dFRAPse)) + 
  geom_bar(stat = "identity", position= "dodge", width = 0.75) +
  labs(y="Change in FRAP", x="") + 
  ggtitle("Delta FRAP") +
  geom_errorbar(width = 0.2, position = position_dodge(0.75), size = 0.5) +
  geom_text(label = c("A", "B", "AB", "A"), aes(y = dFRAPavg + dFRAPse, x = Site), vjust = -0.5, size = 6) +
  scale_y_continuous(expand = c(0,0)) + 
  geom_hline(aes(yintercept = 0)) +
  ylim(-0.4, 0.3) +
  theme_minimal() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))
#A B AB A

##putting graphs into grid format 
ggarrange(f_p1, f_p2, labels = c("A", "B"))
ggsave("~/Desktop/HT_FRAP_Set1_delta_plot", device = "png", width = 6, height = 7, dpi = 500)




























## old plots

#Bar plot of delta glycogen
p1 = ggplot(set2DeltaGly, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", position= "dodge", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  ylab("Change in glycogen \n (% of dry weight)") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  expand_limits(y = c(-10, 12.5)) +
  theme_classic() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

p1

#mean and se for delta CI all sites
set2DeltaCI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                         value = c(-0.8040568, -1.8464727, -1.1765024, 0.4116201),
                         se = c(0.215639, 0.1631159, 0.4379649, 0.2947394))

#bar plot for delta CI
p2 = ggplot(set2DeltaCI, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", position= "dodge", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("a", "b", "ab", "c"), aes(y = c(0.18, 0.18, 0.18, 0.9), x = name), size = 4) +
  ylab("Change in condition index") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

p2

#p2 + scale_y_continuous(breaks = c()) -- if i need to, I can adjust # of tick marks on y scale

##delta FRAP
set2DeltaFRAP = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                         value = c(-0.012521733, -0.278934451, -0.041996053, 0.063943088),
                         se = c(0.06417983, 0.053300707, 0.038891367, 0.099359929))

p3 = ggplot(data = set2DeltaFRAP, aes(x=name, y=value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("a", "b", "ab", "a"), aes(y = c(0.12, 0.05, 0.05, 0.23), x = name), size = 4) +
  geom_hline(aes(yintercept = 0)) +
  ylim(-0.4, 0.4) +
  ylab(expression(~ paste("Change in total antioxidant potential ", "(", mu, "mol/g wet tissue"^-1, ")"))) +
  theme_classic() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

p3

   # ~ paste("Change in total antioxidant potential", "(", mu, "mol/g wet tissue"^-1, ")"))) +
  #    atop("Change in total antioxidant potential", paste("(", mu, "mol/g wet tissue"^-1, ")")))) +



##putting graphs into grid format 
ggarrange(p1, p2, p3, nrow = 2, ncol = 2, widths = c(2,2), labels = c("A", "B", "C"), label.x = c(0.22, 0.16, 0.23)) 
ggsave("plots_HT_071220", device = "png", width = 6, height = 7, dpi = 500)




 


###~~~~
#manipulating plots for presentation for Putnam lab June 2020
#delta glycogen 
#Bar plot of delta glycogen
gly_presentation_plot = ggplot(set2DeltaGly, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", position= "dodge", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  ylab("Change in Glycogen 
(% of Dry Weight)") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 14, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 14))

gly_presentation_plot = gly_presentation_plot + expand_limits(y = c(-10, 12.5))
gly_presentation_plot


#Delta CI - bar plot
#bar plot for delta CI
#mean and se for delta CI all sites
set2DeltaCI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                         value = c(-0.8040568, -1.8464727, -1.1765024, 0.4116201),
                         se = c(0.215639, 0.1631159, 0.4379649, 0.2947394))

CI_presentation_plot = ggplot(set2DeltaCI, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", position= "dodge", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("a", "b", "ab", "a"), aes(y = c(0.18, 0.18, 0.18, 0.9), x = name), size = 4) +
  ylab("Change in Condition Index") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 14, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 14))

CI_presentation_plot

##Going to be using the bar plot by treatment for the presentation to Putnam lab
FRAPtest = read.csv(file="FRAP_test_050620.csv", header=TRUE)

##delta FRAP
FRAP_presentation_plot = ggplot(data = FRAPtest, aes(x=Site, y=dFRAPavg, fill=Treatment, 
                            ymin = dFRAPavg - dFRAPse, ymax = dFRAPavg + dFRAPse)) +
  geom_bar(stat = "identity", position= "dodge", color = "black", width = 0.75) +
  labs(y="Change in total antioxidants", x="") + 
  geom_errorbar(width = 0.2, position = position_dodge(0.75), size = 0.5) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 14, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 14))
FRAP_presentation_plot = FRAP_presentation_plot + scale_fill_brewer(palette = "Blues")
FRAP_presentation_plot  

















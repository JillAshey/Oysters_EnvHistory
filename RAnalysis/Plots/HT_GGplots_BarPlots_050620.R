##GGplots for honors thesis into manuscript
#040220



##Set 1
#reading in data
dat1 = read.csv(file="DistalSites_PhysWQ_HT_030220.csv", header=TRUE)
View(dat1)

#need to run ggplot
install.packages("devtools")
library(devtools)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
# install.packages("devtools")---- didn't work once 
devtools::install_github("tidyverse/reprex")

##Set 1 - Glycogen
#can calculate the average to include in data frame to be put into ggplot
MR = dat1[c(1:9),]
MR_gly = mean(MR$P_Glycogen_DW)

PR = dat1[c(10:18),]
PR_gly = mean(PR$P_Glycogen_DW)

UC = dat1[c(19:25),]
UC_gly = mean(UC$P_Glycogen_DW)

VP = dat1[c(26:34),]
VP_gly = mean(VP$P_Glycogen_DW)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
set1Gly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(10.32668, 14.05932, 11.67275, 8.374433), 
                     se = c(1.740338098, 1.385031994, 2.218250416, 1.211052225))

ggplot(set1Gly, aes(x=name, y=value)) + #establishing what's going on the plot
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) + #its going to be a bar chart in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bar in specific size, shape, color, etc
  ylab("Glycogen (% of DW)") + xlab("") + #giving titles to x and y axes
  scale_y_continuous(expand = c(0,0)) + #making the bars sit right on the x axis-- no white space bt the bars and the actual axis
  theme_minimal() + #setting the background theme
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))
   #adding specific lines, sizes, degrees, etc for the axes texts and lables

##Set 1 - Condition index
#data frame with site, avg physiological measurement by site, and standard error by site 
set1CI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(6.042470414, 7.031203234, 7.05234666, 4.1938246), 
                    se = c(0.341051722, 0.379352077, 0.279384989, 0.192476457))
                       
##creating bar plot for CI
ggplot(set1CI, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) + 
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  ylab("Condition Index") + xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))






###########################################################################

#Set 2
#reading in data
dat2 = read.csv(file="PhysWQData_HT_SET2_Oysters.csv", header=TRUE)
View(dat2)

##calculate glycogen set 2 means from dat2
#Could do it in R or on calculator im tired so i think i just did it on a calculator and put values into dataframe below
##Actually got avg and SE for all sites from excel files

#Glycogen set 2
#dataframe for glycogen set 2 values
set2Gly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(10.34265279, 18.67306882, 12.85837149, 14.56094623),
                     se = c(0.785516438, 1.606846784, 2.352532531, 4.832673059))


#bar plot for glycogen set 2
ggplot(set2Gly, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  ylab("Glycogen Content") + xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))


###Delta glycogen
#Getting averages of specific physiological variable
MR2 = dat2[c(1:9),]
MR2_deltagly = mean(MR2$Delta_Glycogen)

PR2 = dat2[c(10:24),] 
PR2_deltagly = mean(PR2$Delta_Glycogen)

UC2 = dat2[c(25:31),]
UC2_deltagly = mean(UC2$Delta_Glycogen)

VP2 = dat2[c(32:40),] 
VP2_deltagly = mean(VP2$Delta_Glycogen)

#dataframe with delta glycogen values in it
set2DeltaGly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(0.01597742, 4.613753, 1.185624348, 6.051577), 
                     se = c(0.785516438, 1.606846784, 2.352532531, 4.477085325))

#Bar plot of delta glycogen
p1 = ggplot(set2DeltaGly, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("A", "A", "A", "A"), aes(y = c(1.5, 7.3, 4.0, 11.0), x = name), size = 6) +
  ylab("Change in Glycogen Content") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_minimal() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))
#do I need to include the letter designations in a case where nothing is significant?



set2CI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                     value = c(5.238413649, 5.184730541, 5.875844211, 4.605444734), 
                    se = c(0.215639035, 0.163115924, 0.437964907, 0.29473935))
                    
ggplot(set2CI, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  ylab("Condition Index") + xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))


set2DeltaCI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                          value = c(-0.8040568, -1.8464727, -1.1765024, 0.4116201),
                         se = c(0.215639, 0.1631159, 0.4379649, 0.2947394))
                            

p2 = ggplot(set2DeltaCI, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("A", "B", "AB", "C"), aes(y = c(0.1, 0.1, 0.1, 1.0), x = name), size = 4) +
  ylab("Change in Condition Index") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_minimal() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))

#A B AB C


##putting graphs into grid format 
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 2, nrow=2, labels = c("A", "B", "C"))

ggarrange(p1, p2, p3, labels = c("A", "B", "C"), ncol = 2, nrow =2)


#####FRAP ggplots 

##FRAP - set 1
##only need the averages for the plots 
#Load data file with only avg and SE for each site
FRAPtest = read.csv(file="~/Desktop/Statistics/R_stuff/R data files/HT/csv_files/FRAP/FRAP_test.csv", header=TRUE)
View(FRAPtest)
#gives me this warning when I load data file:incomplete final line found by readTableHeader on 'FRAP_avg.csv'
#not sure what it means, but did not affect plots

##FRAP - set 1
ggplot(data = FRAPtest, aes(x=Site, y=FRAPavg1)) +
  geom_bar(stat = "identity", position= "dodge", width = 0.75) +
  labs(y="FRAP", x="")+ 
  geom_errorbar(aes(ymin = FRAPavg1 - FRAPse1, ymax = FRAPavg1 + FRAPse1), width = 0.2, position = position_dodge(0.75), size = 0.5) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))


##FRAP - set 2
ggplot(data = FRAPtest, aes(x=Site, y=FRAPavg2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  geom_errorbar(aes(ymin = FRAPtest$FRAPavg2 - FRAPtest$FRAPse2, ymax = FRAPtest$FRAPavg2 + FRAPtest$FRAPse2), width = 0.2, position = position_dodge(0.75), size = 0.5) +
  labs(x="", y="FRAP") + 
  scale_y_continuous(expand = c(0,0)) + 
  theme_minimal() +
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle = 30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, colour = "black"), axis.title.y = element_text(size = 15))


##delta FRAP
ggplot(data = FRAPtest, aes(x=Site, y=dFRAPavg,
                            ymin = dFRAPavg - dFRAPse, ymax = dFRAPavg + dFRAPse)) +
  geom_bar(stat = "identity", position= "dodge", width = 0.75) +
  labs(y="Change in FRAP", x="") + 
  geom_errorbar(width = 0.2, position = position_dodge(0.75), size = 0.5) +
  geom_text(label = c("A", "B", "AB", "A"), aes(y = dFRAPavg + dFRAPse, x = Site), vjust = -0.5, size = 6) +
  scale_y_continuous(expand = c(0,0)) + 
  geom_hline(aes(yintercept = 0)) +
  ylim(-0.4, 0.3) +
  theme_minimal() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))

#A B AB A

p3 = ggplot(data = FRAPtest, aes(x=Site, y=dFRAPavg,
                            ymin = dFRAPavg - dFRAPse, ymax = dFRAPavg + dFRAPse)) +
  geom_bar(stat = "identity", position= "dodge", width = 0.75) +
  labs(y="Change in FRAP", x="") + 
  geom_errorbar(width = 0.2, position = position_dodge(0.75), size = 0.5) +
  geom_text(label = c("A", "B", "AB", "A"), aes(y = c(0.10, 0.05, 0.05, 0.21), x = Site), size = 6) +
  scale_y_continuous(expand = c(0,0)) + 
  geom_hline(aes(yintercept = 0)) +
  ylim(-0.4, 0.3) +
  theme_minimal() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15))






#####Delta graphs and arranging them on one panel 
###Delta glycogen
#Getting averages of specific physiological variable
MR2 = dat2[c(1:9),]
MR2_deltagly = mean(MR2$Delta_Glycogen)

PR2 = dat2[c(10:24),] 
PR2_deltagly = mean(PR2$Delta_Glycogen)

UC2 = dat2[c(25:31),]
UC2_deltagly = mean(UC2$Delta_Glycogen)

VP2 = dat2[c(32:40),] 
VP2_deltagly = mean(VP2$Delta_Glycogen)

#dataframe with delta glycogen values in it
set2DeltaGly = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                          value = c(0.01597742, 4.613753, 1.185624348, 6.051577), 
                          se = c(0.785516438, 1.606846784, 2.352532531, 4.477085325))

#Bar plot of delta glycogen
p1 = ggplot(set2DeltaGly, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", position= "dodge", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  ylab("Change in Glycogen 
(% of Dry Weight)") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

p1 = p1 + expand_limits(y = c(-10, 12.5))
p1
#do I need to include the letter designations in a case where nothing is significant?

#mean and se for delta CI all sites
set2DeltaCI = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                         value = c(-0.8040568, -1.8464727, -1.1765024, 0.4116201),
                         se = c(0.215639, 0.1631159, 0.4379649, 0.2947394))

#bar plot for delta CI
p2 = ggplot(set2DeltaCI, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", position= "dodge", fill="gray", color = "black", size = 0.3, width = 0.75) +
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("a", "b", "ab", "a"), aes(y = c(0.18, 0.18, 0.18, 0.9), x = name), size = 4) +
  ylab("Change in Condition Index") + xlab("") +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

p2

g =   theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

#p2 + scale_y_continuous(breaks = c()) -- if i need to, I can adjust # of tick marks on y scale

##delta FRAP
p3 = ggplot(data = FRAPtest, aes(x=Site, y=dFRAPavg, ymin = dFRAPavg - dFRAPse, ymax = dFRAPavg + dFRAPse)) +
  geom_bar(stat = "identity", position = "dodge", fill = "gray", color = "black", size = 0.3, width = 0.75) +
  labs(y="Change in total antioxidant potential", x="") + 
  geom_errorbar(position = position_dodge(0.75), width = 0.1, alpha = 0.9, size = 0.5) +
  geom_text(label = c("a", "b", "ab", "a"), aes(y = c(0.12, 0.05, 0.05, 0.23), x = Site), size = 4) +
  geom_hline(aes(yintercept = 0)) +
  ylim(-0.4, 0.4) +
  theme_classic() +
  theme(axis.line.y = element_line(), axis.text.x = element_text(size = 10, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=8, color = "black"), axis.title.y = element_text(size = 10))

p3

##putting graphs into grid format 
install.packages("gridExtra")
library(gridExtra)

ggarrange(p1, p2, p3, nrow = 2, ncol = 2, widths = c(2,2), labels = c("A", "B", "C"), label.x = c(0.22, 0.16, 0.23)) 
ggsave("testplots_HT", device = "png", width = 6, height = 7, dpi = 500)




 


###~~~~
#manipulating plots for presentation 
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

















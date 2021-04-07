# Title: Shell Hieght Plotting 
# Project: Oyster Env Histrory 
# Author: J. Ashey
# Date: 20210325

# Load packages
library(devtools)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library("plotrix")
# install.packages("devtools")---- didn't work once 
devtools::install_github("tidyverse/reprex")


# Read in data
SH <- read.csv("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/data/Growth/HT_InitialShellHeight_022219.csv", header = T)

# Subset by distal shell heights 
distal.SH <- subset(SH, EnvHist == "Distal")

# Calculate mean and std error 
MR <- subset(distal.SH, Site=="Merroir")
MR_SH_mean <- mean(MR$Initial_Shell)
MR_SH_se <- std.error(MR$Initial_Shell)

PR <- subset(distal.SH, Site=="Piankatank")
PR_SH_mean = mean(PR$Initial_Shell)
PR_SH_se = std.error(PR$Initial_Shell)

UC <- subset(distal.SH, Site=="Urbanna Creek")
UC_SH_mean <- mean(UC$Initial_Shell)
UC_SH_se <- std.error(UC$Initial_Shell)

VP <- subset(distal.SH, Site=="VIMS Pier")
VP_SH_mean <- mean(VP$Initial_Shell)
VP_SH_se <- std.error(VP$Initial_Shell)

#make a data frame with the name (categorical variable - Site), the values (average of physiological measurements from each site),
#and se (standard error for each site)
distal.SH_summary = data.frame(name=c("Merroir", "Piankatank", "Urbanna Creek", "VIMS Pier"), 
                    value = c(MR_SH_mean, PR_SH_mean, UC_SH_mean, VP_SH_mean), # mean
                    se = c(MR_SH_se, PR_SH_se, UC_SH_se, VP_SH_se)) #std error

# Makr bar plot for initial shell height 
p1 = ggplot(distal.SH_summary, aes(x=name, y=value)) + #establishing what's going on the plot
  geom_bar(stat = "identity", fill="gray", color = "black", size = 0.3, width = 0.5) + #its going to be a bar plot in these sizes, shapes, colors, etc
  geom_errorbar(aes(x=name, ymin = value - se, ymax = value + se), colour = "black", width = 0.1, alpha = 0.9, size = 0.5) + #error bars in specific size, shape, color, etc
  ylab("Initial Shell Height (mm)") + xlab("") + #x and y axes titles 
  #ggtitle("") + #title 
  scale_y_continuous(expand = c(0,0)) + #making the bars sit right on the x axis-- no white space bt the bars and the actual axis
  theme_minimal() + #setting the background theme
  theme(axis.line = element_line(), axis.text.x = element_text(size = 12, angle=30, hjust = 1, colour = "black"), axis.text.y = element_text(size=12, color = "black"), axis.title.y = element_text(size = 15)) #adding specific lines, sizes, degrees, etc for the axes texts and labels
p1

ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/InitialShellHeight.jpeg", p1)





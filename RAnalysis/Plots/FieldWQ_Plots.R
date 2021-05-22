# Title: Field WQ Plotting 
# Project: Oyster Env Histrory 
# Author: J. Ashey
# Date: 20210325

#Install and Load necessary libraries
library(devtools)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library("plotrix")
# install.packages("devtools")---- didn't work once 
#devtools::install_github("tidyverse/reprex")
if("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 
if("emmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('emmeans') 
if("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
library(lubridate)
library(tidyverse)
library(emmeans)
library(gridExtra)

# Read in data 
WQ <- read.csv("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/data/WQ/HT_FieldWaterQuality.csv", header = T)

# Format data 
WQ$Date <- parse_date(WQ$Date, "%m/%d/%y")

## Plot WQ variables 
Temps <- ggplot(WQ, aes(x=Date, y=Temperature)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("Temperature (°C)") +
  theme_linedraw()
Temps

Sal <- ggplot(WQ, aes(x=Date, y=Salinity)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("Salinity (psu)") +
  theme_linedraw()
Sal

pH <- ggplot(WQ, aes(x=Date, y=specpH)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("pH Total") +
  theme_linedraw()

pH

DO <- ggplot(WQ, aes(x=Date, y=DO_mg)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("Dissolved oxygen (mg/mL)") + 
  theme_linedraw()
DO

TA <- ggplot(WQ, aes(x=Date, y=TA)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("Total alkalinity (μmol/kg)") +
  theme_linedraw()
TA

pCO2 <- ggplot(WQ, aes(x=Date, y=pCO2)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("pCO2 (μatm)") +
  theme_linedraw()
pCO2

Ca <- ggplot(WQ, aes(x=Date, y=Omega_Ca)) +
  geom_line(aes(color = Site), size = 1)+
  scale_color_manual(values = c("mediumpurple3", "darkorange2", "palegreen3", "dodgerblue2", "goldenrod2")) +
  ylab("Ω Calcite") +
  theme_linedraw()
Ca

# Save plots
# as pdfs
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_Temperature.pdf", plot = Temps, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_Salinity.pdf", plot = Sal, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_pH.pdf", plot = pH, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_DO.pdf", plot = DO, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_TA.pdf", plot = TA, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_pCO2.pdf", plot = pCO2, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_OmegaCa.pdf", plot = Ca, width = 8, height = 4)

# as pngs
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_Temperature.png", plot = Temps, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_Salinity.png", plot = Sal, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_pH.png", plot = pH, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_DO.png", plot = DO, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_TA.png", plot = TA, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_pCO2.png", plot = pCO2, width = 8, height = 4)
ggsave("~/Desktop/PutnamLab/Repositories/Oysters_EnvHistory/output/FieldWQ/Field_OmegaCa.png", plot = Ca, width = 8, height = 4)



FRAP=read.table("FRAP_HT.csv", header=T, sep=",")
FRAP$Treatment<-as.factor(FRAP$Treatment)

##Need to 3D plot change in FRAP distal data by their WQs at their originial sites
#so need to add columns into FRAP data sheet 


FRAP_Set2=read.table("FRAP_HT_Set2.csv", header=T, sep=",")
FRAP_Set2$Treatment<-as.factor(FRAP_Set2$Treatment)

##For distal sites, chose salinity, temp, pH, and omega Ca to move forward with
plot(Delta_FRAP~Distal_Sal, FRAP_Set2, col = Site) #significantly different bt sites
plot(Delta_FRAP~Distal_Temp, FRAP_Set2, col = Site)
plot(Delta_FRAP~Distal_pH, FRAP_Set2, col = Site)
plot(Delta_FRAP~Distal_Omega_Ca, FRAP_Set2, col = Site)

#just for kicks
plot(Delta_FRAP~Distal_TA, FRAP_Set2, col = Site) #significantly different bt sites


##now need to plot the proximal WQ against one another 
CG_WQ=read.table("CommonGarden_HT_WQ_csv.csv", header=T, sep=",")

#plots of all WQ variables against one another for common garden WQ - trying to see if there are correlations so that 
#I can remove some variables bc they are redundant 
plot(Temperature~Salinity, CG_WQ) 

plot(Temperature~pH, CG_WQ)

plot(Temperature~Dissolved.oxygen, CG_WQ) #redundant

plot(Temperature~Total.alkalinity, CG_WQ)

plot(Temperature~pCO2, CG_WQ)

plot(Temperature~Omega.Ca, CG_WQ) #redundant

plot(Temperature~Omega.Ar, CG_WQ) #redundant

plot(Salinity~pH, CG_WQ)

plot(Salinity~Dissolved.oxygen, CG_WQ)

plot(Salinity~Total.alkalinity, CG_WQ) #sort of redundant

plot(Salinity~pCO2, CG_WQ)

plot(Salinity~Omega.Ca, CG_WQ)

plot(Salinity~Omega.Ar, CG_WQ)

plot(pH~Dissolved.oxygen, CG_WQ)

plot(pH~Total.alkalinity, CG_WQ)

plot(pH~pCO2, CG_WQ) #sort of redundant

plot(pH~Omega.Ca, CG_WQ) #redundant 

plot(pH~Omega.Ar, CG_WQ) #redundant

plot(Dissolved.oxygen~Total.alkalinity, CG_WQ)

plot(Dissolved.oxygen~pCO2, CG_WQ)

plot(Dissolved.oxygen~Omega.Ca, CG_WQ)

plot(Dissolved.oxygen~Omega.Ar, CG_WQ)


plot(Total.alkalinity~pCO2, CG_WQ)

plot(Total.alkalinity~Omega.Ca, CG_WQ)

plot(Total.alkalinity~Omega.Ar, CG_WQ)

plot(pCO2~Omega.Ca, CG_WQ)

plot(pCO2~Omega.Ar, CG_WQ)

plot(Omega.Ca~Omega.Ar, CG_WQ) #use omega Ca, but it is redundant with some above 


##Now plotting the physiological variable (FRAP) against proximal WQ variables 
##Not super helpful, as oysters from all sites are now in same water
plot(Delta_FRAP~Proximal_Sal, FRAP_Set2, col = Site)
plot(Delta_FRAP~Proximal_Temp, FRAP_Set2, col = Site)
plot(Delta_FRAP~Proximal_pH, FRAP_Set2, col = Site)
plot(Delta_FRAP~Proximal_Omega_Ca, FRAP_Set2, col = Site)
plot(Delta_FRAP~Proximal_TA, FRAP_Set2, col = Site)
plot(Delta_FRAP~Proximal_pCO2, FRAP_Set2, col = Site)
plot(Delta_FRAP~Proximal_DO_mg, FRAP_Set2, col = Site)

##Have to decide which variables to keep and which are redundant 



###Now plot in 3D - 2 WQ variables + FRAP
library(plotly)






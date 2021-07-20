#clear working environment, code below
rm(list=ls())
getwd()

#######pH total scale
library(seacarb)

###Change directory
setwd("~/Desktop/Oysters_Honors_Thesis/CarbChemR")

z=read.csv("pH_VIMS_Meso.csv", header=T, sep=",", na.string="NA")

z <- na.omit(z)
class(z)
attach(z)

phT<-pHinsi(pH=SpecpH, ALK=TA/1000000, Tinsi=T_ysi, Tlab=T_spec, Pinsi=0, S=S, Pt=0, Sit=0, k1k2 = "x", kf = "x", ks="d", pHscale = "T", b="u74", eos = "teos10", long = 1e+20, lat = 1e+20)
phT   
  
carbo=carb(flag=8, var1=phT, var2=TA/1000000, S= S, T=T_ysi, P=0, Pt=0, Sit=0,pHscale="T", kf="pf", k1k2="l", ks="d")

mesochemistry=cbind(Date, Sample, carbo)
 mesochemistry

write.table(mesochemistry,"MesoCarbChem.csv",sep=",",row.names=FALSE)
detach(z)
rm(list=ls())

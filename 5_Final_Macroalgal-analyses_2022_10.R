#Macroalgal survey data merged with eReefs data
# 2022 February - Final
# RUN in R Studio version  1.1.423 - © 2009-2018 RStudio, Inc.
# Run in R 3.4.3


library(abt)
library(vegan)
library(doBy)


setwd("C:\\Users\\kfabrici\\OneDrive - Australian Institute of Marine Science\\Documents2\\_Eco-RRAP\\Macroalgae\\KF_Macroalgae_GBR spatial\\MA-analyses")

mdat2<-read.csv("C:\\Users\\kfabrici\\OneDrive - Australian Institute of Marine Science\\Documents2\\_Eco-RRAP\\Macroalgae\\KF_Macroalgae_GBR spatial\\MA-analyses\\mdat2_2021_09.csv")



max.ma.per.reef<-summaryBy(total.ma~reef+shelf,data=mdat2,FUN=max)
dim(max.ma.per.reef)
sort(max.ma.per.reef$total.ma.max)
124/165   #25% of reefs had at least one site with >49% MA
(165-124)/165
(165-135)/165    #18% of reefs had at least one site with >49% MA
max.ma.per.reef.I<-max.ma.per.reef[max.ma.per.reef$shelf=="I",]
dim(max.ma.per.reef.I)          #there are 94 inshore reefs
sort(max.ma.per.reef.I$total.ma.max)   #of these, 56 reefs had <=50% MA, 66 reefs had <=0% MA
(94-56)/94   #40.4% of inshore reefs had >=50%  MA
(94-66)/94   #29.8% of inshore reefs had >=70%

max.ma.per.reef.MO<-max.ma.per.reef[max.ma.per.reef$shelf!="I",]
dim(max.ma.per.reef.MO)          #there are 71 mid/outer reefs
sort(max.ma.per.reef.MO$total.ma.max)   #of these, 3 reefs had >=50% MA, 1 reefs had >=70% MA
3/71   #4.2% of MO reefs had >=50%  MA
1/94   #1.0% of MO reefs had >=70%



mdat3<-mdat2
names(mdat3)[7]<-"Depth"
names(mdat3)[26]<-"Hard.coral"
names(mdat3)[27]<-"Octocoral"
names(mdat3)[15]<-"Across"
names(mdat3)[55]<-"Tidal.Range"
names(mdat3)[56]<-"Total.Alkalinity"
names(mdat3)[20]<-"Wave.Exposure"
names(mdat3)[61]<-"Temperature"
names(mdat3)[66]<-"PAR"
names(mdat3)[76]<-"Aragonite.Saturation"
names(mdat3)[80]<-"Secchi.Depth"

#write.csv(mdat3,file="MA_Surveys_eReefs_Final_data 2022_02.csv")




# Spatial and benthos abt models : both strong
#Final spatial and biotic model:
z3<-abt(total.ma~across+Latitude+depth.mn+hc+sc,data=mdat2,interaction.depth = 3,var.monotone = c(-1,1,-1,-1,-1),n.trees = 4000) 
z1<-abt(total.ma~across+Latitude+depth.mn+hc+sc,data=mdat2,interaction.depth = 1,var.monotone = c(-1,1,-1,-1,-1),n.trees = 4000) 
z2<-abt(total.ma~across+Latitude+depth.mn+hc+sc,data=mdat2,interaction.depth = 2,var.monotone = c(-1,1,-1,-1,-1),n.trees = 4000) 
z4<-abt(total.ma~across+Latitude+depth.mn+hc+sc,data=mdat2,interaction.depth = 4,var.monotone = c(-1,1,-1,-1,-1),n.trees = 4000)
z5<-abt(total.ma~across+Latitude+depth.mn+hc+sc,data=mdat2,interaction.depth = 5,var.monotone = c(-1,1,-1,-1,-1),n.trees = 4000)
loss(z1);loss(z2);loss(z3);loss(z4);loss(z5)
# [1] 236.8651
# [1] 188.6101
# [1] 177.6608   -- three-way interaction is best
# [1] 174.4944
# [1] 171.0011


# #spatial model without benthos, pruned to main spatial variables: 
##Fig 3:

z2<-abt(total.ma~Across+Latitude+Depth,data=mdat3,interaction.depth = 3,var.monotone = c(-1,1,-1),n.trees = 4000)  # FINAL MODEL
plot.err(z2)  # 4000 trees are more than enough
windows(hei=6,wid=6.5); par(oma=c(2.2,0,0,0),mar=c(1,4,0.5,0.5),las=1,cex=0.95);
plot.all(z2)

  
windows(hei=2.5,wid=8)  ## 3D plots  - FINAL FIG 
par(mar=c(0,2,0.5,0.5),las=1,cex=3,mfrow=c(1,3), pty="s")
plot(z2,c(1,2),ptype="p",col=col5)  #acr vs Latlegend("topleft",legend="a",cex=1.25,bty="n")
plot(z2,c(3,2),ptype="p")  #depth vs Lat
legend("topleft",legend="b",cex=1.25,bty="n")
plot(z2,c(1,3),ptype="p")  #depth vs acr
legend("topleft",legend="c",cex=1.25,bty="n")





###################
# Environmental models: 


# All 26 predictors:
# names(mdat2)
# z2<-abt(total.ma ~ wave+rugos+visib+slope+flow+sedim+Temp.mn+Sal.mn+Alk.mn+DIC.mn+DIN.mn+Mud.all.mn+Chl.all.mn+PAR3.2.mn+PAR.mn+PAR.SuWI.range+omAr.mn+pH.mn+pCO2.mn+omCalc.mn+FineSed.4rt+tide.range+Kd.mn+Turb.mn+Secchi.mn+NH4.mn+OC.mn,data=mdat2,interaction.depth = 3,n.trees = 3000)
# windows(hei=9,wid=9); par(oma=c(2.2,0,0,0),mar=c(1,4,0.5,0.5),las=1,cex=0.95)
# plot.all(z2,col.term="black",se=2,col.se="black",col.res = "gray75",pch.res =16, partial.resid = T,lty.se=1,lwd.term=2,lwd.se=1.5,ylab="")
#  


# #Final model after step-wise elimination of lowest predictors:
##FINAL FIGURE 4:   
z1<-abt(total.ma ~ Wave.Exposure+Temperature+Total.Alkalinity+PAR+Aragonite.Saturation+Tidal.Range+Secchi.Depth,data=mdat3,interaction.depth = 1,var.monotone = c(1,-1,-1,1,-1,1,-1),n.trees = 10000,shrinkage = 0.001) #Final model

windows(hei=9,wid=9.5)   
par(oma=c(0,15,0,0),mar=c(4,2,0.5,0.5),las=1,cex=1.2,mfrow=c(3,3), pty="s")
plot.all(z1,col.term="black",se=2,col.se="black",col.res = "gray75",pch.res =16, partial.resid = T,lty.se=1,lwd.term=2,lwd.se=1.5,ylab="")


#with latitude added (to check, not used):

z1<-abt(total.ma ~ Wave.Exposure+Temperature+Total.Alkalinity+PAR+Aragonite.Saturation+Tidal.Range+Secchi.Depth+Latitude,data=mdat3,interaction.depth = 1,var.monotone = c(1,-1,-1,1,-1,1,-1,1),n.trees = 10000,shrinkage = 0.001) #Final model

windows(hei=9,wid=9.5) 
par(oma=c(0,15,0,0),mar=c(4,2,0.5,0.5),las=1,cex=1.2,mfrow=c(3,3), pty="s")
plot.all(z1,col.term="black",se=2,col.se="black",col.res = "gray75",pch.res =16, partial.resid = T,lty.se=1,lwd.term=2,lwd.se=1.5,ylab="")

#with Across added (to check, not used):

z1<-abt(total.ma ~ Wave.Exposure+Temperature+Total.Alkalinity+PAR+Aragonite.Saturation+Tidal.Range+Secchi.Depth+Across,data=mdat3,interaction.depth = 1,var.monotone = c(1,-1,-1,1,-1,1,-1,-1),n.trees = 10000,shrinkage = 0.001) #Final model

windows(hei=9,wid=9.5) 
par(oma=c(0,15,0,0),mar=c(4,2,0.5,0.5),las=1,cex=1.2,mfrow=c(3,3), pty="s")
plot.all(z1,col.term="black",se=2,col.se="black",col.res = "gray75",pch.res =16, partial.resid = T,lty.se=1,lwd.term=2,lwd.se=1.5,ylab="")

graphics.off()




###################################################
###################################################
# Multivariate analyses

library(vegan)
library(veganFuns)
source("C:\\Users\\kfabrici\\OneDrive - Australian Institute of Marine Science\\Documents2\\_Eco-RRAP\\Macroalgae\\KF_Macroalgae_GBR spatial\\MA-analyses\\plotRDA.R")

col3a=c("#d7301f", "#fdcc8a",  "black")  
col3aa=c("#d7301f", "#fc8d59",  "#fdcc8a")  #Used for Fig 5 - not used here
names(mdat3)
summary(mdat3)

mdatRDA<-mdat3[,c(26:29,32,15,88,7,20,55:58,61,80,66,72,75,76,3)]
names(mdatRDA)
names(mdatRDA)[1:20]<-c("Hard.Coral", "Octocoral", "Turf Algae", "Coralline.Algae", "Macroalgae", "Across", "Latitude", "Depth", "Wave.Exposure","Tidal.Range","Total.Alkalinity","DIC","DIN","Temperature","Secchi.Depth","PAR", "Chlorophyll","pCO2","Aragonite.Saturation","Shelf")

#write.csv(mdatRDA,file="mdatRDA.csv")

#FINAL TABLE 1:
zrda2<-rda(mdatRDA[,c(1:5)]^0.5~Across+Latitude+Depth+Wave.Exposure+Tidal.Range+Secchi.Depth+PAR+Total.Alkalinity+DIC+DIN+Temperature+Aragonite.Saturation+pCO2,data=mdatRDA) 
anova.cca(zrda2,by="margin",permu=1000,model="reduced")   

plot(zrda2)

#FINAL FIGURE 6:
windows(hei=8,wid=9.5);par(mar=c(4,4,0.5,0.5),cex=1,las=1)   
plot1 = plotRDA(zrda2,keep.sp=c(vec=1,name=1),display=c("sp","wa", "bp"),scaling=2, cext=1, cexp=1.5, col=c("black", "transparent", "blue","black"),
                bgsites=c("transparent","transparent","transparent")[as.numeric(mdatRDA$reef)],
                type.site = c("p"), type.con = "p", 
                type.cen = "t", type.bi = "t", 
                xlab = paste("RDA1   ",round((zrda2$CA$eig[1]/sum(zrda2$CA$eig))*100,digits=2),"%"),
                ylab = paste("RDA2   ",round((zrda2$CA$eig[2]/sum(zrda2$CA$eig))*100,digits=2),"%"))
points(zrda2, pch=c(2,1,8,17,16)[as.factor((mdatRDA$Depth))],col=col3a[as.numeric(mdatRDA$Shelf)], cex=((mdatRDA$Macroalgae+40)/70))
ordihull(zrda2, mdatRDA$Depth,scaling=2, col=col5, lty=c(1,1,2,4,4),lwd=2,label=T,cex=1.5)
legend("topleft",legend=levels(as.factor(mdatRDA$Depth)),pch=c(2,1,8,17,16),col=col5,pt.cex=1.5,bty="n",title="Depth (m):")
legend("topleft",legend=levels(as.factor(mdatRDA$Depth)),col=col5,bty="n",lwd=2,lty=c(1,1,2,4,4),inset=c(0.08,0.03))
legend("topright",legend=c("  Inshore","  Midshelf","  Outershelf"),pch=16,col=col3a,pt.cex=1.5,bty="n",title="Shelf:")


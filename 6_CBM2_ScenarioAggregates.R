
rm(list=ls())
setwd("~/1.CEDAR/3_Studies !!/23-CBM2/3-Code")
source('AggScenarios6.R')

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)
library(tcltk)
#library(tcltk2)

setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012_England") #source folder

todos<-list.files(pattern="[0-9].csv")
todos <-c('baseline.csv',todos)
nfiles <-length(todos)

############   CALCULATE from BASELINE: Individuals
bl <- read.csv('baseline.csv')
bl.indiv <- as.data.frame(unique(bl$ID))
colnames(bl.indiv) <- 'ID'

#transforms MainMode_B04ID >> to our own modes
lookup <- data.frame(MainMode_B04ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13),modefinal=c(1,2,3,4,3,7,5,5,5,6,6,7,7))


########## DATA FRAMES for RESULTS (individuals added) -- 16 NEW MEASURES BY INDIVIDUAL
carMiles   <- bl.indiv         # no. 1
carMilesR   <-bl.indiv

carMilesCycled  <- bl.indiv
milesCycled.pers  <- bl.indiv

METh           <- bl.indiv
METhincr      <- bl.indiv
MMETh       <- bl.indiv

CO2.Tm      <- bl.indiv
CO2.R       <- bl.indiv    #new 01-NOV

TripDisIncSW    <- bl.indiv
TripTotalTime1  <- bl.indiv

timeSaved.Total.h   <- bl.indiv

newcyclists       <- bl.indiv
potential.cyclist   <- bl.indiv
trips.bike.perc   <- bl.indiv

mode.travel <- bl.indiv      # no. 16  - 01-NOV

listaDF<-list(carMiles=carMiles,carMilesR=carMilesR,carMilesCycled=carMilesCycled,
              milesCycled.pers=milesCycled.pers,METh=METh,METhincr=METhincr,MMETh=MMETh,
              CO2.Tm=CO2.Tm,CO2.R=CO2.R,
              TripDisIncSW =TripDisIncSW,TripTotalTime1=TripTotalTime1,timeSaved.Total.h=TripTotalTime1,
              newcyclists=newcyclists,potential.cyclist=potential.cyclist,trips.bike.perc=trips.bike.perc)   
              #16 data frames

#################### CALCULATE from BASELINE: f.carMiles0 - f.METh0 - f.TripTotalTime0........

carMiles0 <- sqldf ('SELECT ID,sum([TripDisIncSW]) as [carMiles0] FROM bl WHERE [bl].[MainMode_B04ID] IN (3,4,5,12) GROUP BY ID')
carMiles0 <- left_join(bl.indiv, carMiles0, by='ID')

carMilesR0 <- sqldf ('SELECT ID,0 as [carMilesR0] FROM bl GROUP BY ID')
#carMilesR0 <- left_join(bl.indiv, carMilesR0, by='ID')

METh0 <-  sqldf ('SELECT ID, sum(METh) AS METh0 FROM bl GROUP BY ID')
TripTotalTime0 <- sqldf ('SELECT ID, sum(TripTotalTime) AS TripTotalTime0 FROM bl GROUP BY ID')


############### LOOP all SCENARIOS................

for (i1 in (1:nfiles)) {  #reading scenarios for aggregates
#  for (i1 in (1:3)) {  #reading scenarios for aggregates
  
  #DATA AGGREGATES
  AggScenarios6(todos[i1])
  
  #ifelse(i1==1,df <-info,df<-rbind(df,info))   #consolidates content
  
  if ((i1%%1)==0) {message('no files: ',i1)}
  
                      }  #END main loop



listaDF <-lapply(listaDF, function(x) colnames(x)[2:length(x)] <-todos[1:length(x)-1] )

colnames(carMiles)[2:length(carMiles)] <- todos[1:(length(carMiles)-1)]
colnames(carMilesR)[2:length(carMilesR)] <- todos[1:(length(carMilesR)-1)]
colnames(carMilesCycled)[2:length(carMilesCycled)] <- todos[1:(length(carMilesCycled)-1)]
colnames(milesCycled.pers)[2:length(milesCycled.pers)] <- todos[1:(length(milesCycled.pers)-1)]
colnames(METh)[2:length(METh)] <- todos[1:(length(METh)-1)]

colnames(METhincr)[2:length(METhincr)] <- todos[1:(length(METhincr)-1)]
colnames(MMETh)[2:length(MMETh)] <- todos[1:(length(MMETh)-1)]
colnames(CO2.Tm)[2:length(CO2.Tm)] <- todos[1:(length(CO2.Tm)-1)]
colnames(CO2.R)[2:length(CO2.R)] <- todos[1:(length(CO2.R)-1)]

colnames(TripDisIncSW)[2:length(TripDisIncSW)] <- todos[1:(length(TripDisIncSW)-1)]
colnames(TripTotalTime1)[2:length(TripTotalTime1)] <- todos[1:(length(TripTotalTime1)-1)]
colnames(timeSaved.Total.h)[2:length(timeSaved.Total.h)] <- todos[1:(length(timeSaved.Total.h)-1)]

colnames(newcyclists)[2:length(newcyclists)] <- todos[1:(length(newcyclists)-1)]
colnames(potential.cyclist)[2:length(potential.cyclist)] <- todos[1:(length(potential.cyclist)-1)]
colnames(trips.bike.perc)[2:length(trips.bike.perc)] <- todos[1:(length(trips.bike.perc)-1)]

colnames(mode.travel)[2:length(mode.travel)] <- todos[1:(length(mode.travel)-1)]


########### ANADIR COLNAMES Y WRITE.CSV MEDIANTE LAPPLY ############ 
lapply(listaDF,function(x) write.csv(listaDF[x],file=paste(names(listaDF[x]),".csv",sep="")))

for (i in (1:length(listaDF))) {
              names(listaDF[[i]]) <- todos[1:length(listaDF[[i]]) ]
                                }



setwd("./aggregates") #source folder

write.csv(carMiles,file="carMiles.csv")
write.csv(carMilesR,file="carMilesR.csv")
write.csv(carMilesCycled,file="carMilesCycled.csv")
write.csv(milesCycled.pers,file="milesCycled.pers.csv")
write.csv(METh,file="METh.csv")

write.csv(METhincr,file="METhincr.csv")
write.csv(MMETh,file="MMETh.csv")
write.csv(CO2.Tm,file="CO2.Tm.csv")
write.csv(CO2.R,file="CO2.R.csv")

write.csv(TripDisIncSW,file="TripDisIncSW.csv")
write.csv(TripTotalTime1,file="TripTotalTime1.csv")
write.csv(timeSaved.Total.h,file="timeSaved.Total.h.csv")

write.csv(newcyclists,file="newcyclists.csv")
write.csv(potential.cyclist,file="potential.cyclist.csv")
write.csv(trips.bike.perc,file="trips.bike.perc.csv")

write.csv(mode.travel,file="mode.travel.csv")




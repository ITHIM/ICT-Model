
#rm(list=ls())
#setwd("~/1.CEDAR/3_Studies !!/23-CBM2/3-Code")
source("init.R")
source('AggScenarios6.R')

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)
library(tcltk)
#library(tcltk2)

#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012_England") #source folder

# todos<-list.files(pattern="[0-9].csv")
# todos <-c('baseline.csv',todos)
# nfiles <-length(todos)

############   CALCULATE from BASELINE: Individuals
# bl <- read.csv('baseline.csv')
bl <- read.csv('bl2012_18_84ag_sw.csv', header=T, as.is = T)
bl.indiv <- data.frame(ID = unique(bl$ID))
colnames(bl.indiv) <- 'ID'

#transforms MainMode_B04ID >> to our own modes
lookup <- data.frame(MainMode_B04ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13),modefinal=c(1,2,3,4,3,7,5,5,5,6,6,7,7))


########## DATA FRAMES for RESULTS (individuals added) -- 16 NEW MEASURES BY INDIVIDUAL
carMiles <- bl.indiv         # no. 1
carMilesR <- bl.indiv

carMilesCycledAggr <- bl.indiv
milesCycled.pers <- bl.indiv

METh <- bl.indiv
METhincr <- bl.indiv
MMETh <- bl.indiv
health_mmets <- bl.indiv
PA_mmets <- bl.indiv

CO2.Tm <- bl.indiv
CO2.R <- bl.indiv    #new 01-NOV

TripDisIncSW <- bl.indiv
TripTotalTime1 <- bl.indiv

timeSaved.Total.h <- bl.indiv

newcyclists <- bl.indiv
potential.cyclist <- bl.indiv
trips.bike.perc <- bl.indiv

mode.travel <- bl.indiv      # no. 16  - 01-NOV

listaDF<-list(carMiles = carMiles, carMilesR = carMilesR,carMilesCycledAggr = carMilesCycledAggr,
              milesCycled.pers = milesCycled.pers, METh = METh, METhincr = METhincr, MMETh = MMETh,
              CO2.Tm = CO2.Tm, CO2.R = CO2.R, TripDisIncSW = TripDisIncSW, TripTotalTime1 = TripTotalTime1,
              timeSaved.Total.h = TripTotalTime1, newcyclists = newcyclists, potential.cyclist =  potential.cyclist,
              trips.bike.perc = trips.bike.perc)   
#16 data frames

#################### CALCULATE from BASELINE: f.carMiles0 - f.METh0 - f.TripTotalTime0........

carMiles0 <- sqldf ('SELECT bl.ID,sum(bl.[TripDisIncSW]) as [carMiles0] FROM bl WHERE [bl].[MainMode_B04ID] IN (3,4,5,12) GROUP BY bl.ID')
carMiles0 <- left_join(bl.indiv, carMiles0, by='ID')

carMilesR0 <- sqldf ('SELECT ID,0 as [carMilesR0] FROM bl GROUP BY ID')
#carMilesR0 <- left_join(bl.indiv, carMilesR0, by='ID')

METh0 <-  sqldf ('SELECT bl.ID, sum(bl.METh) AS METh0 FROM bl GROUP BY bl.ID')
TripTotalTime0 <- sqldf ('SELECT bl.ID, sum(bl.TripTotalTime) AS TripTotalTime0 FROM bl GROUP BY bl.ID')


############### LOOP all SCENARIOS................

#for (i1 in (1:nfiles)) {  #reading scenarios for aggregates

AggScenarios6(bl, "baseline")

for (i1 in 1:length(listOfScenarios)) {

  #DATA AGGREGATES
  # AggScenarios6(todos[i1])
  
  
  #DATA AGGREGATES
  tbl <- bl
  sc <- get(as.character(listOfScenarios[i1]) )
  tbl$now_cycle <- sc$now_cycle
  tbl$ebike <- sc$ebike
  tbl$cyclist <- sc$cyclist
  tbl$METh <- sc$METh
  tbl$MMETh <- sc$MMETh
  tbl$TripTotalTime1 <- sc$TripTotalTime1
  #   "now_cycle"      "ebike"          "cyclist"        "METh"           "MMETh"         
  #   "TripTotalTime1"
  
  AggScenarios6(tbl, as.character(listOfScenarios[i1]))
  
  if ((i1%%1)==0) {
    
    message('no files: ',i1)
  }
  
  
}  #END main loop



#listaDF <-lapply(listaDF, function(x) colnames(x)[2:length(x)] <- listOfScenarios[1:length(x)-1] )


colnames(carMiles)[3:length(carMiles)] <- listOfScenarios
colnames(carMilesR)[3:length(carMilesR)] <- listOfScenarios
colnames(carMilesCycledAggr)[3:length(carMilesCycledAggr)] <- listOfScenarios
colnames(milesCycled.pers)[3:length(milesCycled.pers)] <- listOfScenarios
colnames(METh)[3:length(METh)] <- listOfScenarios

colnames(METhincr)[3:length(METhincr)] <- listOfScenarios
colnames(MMETh)[3:length(MMETh)] <- listOfScenarios
colnames(CO2.Tm)[3:length(CO2.Tm)] <- listOfScenarios
# colnames(CO2.R)[2:length(CO2.R)] <- listOfScenarios[1:(length(CO2.R)-1)]

colnames(TripDisIncSW)[3:length(TripDisIncSW)] <- listOfScenarios
colnames(TripTotalTime1)[3:length(TripTotalTime1)] <- listOfScenarios
colnames(timeSaved.Total.h)[3:length(timeSaved.Total.h)] <- listOfScenarios

colnames(health_mmets)[3:length(health_mmets)] <- listOfScenarios
colnames(PA_mmets)[3:length(PA_mmets)] <- listOfScenarios

# colnames(newcyclists)[3:length(newcyclists)] <- listOfScenarios
# colnames(potential.cyclist)[3:length(potential.cyclist)] <- listOfScenarios
# colnames(trips.bike.perc)[3:length(trips.bike.perc)] <- listOfScenarios

# colnames(mode.travel)[3:length(mode.travel)] <- listOfScenarios


# ########### ANADIR COLNAMES Y WRITE.CSV MEDIANTE LAPPLY ############ 
# lapply(listaDF,function(x) write.csv(listaDF[x],file=paste(names(listaDF[x]),".csv",sep="")))
# 
# for (i in (1:length(listaDF))) {
#   names(listaDF[[i]]) <- todos[1:length(listaDF[[i]]) ]
# }
# 
# setwd("./aggregates") #source folder
# 
# write.csv(carMiles,file="carMiles.csv")
# write.csv(carMilesR,file="carMilesR.csv")
# write.csv(carMilesCycled,file="carMilesCycled.csv")
# write.csv(milesCycled.pers,file="milesCycled.pers.csv")
# write.csv(METh,file="METh.csv")
# 
# write.csv(METhincr,file="METhincr.csv")
# write.csv(MMETh,file="MMETh.csv")
# write.csv(CO2.Tm,file="CO2.Tm.csv")
# write.csv(CO2.R,file="CO2.R.csv")
# 
# write.csv(TripDisIncSW,file="TripDisIncSW.csv")
# write.csv(TripTotalTime1,file="TripTotalTime1.csv")
# write.csv(timeSaved.Total.h,file="timeSaved.Total.h.csv")
# 
# write.csv(newcyclists,file="newcyclists.csv")
# write.csv(potential.cyclist,file="potential.cyclist.csv")
# write.csv(trips.bike.perc,file="trips.bike.perc.csv")
# 
# write.csv(mode.travel,file="mode.travel.csv")
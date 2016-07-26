source("init.R")
source('AggScenarios6.R')

library(dplyr)
library(plyr)
library(stringr)
library(data.table)
library(sqldf)
library(tcltk)

############   CALCULATE from BASELINE: Individuals
bl <- readRDS('bl2014_p.Rds')  #needs to be bl2014_p.Rds or bl2014.Rds?

bl1 <- bl
bl1$HHoldGOR_B02ID <- 0
bl <- rbind(bl, bl1)

bl.indiv <- data.frame (unique(bl[c("ID", "HHoldGOR_B02ID")])) 
#sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID from bl GROUP BY bl.ID, bl.HHoldGOR_B02ID')

carMiles <- carMilesR <- carMilesCycledAggr <- 
  milesCycled.pers <- METh <- METhincr <- MMETh <- CO2.Tm <- 
  TripDisIncSW <- TripTotalTime1 <- 
  timeSaved.Total.h <- health_mmets <- PA_mmets <- bl.indiv

#transforms MainMode_B04ID >> to our own modes
lookup <- data.frame(MainMode_B04ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13),modefinal=c(1,2,3,4,3,7,5,5,5,6,6,7,7))
bl$MainMode_Reduced <- lookup$modefinal[match(bl$MainMode_B04ID, lookup$MainMode_B04ID)]
bl_backup <- bl


carMiles0 <- sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID, sum(bl.TripDisIncSW) as carMiles0 FROM bl WHERE bl.MainMode_B04ID IN (3,4,5,12) GROUP BY bl.ID, bl.HHoldGOR_B02ID')
#carMiles0 <- left_join(bl.indiv, carMiles0, by=c('ID, HHoldGOR_B02ID'))

carMilesR0 <- sqldf ('SELECT ID,HHoldGOR_B02ID, 0 as [carMilesR0] FROM bl GROUP BY ID, HHoldGOR_B02ID')
#carMilesR0 <- left_join(bl.indiv, carMilesR0, by='ID')

METh0 <-  sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID, sum(bl.METh) AS METh0 FROM bl GROUP BY bl.ID, bl.HHoldGOR_B02ID')

TripTotalTime0 <- sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID, sum(bl.TripTotalTime) AS TripTotalTime0 FROM bl GROUP BY bl.ID, bl.HHoldGOR_B02ID')


AggScenarios6(bl, "baseline")

for (i1 in 1:length(listOfScenarios)) {

  #DATA AGGREGATES
  tbl <- bl
  sc <- get(as.character(listOfScenarios[i1]) )
  
  # Temporary solution
  # Conider only first half of the scenario objects (as they're twice the size of baseline)
  # sc <- sc[1:(nrow(sc) / 2), ]
  
  tbl$now_cycle <- sc$now_cycle
  tbl$ebike <- sc$ebike
  tbl$cyclist <- sc$cyclist
  tbl$METh <- sc$METh
  tbl$MMETh <- sc$MMETh
  tbl$TripTotalTime1 <- sc$TripTotalTime1
  AggScenarios6(tbl, as.character(listOfScenarios[i1]))
  
  if ((i1%%1)==0) {
    
    message('no files: ',i1)
  }

}  #END main loop

#add baseline (1st colum of results)
local_listOfScenarios <- c('baseline', listOfScenarios)

colnames(carMiles)[2:length(carMiles)] <- local_listOfScenarios
colnames(carMilesR)[2:length(carMilesR)] <- local_listOfScenarios
colnames(carMilesCycledAggr)[2:length(carMilesCycledAggr)] <- local_listOfScenarios
colnames(milesCycled.pers)[2:length(milesCycled.pers)] <- local_listOfScenarios
colnames(METh)[2:length(METh)] <- local_listOfScenarios

colnames(METhincr)[2:length(METhincr)] <- local_listOfScenarios
colnames(MMETh)[2:length(MMETh)] <- local_listOfScenarios
colnames(CO2.Tm)[2:length(CO2.Tm)] <- local_listOfScenarios
# colnames(CO2.R)[2:length(CO2.R)] <- local_listOfScenarios[1:(length(CO2.R)-1)]

colnames(TripDisIncSW)[2:length(TripDisIncSW)] <- local_listOfScenarios
colnames(TripTotalTime1)[2:length(TripTotalTime1)] <- local_listOfScenarios
colnames(timeSaved.Total.h)[2:length(timeSaved.Total.h)] <- local_listOfScenarios

colnames(health_mmets)[2:length(health_mmets)] <- local_listOfScenarios
colnames(PA_mmets)[2:length(PA_mmets)] <- local_listOfScenarios
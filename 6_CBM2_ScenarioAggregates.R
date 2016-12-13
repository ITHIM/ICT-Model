source("init.R")
source('AggScenarios6.R')

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)
library(tcltk)

############   CALCULATE from BASELINE: Individuals
#bl <- readRDS('bl2014_p.rds')  #needs to be bl2014_p.Rds 
bl <- readRDS('bl2014_p_v2.rds')

# For some reason the age_group is not correctly recorded in the baseline line
# Recalculate the age_group variable
# Read nts age group lookup table
ag_lookup <- read.csv("nts-adjusted-age-groups.csv", header = T, as.is = T)
# Create a new variable 'age_group' for baseline, which converts numeric age categories into age ranges
bl$age_group <- ag_lookup$age[match(bl$Age_B01ID, ag_lookup$nts_group)]

bl1 <- bl
bl1$HHoldGOR_B02ID <- 0
bl <- rbind(bl, bl1)

rm(bl1)

bl.indiv <- sqldf("Select ID, Sex_B01ID, age_group, EthGroupTS_B02ID, NSSec_B03ID, HHoldGOR_B02ID from bl Group by ID,
HHoldGOR_B02ID")

carMiles <- carMilesR <- carMilesCycledAggr <- 
  milesCycled.pers <- METh <- METhincr <- MMETh <- CO2.Tm <- 
  TripDisIncSW <- TripTotalTime1 <- 
  timeSaved.Total.h <- health_mmets <- PA_mmets <- bl.indiv

#transforms MainMode_B04ID >> to our own modes
lookup <- data.frame(MainMode_B04ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13),modefinal=c(1,2,3,4,3,7,5,5,5,6,6,7,7))
bl$MainMode_Reduced <- lookup$modefinal[match(bl$MainMode_B04ID, lookup$MainMode_B04ID)]

# carMiles0 <- sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID, sum(bl.TripDisIncSW) as carMiles0 FROM bl WHERE bl.MainMode_B04ID IN (3,4,5,12) GROUP BY bl.ID, bl.HHoldGOR_B02ID')
#carMiles0 <- left_join(bl.indiv, carMiles0, by=c('ID, HHoldGOR_B02ID'))

# carMilesR0 <- sqldf ('SELECT ID,HHoldGOR_B02ID, 0 as [carMilesR0] FROM bl GROUP BY ID, HHoldGOR_B02ID')
#carMilesR0 <- left_join(bl.indiv, carMilesR0, by='ID')

# METh0 <-  sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID, sum(bl.METh) AS METh0 FROM bl GROUP BY bl.ID, bl.HHoldGOR_B02ID')

TripTotalTime0 <- sqldf ('SELECT bl.ID, bl.HHoldGOR_B02ID, sum(bl.TripTotalTime) AS TripTotalTime0 FROM bl GROUP BY bl.ID, bl.HHoldGOR_B02ID')

# Select only interested columns for the aggregate files
# in a new local variable - so that baseline object 'bl' remains unchanged
local_bl <- subset(bl, select = c(ID, HHoldGOR_B02ID, TripDisIncSW, MainMode_B04ID, Cycled, MMETh, physical_activity_mmets, health_mmets, TripTotalTime1))
rm(bl)
local_bl <- arrange(local_bl, ID, HHoldGOR_B02ID)

# Add additional columns for baseline
local_bl$now_cycle <- 0
local_bl$ebike <- 0

# To include to baseline columns
# METh
# tbl$TripTotalTime1

AggScenarios6(local_bl, "baseline")

for (i1 in 1:length(listOfScenarios)) {
  
  print(listOfScenarios[i1])

  #DATA AGGREGATES
  tbl <- local_bl
  # sc <- get(as.character(listOfScenarios[i1]) )
  sc <- readRDS(paste0('./temp_data_folder/output/repo_version/', listOfScenarios[i1], '.rds'))
  
  # Sort the scenario file
  sc <- arrange(sc, ID, HHoldGOR_B02ID)
  
  # Temporary solution
  # Conider only first half of the scenario objects (as they're twice the size of baseline)
  # sc <- sc[1:(nrow(sc) / 2), ]
  
  tbl$now_cycle <- sc$now_cycle
  tbl$ebike <- sc$ebike
  #tbl$cyclist <- sc$cyclist
  tbl$METh <- sc$METh
  tbl$MMETh <- sc$MMETh
  tbl$TripTotalTime1 <- sc$TripTotalTime1
  rm(sc)
  AggScenarios6(tbl, as.character(listOfScenarios[i1]))
  rm(tbl)
  
  message('Scenario: ',as.character(listOfScenarios[i1]), " index ", i1)
  
}  #END main loop

#add baseline (1st colum of results)
local_listOfScenarios <- c('baseline', listOfScenarios)

scenarioStartingIndex <- ncol(bl.indiv) + 1

colnames(carMiles)[scenarioStartingIndex:length(carMiles)] <- local_listOfScenarios
colnames(MMETh)[scenarioStartingIndex:length(MMETh)] <- local_listOfScenarios
colnames(CO2.Tm)[scenarioStartingIndex:length(CO2.Tm)] <- local_listOfScenarios
colnames(PA_mmets)[scenarioStartingIndex:length(PA_mmets)] <- local_listOfScenarios
colnames(health_mmets)[scenarioStartingIndex:length(health_mmets)] <- local_listOfScenarios
colnames(milesCycled.pers)[scenarioStartingIndex:length(milesCycled.pers)] <- local_listOfScenarios
colnames(TripTotalTime1)[scenarioStartingIndex:length(TripTotalTime1)] <- local_listOfScenarios

# Remove newly created local baseline file
#rm(local_bl)


# colnames(carMiles)[scenarioStartingIndex:length(carMiles)] <- local_listOfScenarios
# colnames(carMilesR)[scenarioStartingIndex:length(carMilesR)] <- local_listOfScenarios
# colnames(carMilesCycledAggr)[scenarioStartingIndex:length(carMilesCycledAggr)] <- local_listOfScenarios
# colnames(milesCycled.pers)[scenarioStartingIndex:length(milesCycled.pers)] <- local_listOfScenarios
# colnames(METh)[scenarioStartingIndex:length(METh)] <- local_listOfScenarios
# colnames(METhincr)[scenarioStartingIndex:length(METhincr)] <- local_listOfScenarios
# colnames(TripDisIncSW)[scenarioStartingIndex:length(TripDisIncSW)] <- local_listOfScenarios
# colnames(TripTotalTime1)[scenarioStartingIndex:length(TripTotalTime1)] <- local_listOfScenarios
# colnames(timeSaved.Total.h)[scenarioStartingIndex:length(timeSaved.Total.h)] <- local_listOfScenarios
# colnames(health_mmets)[scenarioStartingIndex:length(health_mmets)] <- local_listOfScenarios

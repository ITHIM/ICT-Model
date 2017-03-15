#rm(list=ls())
source('AggCalc.R')   
source("healthcalculations/functions.R") # for as.numeric.factor

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

# Check if listOfScenarios exists. If not, then read it from a csv file
if (!exists("listOfScenarios") || length(listOfScenarios) != 24){
  listOfScenarios <- read.csv("listofScenarios.csv", header = F, as.is = T)
  listOfScenarios <- as.list(listOfScenarios$V1)
}

#read baseline (short walks already included)
baseline <- readRDS('bl2014_APS_p.rds')
assign(as.character(listOfScenarios[1]), readRDS(paste0('./temp_data_folder/output/repo_version/', as.character(listOfScenarios[1]), '.rds')))
if (nrow(baseline) != nrow(get(as.character(listOfScenarios[1])))){
  
  bl1 <- baseline
  bl1$HHoldGOR_B02ID <- 0
  
  # Rbind with itself setting region to 0, so that it becomes equal to the size of scenarios
  baseline <- rbind(baseline, bl1)
  
  #same for people w/o trips
  indiv.notrips1= indiv.notrips
  indiv.notrips1$HHoldGOR_B02ID <- 0
  
  indiv.notrips <- rbind(indiv.notrips, indiv.notrips1)
  
  # Remove temporary variables
  rm (bl1, indiv.notrips1)
  
  #create regions list (to process regional aggregates)
  regions <- sort(unique(baseline$HHoldGOR_B02ID))
  
} else{
  
  #create regions list (to process regional aggregates)
  regions <- sort(unique(baseline$HHoldGOR_B02ID))
  regions <- c(0,regions)[-c(10,11)]    #regions <- c('all',regions)[-c(10,11)]
}


regions <- sort(unique(baseline$HHoldGOR_B02ID))


#define parameters of interest (33 + 1)
params <-         c('region', 'nopeople', 'no.males', 'no.females', 'no.white', 'no.nonwhite', 
                    'no.caraccess', 'no.noncaraccess', 
                    'no.nssec1', 'no.nssec2','no.nssec3', 'no.nssec4', 'no.nssec5',
                    'notripspeople', 'no.males.wotrips', 'no.females.wotrips', 'no.white.wotrips','no.nonwhite.wotrips',
                    'no.caraccess.wotrips', 'no.noncaraccess.wotrips', 
                    'no.nssec1.wotrips', 'no.nssec2.wotrips', 'no.nssec3.wotrips', 'no.nssec4.wotrips', 'no.nssec5.wotrips',
                    'ncyclists0', 'cyclists0.perc',
                    'carMiles0', 'carMiles0.pers',
                    'METh0', 'MMETh0', 'MMETh0.pers',
                    'CO20',
                    'TripTotalTime0'  )

#matrix to hold aggregates
aggr <- matrix(data=0, nrow = length(regions),ncol = length(params))

#matrix names
colnames(aggr) <- params
row.names(aggr) <- regions

baseline <- rename(baseline, ID = IndividualID)
indiv.notrips <- rename(indiv.notrips, ID = IndividualID)

blreg <-  baseline       # ALREADY includes no trips data
indiv.notrips1reg <-  indiv.notrips # this variable used only for no-trips people calculations


# calculate 33 key figures (nat. & by region)
for (i in regions) {
  
  blreg <-  baseline
  indiv.notripsreg <-  indiv.notrips
  
  #filter data sources for region                      
  blreg <- subset(baseline, subset = baseline$HHoldGOR_B02ID==i)     #if (i!='all')
  indiv.notripsreg <- subset(indiv.notrips, subset = indiv.notripsreg$HHoldGOR_B02ID==i)
    
 
  ichar <- as.character(i)
  
  aggr[ichar, 'region'] <-  i
  # nopeople
  aggr[ichar, 'nopeople'] <-  length(unique(blreg$ID)) 
  
  #males/females
  aggr[ichar,'no.males']  <- length(unique(blreg$ID[blreg$Sex_B01ID==1]))
  aggr[ichar, 'no.females']  <-length(unique(blreg$ID[blreg$Sex_B01ID==2]))     
  
  #ethnicity
  aggr[ichar, 'no.white'] <- length(unique(blreg$ID[blreg$EthGroupTS_B02ID==1]))
  aggr[ichar, 'no.nonwhite'] <- length(unique(blreg$ID[blreg$EthGroupTS_B02ID==2]))
  
  #car access
  aggr[ichar, 'no.caraccess'] <- length(unique(blreg$ID[blreg$CarAccess_B01ID %in% c(1,2,3,4)]))  #check
  aggr[ichar, 'no.noncaraccess'] <- length(unique(blreg$ID[blreg$CarAccess_B01ID %in% c(5,6)]))  #check
  
  #ns-sec
  aggr[ichar, 'no.nssec1'] <- length(unique(blreg$ID[blreg$NSSec_B03ID==1]))
  aggr[ichar, 'no.nssec2'] <- length(unique(blreg$ID[blreg$NSSec_B03ID==2]))
  aggr[ichar, 'no.nssec3'] <- length(unique(blreg$ID[blreg$NSSec_B03ID==3]))
  aggr[ichar, 'no.nssec4'] <- length(unique(blreg$ID[blreg$NSSec_B03ID==4]))
  aggr[ichar, 'no.nssec5'] <- length(unique(blreg$ID[blreg$NSSec_B03ID==5]))
  
  #Individuals with no trips
  aggr[ichar, 'notripspeople'] <-length(unique(indiv.notripsreg$ID))
  aggr[ichar, 'no.males.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$Sex_B01ID==1]))
  aggr[ichar, 'no.females.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$Sex_B01ID==2]))
  
  #ethnicity & trips
  aggr[ichar, 'no.white.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$EthGroupTS_B02ID==1]))
  aggr[ichar, 'no.nonwhite.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$EthGroupTS_B02ID==2]))
  
  #car access & trips
  aggr[ichar, 'no.caraccess.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$CarAccess_B01ID %in% c(1,2,3,4)]))
  aggr[ichar, 'no.noncaraccess.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$CarAccess_B01ID %in% c(5,6)]))
  
  #NS-Sec & trips
  aggr[ichar, 'no.nssec1.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$NSSec_B03ID==1]))
  aggr[ichar, 'no.nssec2.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$NSSec_B03ID==2]))
  aggr[ichar, 'no.nssec3.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$NSSec_B03ID==3]))
  aggr[ichar, 'no.nssec4.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$NSSec_B03ID==4]))
  aggr[ichar, 'no.nssec5.wotrips'] <- length(unique(indiv.notripsreg$ID[indiv.notripsreg$NSSec_B03ID==5]))
  
  #cyclists in baseline
  aggr[ichar, 'ncyclists0'] <-length(unique(blreg$ID[blreg$Cycled==1]))  
  aggr[ichar, 'cyclists0.perc'] <-round(100* aggr[ichar,'ncyclists0'] /length(unique(blreg$ID)), 1)
  
  # car Miles blreg nos.
  aggr[ichar, 'carMiles0'] <-sum(blreg[blreg$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
  aggr[ichar, 'carMiles0.pers'] <-round(aggr[ichar,'carMiles0'] /aggr[ichar, 'nopeople'],1)
  
  # METs blreg nos.
  aggr[ichar, 'METh0'] <-sum(blreg$METh)
  aggr[ichar, 'MMETh0' ] <-sum(blreg$MMETh)
  aggr[ichar, 'MMETh0.pers'] <-round(aggr[ichar, 'MMETh0' ]/ aggr[ichar, 'nopeople'],1)
  
  # Using new Christian's average CO2 value of 0.31 grams 
  aggr[ichar, 'CO20'] <- round(aggr[ichar, 'carMiles0'] * 1.61 * (3.1 / 1.61) * 1e-1,2)   #(in Kg)
  
  aggr[ichar, 'TripTotalTime0'] <- round(sum(blreg$TripTotalTime),1)
  
}   #regions loop for baseline key aggregates


#DF to hold results
df <- data.frame()
num=0
# Creata local variable
# Please don't amend listOfScenarios, as it's a global variable
local_listOfScenarios <- c('baseline', listOfScenarios) 

# Add additional columns for baseline
baseline$now_cycle <- 0
baseline$ebike <- 0

for (i1 in 1:length(local_listOfScenarios)) {  #reading files for aggregates
  
  for (j1 in regions)  {

        if (local_listOfScenarios[i1] == 'baseline'){
      sc <- get(as.character(local_listOfScenarios[i1]) )
    } else {
      sc <- readRDS(paste0('./temp_data_folder/output/repo_version/', as.character(local_listOfScenarios[i1]), '.rds'))
    }
    tbl <- baseline
    
    sc <- subset(sc, subset= sc$HHoldGOR_B02ID==j1)
    
    tbl <- subset(baseline, subset = baseline$HHoldGOR_B02ID==j1)
    
    tbl$now_cycle <- sc$now_cycle
    tbl$ebike <- sc$ebike
    tbl$cyclist <- sc$cyclist
    tbl$METh <- sc$METh
    tbl$MMETh <- sc$MMETh
    tbl$TripTotalTime1 <- sc$TripTotalTime1
    # keeping 6 relevant variables from scenario
    
    info <- AggCalc(tbl, as.character(local_listOfScenarios[i1]), j1)
    if (num==0) {df<- info
    } else {df <- rbind(df,info)     }
    
    #df <- rbind(df,info)   #consolidates content
    num=num+1
    
  } #regions
  
  
  if ((i1%%1) == 0) {message('no scenarios finished: ',i1)  }
  
}  #scenarios

df <-as.data.frame(df)
rownames(df) <- NULL

colnames(df) <-c('Scenario','MS','ebike','equity',
                 "Car Miles Per person (per week)","Car Miles Reduced Per person (per week)","Total Car Miles Cycled (per week)",
                 "Total Miles Cycled (per week)", "Miles Cycled Per Male (per week)", "Miles Cycled Per Female (per week)", 
                 "Miles Cycled Per White Person (per week)", "Miles Cycled Per Non-White Person (per week)",
                 "Miles Cycled Per Person with Car-Access (per week)", "Miles Cycled Per Person with No Car-Access (per week)",
                 "Marginal METs Per Person (per week)",
                 "Transport CO2 Per Person (per week)",
                 "Total Time Saved in minutes by Cyclists (per week)",
                 "% Cyclists in the Total Population","% of Trips by Bicycle",
                 "Region" )

# Column  to character
df$Scenario <- as.character(df$Scenario)

#rest to numeric
for (i in 2:ncol(df)){
  df[,i] <- as.numeric.factor(df[,i])
}

saveRDS(df,file='ICT_aggr_reg.rds')

cat('All done !')

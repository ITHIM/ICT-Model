
#rm(list=ls())
source('AggCalc.R')   

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#read baseline (short walks already included)
baseline <- readRDS('bl2014_p.Rds')
#baseline <- read.csv('bl2012_18_84ag_sw_reduced.csv', header=T, as.is = T)

fnotrips  <- readRDS('people_notrips2014.Rds')
#fnotrips  <- read.csv('People_w_NoTrips2012_ENG_v6_anon.csv',header=T,as.is=T)

# Remove 85+ age group
fnotrips <- subset(fnotrips, Age_B01ID != 21)

fnotrips$agesex <- ""

fnotrips$Age <- ""

fnotrips$Sex <- ""

fnotrips[(fnotrips$Age_B01ID >= 6 & fnotrips$Age_B01ID <= 15) & fnotrips$Sex_B01ID == 1,]$agesex <- '16.59Male'

fnotrips[(fnotrips$Age_B01ID >= 6 & fnotrips$Age_B01ID <= 15) & fnotrips$Sex_B01ID == 2,]$agesex <- '16.59Female'

fnotrips[fnotrips$Age_B01ID >= 16 & fnotrips$Sex_B01ID == 1,]$agesex <- '60plusMale'

fnotrips[fnotrips$Age_B01ID >= 16 & fnotrips$Sex_B01ID == 2,]$agesex <- '60plusFemale'

fnotrips$Age <- 0

fnotrips[fnotrips$Sex_B01ID == 1,]$Sex <- "Male"

fnotrips[fnotrips$Sex_B01ID == 2,]$Sex <- "Female"


fnotrips[(fnotrips$Age_B01ID >= 6 & fnotrips$Age_B01ID <= 15),]$Age <- '16.59'
fnotrips[fnotrips$Age_B01ID >= 16,]$Age <- '60plus'


#create regions list (to process regional aggregates)
regions <- sort(unique(baseline$HHoldGOR_B02ID))
regions <- c('all',regions)


#define parameters of interest (33)
params <-         c('nopeople', 'no.males', 'no.females', 'no.white', 'no.nonwhite', 
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

blreg <-  baseline
fnotripsreg <-  fnotrips

for (i in regions) {
  
  #filter data sources for region                      
  if (i!='all')  { blreg <- subset(baseline, subset = baseline$HHoldGOR_B02ID==i)
  fnotripsreg <- subset(fnotrips, subset = fnotrips$HHoldGOR_B02ID==i)
  }  
  
  ichar <- as.character(i)
  
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
  aggr[ichar, 'notripspeople'] <-length(unique(fnotripsreg$ID))
  aggr[ichar, 'no.males.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$Sex_B01ID==1]))
  aggr[ichar, 'no.females.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$Sex_B01ID==2]))
  
  #ethnicity & trips
  aggr[ichar, 'no.white.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$EthGroupTS_B02ID==1]))
  aggr[ichar, 'no.nonwhite.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$EthGroupTS_B02ID==2]))
  
  #car access & trips
  aggr[ichar, 'no.caraccess.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$CarAccess_B01ID %in% c(1,2,3,4)]))
  aggr[ichar, 'no.noncaraccess.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$CarAccess_B01ID %in% c(5,6)]))
  
  #NS-Sec & trips
  aggr[ichar, 'no.nssec1.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$NSSec_B03ID==1]))
  aggr[ichar, 'no.nssec2.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$NSSec_B03ID==2]))
  aggr[ichar, 'no.nssec3.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$NSSec_B03ID==3]))
  aggr[ichar, 'no.nssec4.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$NSSec_B03ID==4]))
  aggr[ichar, 'no.nssec5.wotrips'] <- length(unique(fnotripsreg$ID[fnotripsreg$NSSec_B03ID==5]))
  
  #cyclists in baseline
  aggr[ichar, 'ncyclists0'] <-length(unique(blreg$ID[blreg$Cycled==1]))  
  aggr[ichar, 'cyclists0.perc'] <-round(100* aggr[ichar,'ncyclists0'] /(length(unique(blreg$ID))+ aggr[ichar, 'notripspeople']),1)
  
  # car Miles blreg nos.
  aggr[ichar, 'carMiles0'] <-sum(blreg[blreg$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
  aggr[ichar, 'carMiles0.pers'] <-round(aggr[ichar,'carMiles0'] /(aggr[ichar, 'nopeople'] + aggr[ichar, 'notripspeople'] ),1)
  
  # METs blreg nos.
  aggr[ichar, 'METh0'] <-sum(blreg$METh)
  aggr[ichar, 'MMETh0' ] <-sum(blreg$MMETh)
  aggr[ichar, 'MMETh0.pers'] <-round(aggr[ichar, 'MMETh0' ]/ aggr[ichar, 'nopeople'],1)
  
  # Using new Christian's average CO2 value of 0.31 grams 
  aggr[ichar, 'CO20'] <- round(aggr[ichar, 'carMiles0'] * 1.61 * (3.1 / 1.61) * 1e-4,2)   #(in metric Tons)
  
  aggr[ichar, 'TripTotalTime0'] <- round(sum(blreg$TripTotalTime),1)
  
}   #regions loop


#DF to hols results
df <- data.frame()
df <- AggCalc(baseline, "baseline", "all")
df = as.data.frame( t(df), stringsAsFactors = F)

for (i1 in 1:length(listOfScenarios)) {  #reading files for aggregates
  
  #DATA AGGREGATES
  
  for (j1 in regions)  {
    sc <- get(as.character(listOfScenarios[i1]) )
    tbl <- baseline
    
    if (j1!='all')  { tbl <- subset(baseline, subset = baseline$HHoldGOR_B02ID==j1)
    sc <- subset(sc, subset = sc$HHoldGOR_B02ID==j1) }  
    
    tbl$now_cycle <- sc$now_cycle
    tbl$ebike <- sc$ebike
    tbl$cyclist <- sc$cyclist
    tbl$METh <- sc$METh
    tbl$MMETh <- sc$MMETh
    tbl$TripTotalTime1 <- sc$TripTotalTime1
    # keeping 6 relevant variables from scenario
    
    info <- AggCalc(tbl, as.character(listOfScenarios[i1]), j1)
    
    df <- rbind(df,info)   #consolidates content
    
  }
  
  
  if ((i1%%1) == 0) {
    message('no scenarios finished: ',i1)
  }
  
}  #END main loop


# FileName	MS	ebike	equity	Car Miles per person	Car Miles Reduction Per Person	Miles Cycled Per Person	Miles Cycled Per White-Person	Miles Cycled Per Non-White-Person	Miles Cycled Per without car-access	Miles Cycled Per without non-car-access	Marginal METs Per Person	CO2 Total Per Person (kg)	% Cyclists in the Total Population	% Males who Cycle	% Females who Cycle	% White people who Cycle	% Non-White people who Cycle	% People Car Access who Cycle	% People without Car Access who Cycle	Years of Life Lost (YLL)	YLL Reduction (%)	


rownames(df) <- NULL

colnames(df) <-c('FileName','MS','ebike','equity',
                 "Car Miles Per person (per week)","Car Miles Reduced Per person (per/week)","Total Car Miles Cycled (per week)",
                 "Total Miles Cycled (per week)", "Miles Cycled Per Male (per week)", "Miles Cycled Per Female (per week)", 
                 "Miles Cycled Per White Person (per week)", "Miles Cycled Per Non-White Person (per week)",
                 "Miles Cycled Per Person with Car-Access (per week)", "Miles Cycled Per Person with No Car-Access (per week)",
                 "Marginal METs Per Person (per week)",
                 "Transport CO2 Per Person (per week)",
                 "Total Time Saved in minutes by Cyclists (per week)",
                 "% Cyclists in the Total Population","% of Trips by Bicycle",
                 "Region"
)


write.csv(df,file='ICT_aggr_reg.csv', row.names=F)

cat('All done !')
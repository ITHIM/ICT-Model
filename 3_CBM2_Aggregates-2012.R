#setwd("//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/23-CBM2/3-Code")
#rm(list=ls())
source('AggCalc.R')   

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#setwd('C:/Temp/Test/Scenarios2012')
#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012_England") #source folder

#read baseline & add short walks
baseline <- read.csv('bl2012_18_84ag_sw_reduced.csv', header=T, as.is = T)

# #add shortwalks
# dfsw <-baseline[baseline$MainMode_B03ID==1,]
# shortwalks <-data.frame()
# 
# for (i in 1:6) {shortwalks <-rbind(shortwalks,dfsw) }
# baseline <- rbind(baseline,shortwalks)    #short walks added
# baseline <- baseline[order(baseline$ID),]     #re-ordering by individuals
# rm(shortwalks,dfsw)

fnotrips  <- read.csv('People_w_NoTrips2012_ENG_v6_anon.csv',header=T,as.is=T)


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

# todos<-list.files(pattern="[0-9].csv")
# todos <-c('baseline.csv',todos)
# nfiles <-length(todos)


#Individuals figures
nopeople <-length(unique(baseline$ID))
no.males  <- length(unique(baseline$ID[baseline$Sex_B01ID==1]))
no.females  <-length(unique(baseline$ID[baseline$Sex_B01ID==2]))     

no.white <- length(unique(baseline$ID[baseline$EthGroupTS_B02ID==1]))
no.nonwhite <- length(unique(baseline$ID[baseline$EthGroupTS_B02ID==2]))

no.caraccess <- length(unique(baseline$ID[baseline$CarAccess_B01ID %in% c(1,2,3,4)]))  #check
no.noncaraccess <- length(unique(baseline$ID[baseline$CarAccess_B01ID %in% c(5,6)]))  #check

no.nssec1 <- length(unique(baseline$ID[baseline$NSSec_B03ID==1]))
no.nssec2 <- length(unique(baseline$ID[baseline$NSSec_B03ID==2]))
no.nssec3 <- length(unique(baseline$ID[baseline$NSSec_B03ID==3]))
no.nssec4 <- length(unique(baseline$ID[baseline$NSSec_B03ID==4]))
no.nssec5 <- length(unique(baseline$ID[baseline$NSSec_B03ID==5]))

#Individuals with no trips
notripspeople <-length(unique(fnotrips$ID))
no.males.wotrips <- length(unique(fnotrips$ID[fnotrips$Sex_B01ID==1]))
no.females.wotrips <- length(unique(fnotrips$ID[fnotrips$Sex_B01ID==2]))

no.white.wotrips <- length(unique(fnotrips$ID[fnotrips$EthGroupTS_B02ID==1]))
no.nonwhite.wotrips <- length(unique(fnotrips$ID[fnotrips$EthGroupTS_B02ID==2]))

no.caraccess.wotrips <- length(unique(fnotrips$ID[fnotrips$CarAccess_B01ID %in% c(1,2,3,4)]))
no.noncaraccess.wotrips <- length(unique(fnotrips$ID[fnotrips$CarAccess_B01ID %in% c(5,6)]))

no.nssec1.wotrips <- length(unique(fnotrips$ID[fnotrips$NSSec_B03ID==1]))
no.nssec2.wotrips <- length(unique(fnotrips$ID[fnotrips$NSSec_B03ID==2]))
no.nssec3.wotrips <- length(unique(fnotrips$ID[fnotrips$NSSec_B03ID==3]))
no.nssec4.wotrips <- length(unique(fnotrips$ID[fnotrips$NSSec_B03ID==4]))
no.nssec5.wotrips <- length(unique(fnotrips$ID[fnotrips$NSSec_B03ID==5]))

#cyclists
ncyclists0 <-length(unique(baseline$ID[baseline$Cycled==1]))
cyclists0.perc <-round(100* ncyclists0 /(length(unique(baseline$ID))+notripspeople),1)

# baseline nos.
carMiles0 <-sum(baseline[baseline$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
carMiles0.pers <-round(carMiles0/(nopeople+notripspeople),1)

METh0 <-sum(baseline$METh)
MMETh0 <-sum(baseline$MMETh)
MMETh0.pers <-round(MMETh0/nopeople,1)

# Using new Christian's average CO2 value of 0.31 grams 
CO20 <- round(carMiles0 * 1.61 * (3.1 / 1.61) * 1e-4,2)   #(in metric Tons)

TripTotalTime0 <- round(sum(baseline$TripTotalTime),1)

#DF for results
df <- AggCalc(baseline, "baseline")

for (i1 in 1:length(listOfScenarios)) {  #reading files for aggregates

  #DATA AGGREGATES
  tbl <- baseline
  sc <- get(as.character(listOfScenarios[i1]) )
  tbl$now_cycle <- sc$now_cycle
  tbl$ebike <- sc$ebike
  tbl$cyclist <- sc$cyclist
  tbl$METh <- sc$METh
  tbl$MMETh <- sc$MMETh
  tbl$TripTotalTime1 <- sc$TripTotalTime1
#   "now_cycle"      "ebike"          "cyclist"        "METh"           "MMETh"         
#   "TripTotalTime1"
  
  info <- AggCalc(tbl, as.character(listOfScenarios[i1]))
  
  df <- rbind(df,info)   #consolidates content
  
  if ((i1%%1) == 0) {
    message('no files: ',i1)
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
          "% Cyclists in the Total Population","% of Trips by Bicycle"
)

# colnames(df) <-c('FileName','MS','ebike','equity',
#                  'carMiles','carMilesR','carMiles.pers','carMilesR.pers','carMilesCycled',
#                  'milesCycled.pers','milesCycled.male', 'milesCycled.female', 
#                  'milesCycled.white', 'milesCycled.nonwhite',
#                  'milesCycled.caraccess', 'milesCycled.noncaraccess',
#                  'METh','METhincr','MMETh','MMETh.pers','MMEThincr',
#                  'CO2.Tm','CO2R.perc','CO2.pers',
#                  'TripDisIncSW','TripTotalTime1','timeSaved.Total.h','timeSavedCyclists.perc',
#                  'nocarTrips.people.perc','nocar.males.perc','nocar.females.perc',
#                  'nocar.white.perc','nocar.nonwhite.perc',
#                  'nocar.caraccess','nocar.noncaraccess',
#                  'nopeople','nocyclists','newcyclists','cyclist.potential',
#                  'cyclists.perc','cyclists.incr','cyclist.male.perc','cyclist.female.perc',
#                  'cyclist.white.perc','cyclist.nonwhite.perc','cyclist.caraccess.perc','cyclist.noncaraccess.perc',
#                  'cyclist.nssec1.perc','cyclist.nssec2.perc','cyclist.nssec3.perc','cyclist.nssec4.perc','cyclist.nssec5.perc',
#                  'trips.bike.perc',
#                  'trips.nssec1.perc','trips.nssec2.perc','trips.nssec3.perc','trips.nssec4.perc','trips.nssec5.perc',
#                  'trips.age20.39.perc','trips.age40.59.perc','trips.age60plus.perc',
#                  'nopeopleWithTrips','People.with.NoTrips')

write.csv(df,file='ICT_aggr.csv', row.names=F)

cat('All done !')
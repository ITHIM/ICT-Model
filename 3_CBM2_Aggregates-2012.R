#setwd("//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/23-CBM2/3-Code")
rm(list=ls())
source('AggCalc.R')   

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#setwd('C:/Temp/Test/Scenarios2012')
#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012_England") #source folder

#read baseline & add short walks
baseline <-read.csv('baseline.csv',header=T,as.is=T)

# #add shortwalks
# dfsw <-baseline[baseline$MainMode_B03ID==1,]
# shortwalks <-data.frame()
# 
# for (i in 1:6) {shortwalks <-rbind(shortwalks,dfsw) }
# baseline <- rbind(baseline,shortwalks)    #short walks added
# baseline <- baseline[order(baseline$IndividualID),]     #re-ordering by individuals
# rm(shortwalks,dfsw)

fnotrips  <- read.csv('People_w_NoTrips2012_ENG_v5.csv',header=T,as.is=T)

#DF for results
df <- data.frame()

todos<-list.files(pattern="[0-9].csv")
todos <-c('baseline.csv',todos)
nfiles <-length(todos)


#Individuals figures
nopeople <-length(unique(baseline$IndividualID))
no.males  <- length(unique(baseline$IndividualID[baseline$Sex_B01ID==1]))
no.females  <-length(unique(baseline$IndividualID[baseline$Sex_B01ID==2]))     

no.white <- length(unique(baseline$IndividualID[baseline$EthGroupTS_B02ID==1]))
no.nonwhite <- length(unique(baseline$IndividualID[baseline$EthGroupTS_B02ID==2]))

no.caraccess <- length(unique(baseline$IndividualID[baseline$CarAccess_B01ID %in% c(1,2,3,4)]))  #check
no.noncaraccess <- length(unique(baseline$IndividualID[baseline$CarAccess_B01ID %in% c(5,6)]))  #check

no.nssec1 <- length(unique(baseline$IndividualID[baseline$NSSec_B03ID==1]))
no.nssec2 <- length(unique(baseline$IndividualID[baseline$NSSec_B03ID==2]))
no.nssec3 <- length(unique(baseline$IndividualID[baseline$NSSec_B03ID==3]))
no.nssec4 <- length(unique(baseline$IndividualID[baseline$NSSec_B03ID==4]))
no.nssec5 <- length(unique(baseline$IndividualID[baseline$NSSec_B03ID==5]))

#Individuals with no trips
notripspeople <-length(unique(fnotrips$IndividualID))
no.males.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$Sex_B01ID==1]))
no.females.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$Sex_B01ID==2]))

no.white.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$EthGroupTS_B02ID==1]))
no.nonwhite.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$EthGroupTS_B02ID==2]))

no.caraccess.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$CarAccess_B01ID %in% c(1,2,3,4)]))
no.noncaraccess.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$CarAccess_B01ID %in% c(5,6)]))

no.nssec1.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$NSSec_B03ID==1]))
no.nssec2.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$NSSec_B03ID==2]))
no.nssec3.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$NSSec_B03ID==3]))
no.nssec4.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$NSSec_B03ID==4]))
no.nssec5.wotrips <- length(unique(fnotrips$IndividualID[fnotrips$NSSec_B03ID==5]))

#cyclists
ncyclists0 <-length(unique(baseline$IndividualID[baseline$Cycled==1]))
cyclists0.perc <-round(100* ncyclists0 /(length(unique(baseline$IndividualID))+notripspeople),1)

# baseline nos.
carMiles0 <-sum(baseline[baseline$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
carMiles0.pers <-round(carMiles0/(nopeople+notripspeople),1)

METh0 <-sum(baseline$METh)
MMETh0 <-sum(baseline$MMETh)
MMETh0.pers <-round(MMETh0/nopeople,1)

# Using new Christian's average CO2 value of 0.31 grams 
CO20 <- round(carMiles0 * 1.61 * (3.1 / 1.61) * 1e-4,2)   #(in metric Tons)

TripTotalTime0 <- round(sum(baseline$TripTotalTime),1)

for (i1 in (1:nfiles)) {  #reading files for aggregates
  #for (i1 in (1:3)) {
  
  #DATA AGGREGATES
  AggCalc(todos[i1])
  
  ifelse(i1==1,df <-info,df<-rbind(df,info))   #consolidates content
  
  if ((i1%%1) == 0) {
    message('no files: ',i1)
  }
  
}  #END main loop

rownames(df) <- NULL
colnames(df) <-c('FileName','MS','ebike','equity',
                 'carMiles','carMilesR','carMiles.pers','carMilesR.pers','carMilesCycled',
                 'milesCycled.pers','milesCycled.male', 'milesCycled.female', 
                 'milesCycled.white', 'milesCycled.nonwhite',
                 'milesCycled.caraccess', 'milesCycled.noncaraccess',
                 'METh','METhincr','MMETh','MMETh.pers','MMEThincr',
                 'CO2.Tm','CO2R.perc','CO2.pers',
                 'TripDisIncSW','TripTotalTime1','timeSaved.Total.h','timeSavedCyclists.perc',
                 'nocarTrips.people.perc','nocar.males.perc','nocar.females.perc',
                 'nocar.white.perc','nocar.nonwhite.perc',
                 'nocar.caraccess','nocar.noncaraccess',
                 'nopeople','nocyclists','newcyclists','cyclist.potential',
                 'cyclists.perc','cyclists.incr','cyclist.male.perc','cyclist.female.perc',
                 'cyclist.white.perc','cyclist.nonwhite.perc','cyclist.caraccess.perc','cyclist.noncaraccess.perc',
                 'cyclist.nssec1.perc','cyclist.nssec2.perc','cyclist.nssec3.perc','cyclist.nssec4.perc','cyclist.nssec5.perc',
                 'trips.bike.perc',
                 'trips.nssec1.perc','trips.nssec2.perc','trips.nssec3.perc','trips.nssec4.perc','trips.nssec5.perc',
                 'trips.age20.39.perc','trips.age40.59.perc','trips.age60plus.perc',
                 'nopeopleWithTrips','People.with.NoTrips')


write.csv(df,file='CBM_aggr_II.csv', row.names=F)

cat('All done !')
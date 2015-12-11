#setwd("//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/23-CBM2/3-Code") #NOT NEEDED ANYMORE
rm(list=ls())
timeStart<-Sys.time()

source('1_flowgram2-2012.R')  #scenarios generator
source('pcyc21.R')            # cycling probabilities
source('tripspeed.R')         # speed by age/gender
source('oddsp.R')             # calculates odds > prob
source('podds.R')             # calculates prob > odds
source('bikechoice.r')        #calculates prob of using pushbike/ebike 
# depending on: [age-sex-trip distance]

library(dplyr)  
library(stringr)
library(data.table)
library(sqldf)

#read reference odds
pcycl_baseline <- read.csv('pcycl_baseline2.csv')  #cycling probs file
pcycl_baseline <- pcycl_baseline[,-1]              #quita columna 1

#reads scenarios constants
METcycling <- 6.44
METwalking <- 4.61
METebikes <- 4.50

# Read Cycling Probabilities into an R Object
probCycling <- read.csv("cycling-probability.csv", header = T, as.is = T)

# Convert all probabilities into odss
oddsCycling <- round(probCycling /( 1 - probCycling), 3)

# Convert data.frame into a list
oddsCycling <- unlist(oddsCycling, use.names = F)

#lookup tables for equity=0,1. MUST BE expressed in ODDS!  
Pcyc0.eq0 <- oddsCycling[1:4]
Pcyc0.eq1 <- rep(oddsCycling[5], 4)
#AS ILLUSTRATION: 
#lookup.eq1 <-data.frame(agesex=c('16.59Male','16.59Female','60plusMale','60plusFemale'),

#get source file as DF and filter for interest year 
# bl <- read.csv('CBM5-TripsBaseline.csv', header=T)  #NTS trips, for age>18 & England only households
# bl <- read.csv('bl.csv', header=T)
# Only read baseline for the year 2012
bl <- read.csv('bl2012.csv', header=T)

# bl <- read.csv('CBM5-TripsBaseline-AnonymousID.csv', header=T)  #NTS trips, for age>18 & England only households

# Don't need to subset for the year 2012
# whichlines <- which(bl$SurveyYear==2012)
# baseline <- bl[whichlines,]  #baseline 30K / 2012

baseline <- bl

#handle short walks, creating 6x of each
df <- baseline[baseline$MainMode_B03ID==1,]
shortwalks <- data.frame()

for (i in 1:6) {
  shortwalks <- rbind(shortwalks,df) 
}

baseline <- rbind(baseline,shortwalks)
baseline <- baseline[order(baseline$ID),]
rm(shortwalks,df)

#1 sample before running scenarios -
# hsematch <- read.csv('indivHSE-NTS_2012_v1_AnonymousID.csv',header=T)
# hsematch <- read.csv('indivHSE-NTS_2012_v1.csv',header=T)

#hsematch <- read.csv('hsematchonlymmets.csv',header=T)
# Replace hsematch by including a two different columns for mmets
# hsematch <- read.csv('hsematchOnly2mmets.csv', header=T)#, colClasses=c("integer", "numeric", "numeric"))

# Removed NAs from the data.frame
hsematch <- read.csv('hsematchOnly2mmetsremovedNAs.csv', header = T, as.is = T)


#hsematch <- hsematch[,c(8,9)]  #keep only first and last column > IndivID, mMETs
hse1 <- setDT(hsematch)[,if(.N<1) .SD else .SD[sample(.N,1,replace=F)],by=ID]

#set scenarios folder (for saving them)
#setwd('C:/Temp/Test')
#setwd('V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012_England')

#add times cols. to baseline (used for travel time after cycle switch takes place)
TripTotalTime1 <- 0
TripTravelTime1 <- 0
baseline <- cbind(baseline,TripTotalTime1,TripTravelTime1)
baseline$TripTotalTime1 <- baseline$TripTotalTime
baseline$TripTravelTime1 <-baseline$TripTravelTime

#add mmets column to baseline (& save for having total mmets)
baseline <-inner_join(baseline,hse1,by='ID')

#randcycle (used later to calculate if people are cyclists), add col. [prob]
randcycle <- runif(length(unique(baseline$ID)))
randcycle <- data.frame(ID=unique(baseline$ID),prob=randcycle)
baseline <- inner_join(baseline,randcycle,by='ID')

#keep blas a backup for future scenarios core values
bl <- baseline

#save FINAL baseline in scenarios folder
# write.csv(baseline,file='baseline.csv', row.names=F)


###################################  START CALCULATIONS on BASELINE #############################

#nos. for baseline
carMiles0 <- sum(baseline[baseline$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
carMiles0 <- round(carMiles0,1)
METh0 <- round(sum(baseline$METh),1)
MMETh0 <- round(sum(baseline$MMETh),1)
# Miles to Kilometres, Grams to metric tonnes, 0.0001
#CO20 <- round(carMiles0 * 1.61 * 1.50 * 1e-4,2)   #(in metric Tons)
# Using new Christian's average CO2 value of 0.31 grams 
CO20 <- round(carMiles0 * 1.61 * (3.1 / 1.61) * 1e-4,2)   #(in metric Tons)

df <- data.frame()

i <- c(1,2,4,8,16,32,64)

# Removing TDR
# TDR
# j <- c(1,0.9,0.8,0.7)

m <- c(0,1)   #ebikes 
n <- c(0,1)   #equity
num = 1

for (ebikes in m) {
  for (equity in n) {
    for  (MS in i) {
      cat(ebikes, equity, MS, "\n")
      assign(paste("MS",MS,"_ebik",ebikes,"_eq" ,equity,sep=""),flowgram(baseline, MS,ebikes,equity, pcycl_baseline))      
      num <- num + 1
    }  
  } 
}  #j-i-m-n loop


#grouping baseline
# blaggr <- group_by(baseline,ID)
# blaggr <- summarise(blaggr,sum(METh),sum(MMETh),sum(health_mmets), sum(physical_activity_mmets))
# blaggr <- cbind(blaggr,blaggr[,3]+blaggr[,4])
# colnames(blaggr) <-c('ID','METh','MMETh1','MMETh2','MMEThTotal', 'TotalHealthMMETS', 'TotalPhysicalActivityMMETS')
# blaggr <- sqldf('select t1.ID,t1.Age,t1.Sex,t1.NSSec_B03ID,
#               t2.METh,t2.MMETh1,t2.MMETh2,t2.MMEThTotal as TotalMMETH, t2.TotalHealthMMETS, t2.TotalPhysicalActivityMMETS from baseline  
#               as t1 inner join blaggr as t2 where t1.ID=t2.ID 
#               group by t1.ID')

#blaggr<-sqldf('select t1.Age,t1.Sex,t1.NSSec_B03ID,t1.ID,(t1.MMETh+t2.mMETs) as TotalMMETH from baseline  as t1 inner join hse1 as t2 where t1.ID=t2.ID group by t1.ID')

# write.csv(blaggr,file=paste(scenarioFolderNameAndPath, 'baseline_aggr.csv', sep = "\\"), row.names=F)

# MMEThbl <-sum(blaggr$TotalMMETH)   #for aggregate file totals
# blsumm<-c('baseline',METh0,MMETh0,MMEThbl,MMETh0+MMEThbl,carMiles0,0,0)
# df<-rbind(blsumm,df)
# rownames(df) <- NULL
# colnames(df) <-c('scenario','METh','MMETh1','MMETh2','TotalMMETh','carMiles','carMilesSaved','carMilesReplaced')
# write.csv(df,file='./Scenarios2012/aggr.csv')

cat('All done! \n')

timeEnd<-Sys.time()
cat(difftime(timeEnd, timeStart, units='mins'), "\n")

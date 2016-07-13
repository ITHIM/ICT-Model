
rm(list=ls())
timeStart<-Sys.time()

source('1_flowgram2-2012.R')  #scenarios generator
source('pcyc21.R')            # cycling probabilities
source('tripspeed.R')         # speed by age/gender
source('oddsp.R')             # calculates odds > prob
source('podds.R')             # calculates prob > odds
source('bikechoice.R')        #calculates prob of using pushbike/ebike 
# depending on: [age-sex-trip distance]
library(plyr)
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

#Read cycling speeds by age/sex
cyclingspeed <- read.csv('cyclingSpeeds.csv',header=T)

# Read Cycling Probabilities into an R Object
probCycling <- read.csv("cycling-probability.csv", header = T, as.is = T)

# Convert all probabilities into odss
oddsCycling <- round(probCycling /( 1 - probCycling), 3)

# Convert data.frame into a list
oddsCycling <- unlist(oddsCycling, use.names = F)

#lookup tables for equity=0,1. MUST BE expressed in ODDS!  
Pcyc0.eq0 <- oddsCycling[1:4]
Pcyc0.eq1 <- rep(oddsCycling[5], 4)

# Only read baseline for the year 2012 and individuals between 18-84 year olds
bl <- readRDS('bl2014.Rds')
#bl <- read.csv('bl2012_18_84ag_reduced.csv', header=T, as.is = T)

baseline <- bl

#handle short walks, creating 6x of each
df <- baseline[baseline$MainMode_B03ID==1,]
shortwalks <- data.frame()

for (i in 1:6) {
  shortwalks <- rbind(shortwalks,df) 
}

# Update trip IDs with new IDs
shortwalks$TripID <-  c(max(baseline$TripID) + 1:nrow(shortwalks))

baseline <- rbind(baseline,shortwalks)
baseline <- baseline[order(baseline$ID),]

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

# Add tripID variable to it
fnotrips$TripID <- c(max(baseline$TripID) + 1:nrow(fnotrips))

rm(shortwalks,df)

#Sample before running scenarios -

# Removed NAs from the data.frame
#hsematch <- read.csv('hsematchOnly2mmetsremovedNAs.csv', header = T, as.is = T)
hsematch <- readRDS('hse-nts_match.Rds')
#names(hsematch)[c(1,7,8)] <- c('ID', 'health_mmets', 'physical_activity_mmets')
hsematch <- rbind(hsematch, subset(fnotrips, select = c(ID, health_mmets, physical_activity_mmets))) #this needed temporarily

#hsematch <- rbind(hsematch, subset(fnotrips, select = c(IndividualID,health_mmets, physical_activity_mmets)))


# Remove health_mmets and physical_activity_mmets from fnotrips
fnotrips$health_mmets <- NULL
fnotrips$physical_activity_mmets <- NULL

baseline <- rbind.fill(baseline, fnotrips)

baseline[is.na(baseline)] <- 0
baseline$TripID  <- as.numeric(factor(baseline$TripID))


#hsematch <- hsematch[,c(8,9)]  #keep only first and last column > IndivID, mMETs
hse1 <- setDT(hsematch)[,if(.N<1) .SD else .SD[sample(.N,1,replace=F)],by=ID]

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

baseline$prob[baseline$TravDay==0 ] <- 0


#keep bl as backup for future scenarios core values
bl <- baseline

#save FINAL baseline in scenarios folder
saveRDS(bl,file='bl2014_p.Rds')
#write.csv(bl,file='bl2012_18_84ag_sw_reduced.csv', row.names=F)


###################################  START CALCULATIONS on BASELINE #############################

#nos. for baseline
carMiles0 <- sum(baseline[baseline$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
carMiles0 <- round(carMiles0,1)
METh0 <- round(sum(baseline$METh),1)
MMETh0 <- round(sum(baseline$MMETh),1)
# Miles to Kilometres, Grams to metric tonnes, 0.0001
# CO20 <- round(carMiles0 * 1.61 * 1.50 * 1e-4,2)   #(in metric Tons)
# Using new Christian's average CO2 value of 0.31 grams
CO20 <- round(carMiles0 * 1.61 * (3.1 / 1.61) * 1e-4,2)   #(in metric Tons)

df <- data.frame()

i <- c(2,4,8,16,32,64)

# TODO: directProbs temp values - should be replaced with the final values

directProbs <- c(0.05, 0.1, 0.2, 0.5, 0.7, 1)

# work out proportion of cyclists by age-sex subgroups

totalNumberOfCyclistInPop <- length(unique(baseline[baseline$Cycled == 1,]$ID))
cyclistsPropBySubgroups <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'))

cyclistsPropBySubgroups$prop <-mapply(function(whichGroup) {
    length(unique(baseline[baseline$Cycled == 1 & baseline$agesex == whichGroup,]$ID))/totalNumberOfCyclistInPop
  }, cyclistsPropBySubgroups$agesex)

# Removing TDR
# TDR
# j <- c(1,0.9,0.8,0.7)

m <- c(0,1)   #ebikes 
n <- c(0,1)   #equity
num = 1

listOfScenarios <- list()
for (ebikes in m) {
  for (equity in n) {
    for  (MS in directProbs) { # all occurences of MS should be replaced
      cat(ebikes, equity, MS, "\n") 
      scenario_name <- paste("MS",MS,"_ebik",ebikes,"_eq" ,equity,sep="")
      assign(scenario_name,flowgram(baseline, MS,ebikes,equity, pcycl_baseline))      
      
      listOfScenarios[[num]] <- scenario_name
      num <- num + 1
    }  
  } 
}  #j-i-m-n loop

cat('All done! \n')

timeEnd<-Sys.time()
cat(difftime(timeEnd, timeStart, units='mins'), "\n")

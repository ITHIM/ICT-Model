rm(list=ls())
timeStart<-Sys.time()

# to avoid error on scenarios MS=1
.libPaths("C:/Program Files/R/R-3.3.1/library")

#to show things the right way
options("scipen" = 20)

# Functions
source('1_flowgram2-2012.R')  #scenarios generator
source('pcyc21.R')            # cycling probabilities
source('tripspeed.R')         # speed by age/gender
source('oddsp.R')             # calculates odds > prob
source('podds.R')             # calculates prob > odds
source('bikechoice.R')        #calculates prob of using pushbike/ebike 
source('directProbs.R')       # used in 1_flowgram2-2012.R


# Packages
library(dplyr)
library(plyr)  
library(stringr)
library(data.table)
library(sqldf)

#path to input files
datapath= './input'

# read reference odds (varies by age/sex/dist)
pcycl_baseline <- read.csv(file.path(datapath, 'pcycl_baseline2.csv'))  #cycling probs file
pcycl_baseline <- pcycl_baseline[,-1]              #quita columna 1

# read energy expenditure constants
METcycling <- 5.63
METwalking <- 3.53
METebikes <- 4.50

#Read cycling speeds by age/sex
cyclingspeed <- read.csv(file.path(datapath, 'cyclingSpeeds.csv'), header=T)

# Read Cycling Probabilities into an R Object
probCycling <- read.csv(file.path(datapath, "cycling-probability.csv"), header = T, as.is = T)

# Convert all probabilities into odss
oddsCycling <- round(probCycling /( 1 - probCycling), 3)

# Convert data.frame into a list
oddsCycling <- unlist(oddsCycling, use.names = F)

#lookup tables for equity=0,1. MUST BE expressed in ODDS!  
Pcyc0.eq0 <- oddsCycling[1:4]
Pcyc0.eq1 <- rep(oddsCycling[5], 4)

# Baseline= < NTS years 2004-2014 + individuals between 18-84 y.o + not Wales/Scotland >
bl <- readRDS(file.path(datapath, 'bl2014_APS.rds')  ) # built with NTS 2004-14 + APS
bl = subset(bl, subset = Age_B01ID < 21 & HHoldGOR_B02ID!=10  & HHoldGOR_B02ID!=11)
bl$Age[bl$Age_B01ID<16] <- '16.59'
bl$Age[bl$Age_B01ID>=16] <- '60plus'
bl$Sex[bl$Sex_B01ID==1] <- 'Male'
bl$Sex[bl$Sex_B01ID==2] <- 'Female'

#IMPORTANT: from database -> ID needs to be deleted, IndividualID renamed to ID.
baseline <- bl

# ATTENTION: shortwalks x7 already processed in baseline input

# Update trip IDs with new IDs
# baseline <- baseline[order(baseline$ID),]

#add people w/o trips to baseline
indiv.notrips  <- readRDS(file.path(datapath, 'indiv.notrips.Rds') )

# same as for baseline
indiv.notrips <- dplyr::rename(indiv.notrips, ID = IndividualID)


# Remove 85+ age group + Wales/Scotland (same as in baseline)
indiv.notrips <- subset(indiv.notrips, subset = Age_B01ID < 21 & HHoldGOR_B02ID<10)

#create new variables for scenario generation
indiv.notrips$agesex <- indiv.notrips$Age <- indiv.notrips$Sex <- ""

sel= (indiv.notrips$Age_B01ID >= 6 & indiv.notrips$Age_B01ID <= 15) & indiv.notrips$Sex_B01ID == 1
indiv.notrips$agesex[sel] <- '16.59Male'

sel = (indiv.notrips$Age_B01ID >= 6 & indiv.notrips$Age_B01ID <= 15) & indiv.notrips$Sex_B01ID == 2
indiv.notrips$agesex[sel] <- '16.59Female'

sel = indiv.notrips$Age_B01ID >= 16 & indiv.notrips$Sex_B01ID == 1
indiv.notrips$agesex[sel] <- '60plusMale'

sel= indiv.notrips$Age_B01ID >= 16 & indiv.notrips$Sex_B01ID == 2
indiv.notrips$agesex[sel] <- '60plusFemale'

indiv.notrips$Age <- 0

indiv.notrips$Sex[indiv.notrips$Sex_B01ID == 1] <- "Male"
indiv.notrips$Sex[indiv.notrips$Sex_B01ID == 2] <- "Female"

sel= (indiv.notrips$Age_B01ID >= 6 & indiv.notrips$Age_B01ID <= 15)
indiv.notrips$Age[sel] <- '16.59'

sel= indiv.notrips$Age_B01ID >= 16
indiv.notrips$Age[sel] <- '60plus'

# Add tripID variable to it
indiv.notrips$TripID <- c(max(baseline$TripID) + 1:nrow(indiv.notrips))

#baseline now IS complete= INDIVIDUALS [WITH +WITHOUT] TRIPS
baseline <- rbind.fill(baseline, indiv.notrips)

delcols = c("bin.WalkTime.h", "CycleTime", "CycleTime.h", 
            "WalkTime", "WalkTime.h" )
baseline = baseline[ , !(names(baseline) %in% delcols)] #delete energy columns as they're 0

# Read nts age group lookup table
ag_lookup <- read.csv("nts-adjusted-age-groups.csv", header = T, as.is = T)

# Create a new variable 'age_group' for baseline, which converts numeric age categories into age ranges
baseline$age_group <- ag_lookup$age[match(baseline$Age_B01ID, ag_lookup$nts_group)]
rm(ag_lookup)

baseline[is.na(baseline)] <- 0
#baseline$TripID  <- as.numeric(factor(baseline$TripID))

###### MATCHING & ENERGY vars

#read NTS<> APS matching file
nts.aps <- readRDS(file.path(datapath, 'nts.aps.rds'))
nts.aps = as.data.frame(nts.aps)   #prevent using data.tables

# subset ENERGY columns from APS and add to baseline
selcols = c("IndividualID", "mets_sport_wk", "WalkTime",
            "CycleTime", "walkAPS", "cycleAPS")

baseline= inner_join(baseline, nts.aps[, selcols], by= "IndividualID")

#add times cols. to baseline (used for travel time after cycle switch takes place)
TripTotalTime1 <- 0
TripTravelTime1 <- 0
baseline <- cbind(baseline,TripTotalTime1,TripTravelTime1)
baseline$TripTotalTime1 <- baseline$TripTotalTime
baseline$TripTravelTime1 <-baseline$TripTravelTime

rm(nts.aps)

#calculate mmets in baseline (& save for having total mmets)
baseline$METh = (METwalking * baseline$SumWStageTime/60)+ (METcycling * baseline$SumCStageTime/60)
baseline$MMETh = (METwalking-1) * (baseline$SumWStageTime/60)+ (METcycling-1) * (baseline$SumCStageTime/60)

#total P.A. mmets = 1.NTS person-level mmets    + 2.APS sport + 3.APS recreational W & C
baseline$physical_activity_mmets =  (METwalking-1) * (baseline$WalkTime/60) + 
  (METcycling-1) * (baseline$CycleTime / 60)  +
  (METwalking-1) * (baseline$walkAPS)         +
  (METcycling-1) * (baseline$cycleAPS)        +
  baseline$mets_sport_wk

baseline$health_mmets = baseline$physical_activity_mmets

#randcycle (used later to calculate if people become cyclists), add col. [prob]
#randcycle <- runif(length(unique(baseline$ID))) 
#randcycle <- data.frame(ID=unique(baseline$ID),prob=randcycle)

randcycle <- runif(length(unique(baseline$IndividualID)))
randcycle <- data.frame(IndividualID=unique(baseline$IndividualID), prob=randcycle)

baseline <- inner_join(baseline,randcycle, by='IndividualID')
rm(randcycle)

baseline$prob[baseline$TravDay==0 ] <- 0

# Rename IndividualID to ID
baseline <- dplyr::rename(baseline, ID = IndividualID)


#save PROCESSED baseline in main folder
saveRDS(baseline,file='bl2014_APS_p.rds')
rm(bl)

###################################  START CALCULATIONS on BASELINE #############################

#nos. for baseline
carMiles0 <- sum(baseline[baseline$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
carMiles0 <- round(carMiles0, 1)
METh0 <- round(sum(baseline$METh), 1)
MMETh0 <- round(sum(baseline$MMETh), 1)
# Miles to Kilometres, Grams to metric tonnes, 0.0001
# Using new Christian's average CO2 value of 0.31 grams
CO20 <- round(carMiles0 * 1.61 * (3.1 / 1.61) * 1e-1,2)   #(in Kg)

# DF which main role is to store info of every case in which Observed > DP. Used in UI to filter out mentioned cases (used in 1_flowgram2-2012)

directProbCasesAboveGivenPerc <- data.frame(MS = numeric(0),
                                            ebikes = numeric(0),
                                            equity = numeric(0),
                                            region = numeric(0))

# directProbs values

directProbs <- c(0.05, 0.1, 0.25, 0.5, 0.75, 1)

# Removing TDR
# TDR
# j  <- c(1,0.9,0.8,0.7)

m <- c(0,1)   #ebikes 
n <- c(0,1)   #equity
num = 1

baseline <-baseline[ , c('Age_B01ID', 'Sex_B01ID', 'HHoldGOR_B02ID', 'CarAccess_B01ID',
                         'NSSec_B03ID', 'IndIncome2002_B02ID', 'EthGroupTS_B02ID', 'TripID', 'ID',
                         'MainMode_B03ID', 'MainMode_B04ID', 'MainMode_B11ID', 
                         'TripTotalTime', 'TripTravTime', 'TripDisIncSW',
                         'agesex', 'Cycled', 'METh', 'MMETh', 'cyclist',
                         'Age', 'Sex', 'age_group',
                         'TripTotalTime1', 'TripTravelTime1', 
                         'health_mmets', 'physical_activity_mmets', 'prob')]

listOfScenarios <- list()

for (ebikes in m) {
  for (equity in n) {
    for  (MS in directProbs) { # all occurences of MS should be replaced
      cat(MS, ebikes, equity, "\n") 
      scenario_name <- paste("MS",MS,"_ebik",ebikes,"_eq" ,equity,sep="")
      #assign(scenario_name,flowgram(baseline, MS,ebikes,equity, pcycl_baseline))
      tempSc <- flowgram(baseline, MS,ebikes,equity, pcycl_baseline)
      saveRDS(tempSc, paste0('./temp_data_folder/output/repo_version/', scenario_name, '.rds'))
      
      listOfScenarios[[num]] <- scenario_name
      num <- num + 1
    }  
  } 
}  #j-i-m-n loop

cat('All done! \n')

timeEnd<-Sys.time()

save.image('afterLauncher.RData')
cat(difftime(timeEnd, timeStart, units='mins'), "\n")

rm(cyclingspeed, pcycl_baseline, probCycling)

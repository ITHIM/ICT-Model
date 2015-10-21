#setwd("//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/23-CBM2/3-Code") #NOT NEEDED ANYMORE
rm(list=ls())

# Specify Scenario folder
# scenarioFolderName <- "Scenarios2012"

scenarioFolderNameAndPath <- choose.dir(getwd(), "Choose a suitable folder")

# if(!dir.exists(scenarioFolderNameAndPath)){
if (length(list.files(path = scenarioFolderNameAndPath)) == 0){
#   dir.create(file.path(getwd(), scenarioFolderName))
  
  
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
  pcycl_baseline <-read.csv('pcycl_baseline2.csv')  #cycling probs file
  pcycl_baseline <-pcycl_baseline[,-1]              #quita columna 1
  
  #reads scenarios constants
  METcycling = 6.44
  METwalking = 4.61
  METebikes =  4.50
  
  #lookup tables for equity=0,1. MUST BE expressed in ODDS!  
  Pcyc0.eq0=c(0.094,0.038,0.045,0.015)
  Pcyc0.eq1=c(0.050,0.050,0.050,0.050)
  #AS ILLUSTRATION: 
  #lookup.eq1 <-data.frame(agesex=c('16.59Male','16.59Female','60plusMale','60plusFemale'),
  
  #get source file as DF and filter for interest year 
  bl <- read.csv('CBM5-TripsBaseline.csv', header=T)  #NTS trips, for age>18 & England only households
  whichlines <-which(bl$SurveyYear==2012)
  baseline <-bl[whichlines,]  #baseline 30K / 2012
  
  #handle short walks, creating 6x of each
  df <-baseline[baseline$MainMode_B03ID==1,]
  shortwalks <-data.frame()
  for (i in 1:6) {shortwalks <-rbind(shortwalks,df) }
  baseline <- rbind(baseline,shortwalks)
  baseline <- baseline[order(baseline$IndividualID),]
  rm(shortwalks,df)
  
  #1 sample before running scenarios -
  hsematch <- read.csv('indivHSE-NTS_2012_v1.csv',header=T)
  hsematch <- hsematch[,c(1,8)]  #keep only first and last column > IndivID, mMETs
  hse1 <- setDT(hsematch)[,if(.N<1) .SD else .SD[sample(.N,1,replace=F)],by=IndividualID]
  
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
  baseline <-inner_join(baseline,hse1,by='IndividualID')
  
  #randcycle (used later to calculate if people are cyclists), add col. [prob]
  randcycle <-runif(length(unique(baseline$IndividualID)))
  randcycle <-data.frame(IndividualID=unique(baseline$IndividualID),prob=randcycle)
  baseline <-inner_join(baseline,randcycle,by='IndividualID')
  
  #keep blas a backup for future scenarios core values
  bl <- baseline
  
  #save FINAL baseline in scenarios folder
  write.csv(baseline,file='baseline.csv', row.names=F)
  
  
  ###################################  START CALCULATIONS on BASELINE #############################
  
  #nos. for baseline
  carMiles0 <-sum(baseline[baseline$MainMode_B04ID %in% c(3,4,5,12),'TripDisIncSW'])
  carMiles0 <-round(carMiles0,1)
  METh0<-round(sum(baseline$METh),1)
  MMETh0<-round(sum(baseline$MMETh),1)
  CO20 <- round(carMiles0 * 1.61 * 1.50 * 1e-4,2)   #(in metric Tons)
  
  df<-data.frame()
  
  #i <- c(1,4,16,64)    #short
  i <- c(1,2,4,8,16,32,64)
  # i <- c(1,64)
  
  #j <- c(1,0.97,0.94,0.91,0.88,0.85, 0.82,0.79, 0.76)  #TDR
  #j <- c(1,0.8)    #short
  j <- c(1,0.9,0.8,0.7)    #short
  # j <- c(1)    #short
  
  m <- c(0,1)   #ebikes 
  #m <-0
  n <-c(0,1)   #equity
  num=1
  
  for (ebikes in m)   {
    for (equity in n)     {
      for  (MS in i)      {
        for (TDR in j)      {
          
          flowgram(MS,TDR,ebikes,equity)      
          num <- num+1
        }  } } }  #j-i-m-n loop
  
  
  #grouping baseline
  blaggr <- group_by(baseline,IndividualID)
  blaggr <- summarise(blaggr,sum(METh),sum(MMETh),mean(mMETs))
  blaggr <- cbind(blaggr,blaggr[,3]+blaggr[,4])
  colnames(blaggr) <-c('IndividualID','METh','MMETh1','MMETh2','MMEThTotal')
  blaggr<-sqldf('select t1.IndividualID,t1.Age,t1.Sex,t1.NSSec_B03ID,
              t2.METh,t2.MMETh1,t2.MMETh2,t2.MMEThTotal as TotalMMETH from baseline  
              as t1 inner join blaggr as t2 where t1.IndividualID=t2.IndividualID 
              group by t1.IndividualID')
  
  #blaggr<-sqldf('select t1.Age,t1.Sex,t1.NSSec_B03ID,t1.IndividualID,(t1.MMETh+t2.mMETs) as TotalMMETH from baseline  as t1 inner join hse1 as t2 where t1.IndividualID=t2.IndividualID group by t1.IndividualID')
  
  write.csv(blaggr,file=paste(scenarioFolderNameAndPath, 'baseline_aggr.csv', sep = "\\"), row.names=F)
  
  # MMEThbl <-sum(blaggr$TotalMMETH)   #for aggregate file totals
  # blsumm<-c('baseline',METh0,MMETh0,MMEThbl,MMETh0+MMEThbl,carMiles0,0,0)
  # df<-rbind(blsumm,df)
  # rownames(df) <- NULL
  # colnames(df) <-c('scenario','METh','MMETh1','MMETh2','TotalMMETh','carMiles','carMilesSaved','carMilesReplaced')
  # write.csv(df,file='./Scenarios2012/aggr.csv')
  
  cat('All done !')
}else{
  stop("The Scenario directory is not empty. Please select a new directory")
}
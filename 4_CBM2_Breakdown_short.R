setwd("//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/23-CBM2/Code")
rm(list=ls())
source('Analyze_short.R')

library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios")
#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012")

#DF for results
todos<-list.files(pattern="[0-9].csv")
todos <-c('baseline.csv',todos)
nfiles <-length(todos)

baseline <-read.csv('baseline.csv',header=T,as.is=T)
lookup <- data.frame(MainMode_B04ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13),modefinal=c(1,2,3,4,3,7,5,5,5,6,6,7,7))
modefinal=c(1,2,3,4,5,6,7)
modefinal <-data.frame(modefinal)
# colnames(modefinal) <-'modefinal'

#for grouping time savings
intervals <-c(0.5,0.7,0.9,1,1.1,1.3,1.5,2)
bands <-seq(0:9)
bands <-as.data.frame(bands)


####data frames for mining

#TIME SAVINGS: GLOBAL & by different criteria
BD_timehist <-data.frame()

BD_timehist.male <-data.frame()
BD_timehist.female <-data.frame()

BD_timehist.caraccess <-data.frame()
BD_timehist.noncaraccess <-data.frame()

#MODE ORIGIN
BD_mode <-data.frame()

#MODE ORIGIN of CYCLED TRIPS: by faster/slower - sex - ethn - car access
BD_mode.fastertrips <-data.frame()
BD_mode.slowertrips <-data.frame()

BD_mode.male <-data.frame()
BD_mode.female <-data.frame()

BD_mode.white <-data.frame()
BD_mode.nonwhite <-data.frame()

BD_mode.caraccess <-data.frame()
BD_mode.noncaraccess <-data.frame()

#MODE SHARE
BD_share <- data.frame()
BD_share.caraccess <- data.frame()
BD_share.noncaraccess <- data.frame()


#for (i1 in 1:10) {
for (i1 in (1:nfiles)) {  #reading files for aggregates
     
     #DATA MINING
     Analyze_short(todos[i1],i1)
     
     if ((i1%%5)==0) {message('no files: ',i1)}
     
}  #END main loop


#mode origin of cycling, global
write.csv(BD_mode,file='BD_mode.csv', row.names=F)

#time histograms
write.csv(BD_timehist,file='BD_timehist.csv', row.names=F)
write.csv(BD_timehist.male,file= 'BD_timehist-male.csv', row.names=F)
write.csv(BD_timehist.female,file= 'BD_timehist-female.csv', row.names=F)
#-----
write.csv(BD_timehist.caraccess,file= 'BD_timehist-caraccess.csv', row.names=F)
write.csv(BD_timehist.noncaraccess,file= 'BD_timehist-noncaraccess.csv', row.names=F)

#mode origin of cycling, breakdowns
write.csv(BD_mode.fastertrips,file='BD_mode-fastertrips.csv', row.names=F)
write.csv(BD_mode.slowertrips,file='BD_mode-slowertrips.csv', row.names=F)

write.csv(BD_mode.male,file='BD_mode-male.csv', row.names=F)
write.csv(BD_mode.female,file='BD_mode-female.csv', row.names=F)

write.csv(BD_mode.white,file='BD_mode-white.csv', row.names=F)
write.csv(BD_mode.nonwhite,file='BD_mode-nonwhite.csv', row.names=F)

write.csv(BD_mode.caraccess,file='BD_mode.caraccess.csv', row.names=F)
write.csv(BD_mode.noncaraccess,file='BD_mode.noncaraccess.csv', row.names=F)

#mode shares: global & by car access
write.csv(BD_share,file='BD_share.csv', row.names=F)

write.csv(BD_share.caraccess,file='BD_share-caraccess.csv', row.names=F)
write.csv(BD_share.noncaraccess,file='BD_share-noncaraccess.csv', row.names=F)

cat('All done !')
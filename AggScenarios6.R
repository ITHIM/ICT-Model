
AggScenarios6 <- function(fname)  {
  
#calculates 14 agregates for each scenario (by IndividualID)
################MISSING:        TDR for ALL CALCULATIONS !!!!!!!!!
  
#get the basics
  if (fname=='baseline.csv')  {
    MS<-1
    TDR <- 1
    equity<-0
    ebike <-0    }                  #baseline special case
  
  else       {
    params <-str_extract_all(fname, "([0-9]+(?:\\.[0-9]+)?)")
    params <-as.numeric(params[[1]] )
    MS <- as.integer(params[1])+1
    TDR <-as.numeric(gsub("^.*?(?:\\d+(?:\\.\\d+)?).*?(\\d+(?:\\.\\d+)?).*$","\\1",fname,perl=TRUE))
    ebike <- as.integer(params[3])
    equity <- as.integer(params[4])
              }   #rest of files        
  
  
f <-read.csv(file=fname) 
#check NAs
# f.indiv <-as.data.frame(unique(f$IndividualID))


# 1- carMiles 
f.carMiles  <- sqldf ('SELECT IndividualID,sum(TripDisIncSW) FROM f WHERE ([MainMode_B04ID] IN (3,4,5,12) 
AND ([now_cycle]=0 AND [Cycled]=0) ) GROUP BY IndividualID')

f.carMiles[,2] <- TDR * f.carMiles[,2]
f.carMiles <- left_join(bl.indiv,f.carMiles,by='IndividualID')
colnames(f.carMiles)[2] <- fname #rename column

carMiles <<- cbind(carMiles,f.carMiles[,2])

# 2-carMilesR           
f.carMilesR  <- cbind(f.carMiles[,1],(carMiles0[,2])-f.carMiles[,2])    #TDR already counted
colnames(f.carMilesR)[2] <- fname   #can rename the last!
carMilesR <<- cbind(carMilesR,f.carMilesR[,2])

# 3-carMilesCycled      
f.carMilesCycled <- sqldf ('SELECT IndividualID,sum(TripDisIncSW) AS carMilesCycled FROM f WHERE (f.[MainMode_B04ID] IN (3,4,5,12) 
AND (f.[now_cycle]==1 AND f.[Cycled]==0) ) GROUP BY IndividualID')

#f.carMilesCycled[,2] <- TDR * f.carMilesCycled[,2]  GIVES ERROR
f.carMilesCycled <- left_join(bl.indiv,f.carMilesCycled, by='IndividualID')
colnames(f.carMilesCycled)[2] <- fname

carMilesCycled <<- cbind(carMilesCycled,f.carMilesCycled[,2])

# 4-milesCycled.pers    
f.milesCycled <- sqldf ('SELECT IndividualID,sum(TripDisIncSW) AS milesCycled FROM f WHERE f.[now_cycle]=1 OR f.[Cycled]=1  
                         GROUP BY IndividualID')

f.milesCycled [,2] <- TDR * f.milesCycled[,2]
f.milesCycled  <- left_join(bl.indiv,f.milesCycled,by='IndividualID')

milesCycled.pers <<- cbind(milesCycled.pers,f.milesCycled[,2])


# 5-METh   
f.METh <- sqldf('select IndividualID,sum(METh) FROM f GROUP BY IndividualID')
METh <<- cbind(METh,f.METh[,2])


# 6-METhincr        
f.METhincr  <- cbind(f.METh[,1], f.METh[,2] - METh0[,2])
colnames(f.METhincr)[2] <- fname
METhincr <<- cbind(METhincr,f.METhincr[,2])


# 7-MMETh            
f.MMETh <- sqldf('select IndividualID,sum(MMETh) FROM f GROUP BY IndividualID')
MMETh <<- cbind(MMETh, f.MMETh[,2])


# 8-CO2.Tm           
CO2.Tm <<-cbind(CO2.Tm, 1.61 * 1.50 * 1e-1 * f.carMiles[,2])    #this is KG !! No TDR as it's been 
#already applied


# 9-TripDisIncSW     
f.TripDisIncSW <- sqldf('select IndividualID, sum(TripDisIncSW) FROM f GROUP BY IndividualID')

f.TripDisIncSW[,2] <- TDR * f.TripDisIncSW[,2]
colnames(f.TripDisIncSW)[2] <- fname

TripDisIncSW  <<- cbind(TripDisIncSW,TDR * f.TripDisIncSW)

# 10-TripTotalTime1   
f.TripTotalTime1 <- sqldf('select IndividualID, sum(TripTotalTime1) FROM f GROUP BY IndividualID')
colnames(f.TripTotalTime1)[2] <- fname

TripTotalTime1 <<- cbind(TripTotalTime1, f.TripTotalTime1[,2])

# 11- timeSaved.Total.h
f.timeSaved   <- as.data.frame(f.TripTotalTime1[,2] - TripTotalTime0[,2])
colnames(f.timeSaved) <- fname

timeSaved.Total.h <<- cbind(timeSaved.Total.h, f.timeSaved)

#write.csv (timeSaved.Total.h,file='timeSaved.Total.h.csv')

# 12- newcyclists 
# 1 if they have become cyclists.


# 13- cyclist.potential 
# ???????????????




# 14-trips.bike.perc

# f.trips.no <-sqldf ('SELECT IndividualID, Count(X) AS noTrips  FROM f group BY IndividualID')
# f.trips.bike <-sqldf ('SELECT IndividualID, Count(X) AS noTripsBike  FROM f WHERE (f.now_cycle=1 OR f.Cycled=1) group BY IndividualID ')
# f.trips.bike <- left_join(bl.indiv,f.trips.bike,by='IndividualID')
# 
# f.trips.bike.perc <-cbind(f.trips.bike[,1], (f.trips.bike[,2]/f.trips.no[,2]))
# 
# # f.trips.bike.perc <-sqldf ('SELECT IndividualID, Count(X) AS noTripsBike,
# #                           (SELECT Count(X) FROM f AS t1 WHERE MainMode_B04ID=2 AND t1.IndividualID=bl.individualID) AS noTripsBike
# #                            FROM f group BY IndividualID')
#   
# #SQL DIRECT ALTERNATIVE
# f.trips.bike.perc <- sqldf('SELECT f.IndividualID, Count(f.TripID) AS noTrips,(SELECT Count(TripID) from f as T1 where T1.[MainMode_B04ID]=2 and T1.individualID=f.individualID) AS tripsbyBike
#                             FROM f GROUP BY f.IndividualID')
                           
  
  
  
  
}
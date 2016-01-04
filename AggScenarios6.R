
AggScenarios6 <- function (f, fname) {
  
  #calculates 14 agregates for each scenario (by ID)
  ################MISSING:        TDR for ALL CALCULATIONS !!!!!!!!!
  #get file parameters
  if (fname == 'baseline') {
    MS <- 0
    equity <- 0
    ebike <- 0
  }     #baseline special case
  
  else {
    params <- str_extract_all(fname, "([0-9]+(?:\\.[0-9]+)?)")
    params <- as.numeric(params[[1]] )
    MS <- as.integer(params[1])
    ebike <- as.integer(params[2]) 
    equity <- as.integer(params[3])
  }   #rest of files 
  
  #f <- read.csv(file=fname)    
  #check NAs
  # f.indiv <-as.data.frame(unique(f$ID))
  
  
  # 1- carMiles 
  f.carMiles  <- sqldf ('SELECT f.ID,sum(f.TripDisIncSW) FROM f WHERE (f.[MainMode_B04ID] IN (3,4,5,12) 
AND (f.[now_cycle]=0 AND f.[Cycled]=0) ) GROUP BY f.ID')
  
  f.carMiles[,2] <- f.carMiles[,2]
  f.carMiles <- left_join(bl.indiv,f.carMiles,by='ID')
  colnames(f.carMiles)[2] <- fname #rename column
  
  carMiles <<- cbind(carMiles,f.carMiles[,2])
  
  # 2-carMilesR           
  f.carMilesR  <- cbind(f.carMiles[,1],(carMiles0[,2])-f.carMiles[,2])    #TDR already counted
  colnames(f.carMilesR)[2] <- fname   #can rename the last!
  carMilesR <<- cbind(carMilesR,f.carMilesR[,2])
  
  # 3-carMilesCycled      
  f.carMilesCycled <- sqldf ('SELECT f.ID,sum(f.TripDisIncSW) AS carMilesCycled1 FROM f WHERE (f.[MainMode_B04ID] IN (3,4,5,12) 
AND (f.[now_cycle]==1 AND f.[Cycled]==0) ) GROUP BY f.ID')
  
  #f.carMilesCycled[,2] <- f.carMilesCycled[,2]  GIVES ERROR
  f.carMilesCycled <- left_join(bl.indiv,f.carMilesCycled, by='ID')
  colnames(f.carMilesCycled)[2] <- fname
  
  carMilesCycledAggr <<- cbind(carMilesCycledAggr,f.carMilesCycled[,2])
  
  # 4-milesCycled.pers    
  f.milesCycled <- sqldf ('SELECT f.ID,sum(f.TripDisIncSW) AS milesCycled FROM f WHERE f.[now_cycle]=1 OR f.[Cycled]=1  
                         GROUP BY f.ID')
  
  f.milesCycled [,2] <- f.milesCycled[,2]
  f.milesCycled  <- left_join(bl.indiv,f.milesCycled,by='ID')
  
  milesCycled.pers <<- cbind(milesCycled.pers,f.milesCycled[,2])
  
  
  # 5-METh   
  f.METh <- sqldf('select f.ID,sum(f.METh) FROM f GROUP BY f.ID')
  METh <<- cbind(METh,f.METh[,2])
  
  
  # 6-METhincr        
  f.METhincr  <- cbind(f.METh[,1], f.METh[,2] - METh0[,2])
  colnames(f.METhincr)[2] <- fname
  METhincr <<- cbind(METhincr,f.METhincr[,2])
  
  # 7-MMETh            
  f.MMETh <- sqldf('select f.ID,sum(f.MMETh) as mmets FROM f GROUP BY f.ID')
  MMETh <<- cbind(MMETh, f.MMETh[,2])
  
  # 8-CO2.Tm           
  # Using new Christian's average CO2 value of 0.31 grams 
  CO2.Tm <<-cbind(CO2.Tm, 1.61 * (3.1 / 1.61) * 1e-1 * f.carMiles[,2])    #this is KG !! No TDR as it's been 
  #already applied
  
  # 9-TripDisIncSW     
  f.TripDisIncSW <- sqldf('select f.ID, sum(f.TripDisIncSW) FROM f GROUP BY f.ID')
  
  # f.TripDisIncSW[,2] <- f.TripDisIncSW[,2]
  colnames(f.TripDisIncSW)[2] <- fname
  
  TripDisIncSW  <<- cbind(TripDisIncSW,f.TripDisIncSW[,2])
  
  # 10-TripTotalTime1   
  f.TripTotalTime1 <- sqldf('select f.ID, sum(f.TripTotalTime1) FROM f GROUP BY f.ID')
  colnames(f.TripTotalTime1)[2] <- fname
  
  TripTotalTime1 <<- cbind(TripTotalTime1, f.TripTotalTime1[,2])
  
  # 11- timeSaved.Total.h
  f.timeSaved   <- as.data.frame(f.TripTotalTime1[,2] - TripTotalTime0[,2])
  colnames(f.timeSaved) <- fname
  
  timeSaved.Total.h <<- cbind(timeSaved.Total.h, f.timeSaved)
  
  # 12-Health_mmts   
  f.hmmets <- sqldf('Select f.ID, f.health_mmets from f Group by f.ID')
  f.hmmets$health_mmets <- f.hmmets$health_mmets + f.MMETh$mmets
  #colnames(f.hmmets$health_mmets) <- fname
  health_mmets <<- cbind(health_mmets,f.hmmets$health_mmets)
  
  # 13-PA_mmets
  f.PA_mmets <- sqldf('Select f.ID, f.physical_activity_mmets from f Group by f.ID')
  f.PA_mmets$physical_activity_mmets <- f.PA_mmets$physical_activity_mmets + f.MMETh$mmets
  #colnames(f.PA_mmets$physical_activity_mmets) <- fname
  PA_mmets <<- cbind(PA_mmets,f.PA_mmets$physical_activity_mmets)
  
  #write.csv (timeSaved.Total.h,file='timeSaved.Total.h.csv')
  
  # 12- newcyclists 
  # 1 if they have become cyclists.
  
  
  # 13- cyclist.potential 
  # ???????????????
  
  
  
  
  # 14-trips.bike.perc
  
  # f.trips.no <-sqldf ('SELECT ID, Count(X) AS noTrips  FROM f group BY ID')
  # f.trips.bike <-sqldf ('SELECT ID, Count(X) AS noTripsBike  FROM f WHERE (f.now_cycle=1 OR f.Cycled=1) group BY ID ')
  # f.trips.bike <- left_join(bl.indiv,f.trips.bike,by='ID')
  # 
  # f.trips.bike.perc <-cbind(f.trips.bike[,1], (f.trips.bike[,2]/f.trips.no[,2]))
  # 
  # # f.trips.bike.perc <-sqldf ('SELECT ID, Count(X) AS noTripsBike,
  # #                           (SELECT Count(X) FROM f AS t1 WHERE MainMode_B04ID=2 AND t1.ID=bl.individualID) AS noTripsBike
  # #                            FROM f group BY ID')
  #   
  # #SQL DIRECT ALTERNATIVE
  # f.trips.bike.perc <- sqldf('SELECT f.ID, Count(f.TripID) AS noTrips,(SELECT Count(TripID) from f as T1 where T1.[MainMode_B04ID]=2 and T1.individualID=f.individualID) AS tripsbyBike
  #                             FROM f GROUP BY f.ID')

}

AggScenarios6 <- function (f, fname) {
  
  #calculates 14 agregates for each scenario (by ID)
  ################MISSING:        TDR for ALL CALCULATIONS !!!!!!!!!
  #get file parameters
#   f <- bl
#   fnmae <- 'baseline'
  if (fname == 'baseline') {
    MS <- 0
    equity <- 0
    ebike <- 0
  }else {
    params <- str_extract_all(fname, "([0-9]+(?:\\.[0-9]+)?)")
    params <- as.numeric(params[[1]] )
    MS <- as.integer(params[1])
    ebike <- as.integer(params[2]) 
    equity <- as.integer(params[3])
  }   #rest of files 
  
  # 1- carMiles 
  f.carMiles  <- sqldf ('SELECT f.ID,f.HHoldGOR_B02ID, sum(f.TripDisIncSW) as var FROM f WHERE (f.MainMode_B04ID IN (3,4,5,12) 
                    AND (f.now_cycle=0 AND f.Cycled=0) ) GROUP BY f.ID, f.HHoldGOR_B02ID')
  
  f.carMiles[,2] <- f.carMiles[,2]
  f.carMiles <- left_join(bl.indiv, f.carMiles, by = c("ID" = "ID", "HHoldGOR_B02ID" = "HHoldGOR_B02ID"))
  colnames(f.carMiles)[ncol(f.carMiles)] <- fname #rename column
  
  carMiles <<- cbind(carMiles, f.carMiles[, ncol(f.carMiles)])
  
#   # 2-carMilesR           
#   f.carMilesR  <- cbind(f.carMiles[,1],(carMiles0[,3])-f.carMiles[,3])    #TDR already counted
#   colnames(f.carMilesR)[2] <- fname   #can rename the last!
#   carMilesR <<- cbind(carMilesR,f.carMilesR[,2])
#   
#   # 3-carMilesCycled      
#   f.carMilesCycled <- sqldf ('SELECT f.ID, f.HHoldGOR_B02ID, sum(f.TripDisIncSW) AS carMilesCycled1 FROM f WHERE (f.MainMode_B04ID IN (3,4,5,12) 
#                         AND (f.now_cycle = 1 AND f.Cycled = 0) ) GROUP BY f.ID, f.HHoldGOR_B02ID')
#   
#   #f.carMilesCycled[,2] <- f.carMilesCycled[,2]  GIVES ERROR
#   f.carMilesCycled <- left_join(bl.indiv,f.carMilesCycled, by = c("ID" = "ID", "HHoldGOR_B02ID" = "HHoldGOR_B02ID"))
#   colnames(f.carMilesCycled)[2] <- fname
#   
#   carMilesCycledAggr <<- cbind(carMilesCycledAggr,f.carMilesCycled[,2])
#   
#   # 4-milesCycled.pers    
#   f.milesCycled <- sqldf ('SELECT f.ID,f.HHoldGOR_B02ID, sum(f.TripDisIncSW) AS milesCycled FROM f WHERE f.[now_cycle]=1 OR f.[Cycled]=1  
#                          GROUP BY f.ID, f.HHoldGOR_B02ID')
#   
#   f.milesCycled [,2] <- f.milesCycled[,2]
#   f.milesCycled  <- left_join(bl.indiv,f.milesCycled,by = c("ID" = "ID", "HHoldGOR_B02ID" = "HHoldGOR_B02ID"))
#   
#   milesCycled.pers <<- cbind(milesCycled.pers,f.milesCycled[,2])
#   
#   
#   # 5-METh   
#   f.METh <- sqldf('select f.ID, f.HHoldGOR_B02ID, sum(f.METh) FROM f GROUP BY f.ID, f.HHoldGOR_B02ID')
#   METh <<- cbind(METh,f.METh[,2])
#   
#   
#   # 6-METhincr        
#   f.METhincr  <- cbind(f.METh[,1], f.METh[,3] - METh0[,3])
#   colnames(f.METhincr)[2] <- fname
#   METhincr <<- cbind(METhincr,f.METhincr[,2])
#   
#   # 7-MMETh            
  f.MMETh <- sqldf('select f.ID,f.HHoldGOR_B02ID, sum(f.MMETh) as mmets FROM f GROUP BY f.ID, f.HHoldGOR_B02ID')
  MMETh <<- cbind(MMETh, f.MMETh[,3])
#   
  # 8-CO2.Tm           
  # Using new Christian's average CO2 value of 0.31 grams 
  CO2.Tm <<- cbind(CO2.Tm, 1.61 * (3.1 / 1.61) * 1e-1 * f.carMiles[, ncol(f.carMiles)])    #this is KG !! No TDR as it's been 
#   #already applied
#   
#   # 9-TripDisIncSW     
#   f.TripDisIncSW <- sqldf('select f.ID, f.HHoldGOR_B02ID,  sum(f.TripDisIncSW) FROM f GROUP BY f.ID, f.HHoldGOR_B02ID')
#   
#   # f.TripDisIncSW[,2] <- f.TripDisIncSW[,2]
#   colnames(f.TripDisIncSW)[3] <- fname
#   
#   TripDisIncSW  <<- cbind(TripDisIncSW,f.TripDisIncSW[,3])
#   
#   # 10-TripTotalTime1   
#   f.TripTotalTime1 <- sqldf('select f.ID, f.HHoldGOR_B02ID, sum(f.TripTotalTime1) FROM f GROUP BY f.ID, f.HHoldGOR_B02ID')
#   colnames(f.TripTotalTime1)[3] <- fname
#   
#   TripTotalTime1 <<- cbind(TripTotalTime1, f.TripTotalTime1[,3])
#   
#   # 11- timeSaved.Total.h
#   f.timeSaved   <- as.data.frame(f.TripTotalTime1[,3] - TripTotalTime0[,3])
#   colnames(f.timeSaved) <- fname
#   
#   timeSaved.Total.h <<- cbind(timeSaved.Total.h, f.timeSaved)
#   
  # 12-Health_mmts   
  f.hmmets <- sqldf('Select f.ID, f.HHoldGOR_B02ID, f.health_mmets from f GROUP BY f.ID, f.HHoldGOR_B02ID')
  f.hmmets$health_mmets <- f.hmmets$health_mmets + f.MMETh$mmets
  #colnames(f.hmmets$health_mmets) <- fname
  health_mmets <<- cbind(health_mmets,f.hmmets$health_mmets)
  
  # 13-PA_mmets
  f.PA_mmets <- sqldf('Select f.ID, f.HHoldGOR_B02ID, f.physical_activity_mmets from f GROUP BY f.ID, f.HHoldGOR_B02ID')
  f.PA_mmets$physical_activity_mmets <- f.PA_mmets$physical_activity_mmets + f.MMETh$mmets
  #colnames(f.PA_mmets$physical_activity_mmets) <- fname
  PA_mmets <<- cbind(PA_mmets,f.PA_mmets$physical_activity_mmets)
  
  
}
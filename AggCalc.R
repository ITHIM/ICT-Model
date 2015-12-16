
AggCalc  <- function (f, fname) {    #calculates all agregates of a given scenario
  
  #get file parameters
  if (fname=='baseline') {
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
  
  
  #1-MILES
  carMiles <-sum(f[(f$MainMode_B04ID %in% c(3,4,5,12)) & (f$now_cycle==0 & f$Cycled==0),'TripDisIncSW'])
  carMiles <-round(carMiles,2)
  carMilesR <- round(carMiles0 - carMiles,2)
  carMiles.pers <- round(carMiles/(nopeople + notripspeople),1)
  carMilesR.pers <- round((carMiles0 - carMiles)/(nopeople + notripspeople),1)
  carMilesCycled <- sum(f[(f$MainMode_B04ID %in% c(3,4,5,12)) & (f$now_cycle==1 | f$Cycled==1),'TripDisIncSW']) #no need for Cycled=1!
  carMilesCycled <-round(carMilesCycled,1)
  
  milesCycled <- (sum(f[(f$now_cycle==1|f$Cycled==1),'TripDisIncSW']))/(nopeople + notripspeople)
  milesCycled <- round(milesCycled,2)
  
  milesCycled.male <- round((sum(f[(f$now_cycle==1|f$Cycled==1) & f$Sex=='Male','TripDisIncSW']))/(no.males + no.males.wotrips),2)
  milesCycled.female <- round((sum(f[(f$now_cycle==1|f$Cycled==1) & f$Sex=='Female','TripDisIncSW']))/(no.females + no.females.wotrips),2)
  
  milesCycled.white <- round((sum(f[(f$now_cycle==1|f$Cycled==1) & f$EthGroupTS_B02ID==1,'TripDisIncSW']))/(no.white + no.white.wotrips),2)
  milesCycled.nonwhite <- round( (sum(f[(f$now_cycle==1|f$Cycled==1) & f$EthGroupTS_B02ID==2,'TripDisIncSW']))/(no.nonwhite + no.nonwhite.wotrips),2)
  
  milesCycled.caraccess <- round((sum(f[(f$now_cycle==1|f$Cycled==1) & f$CarAccess_B01ID %in% c(1,2,3,4),'TripDisIncSW']))/(no.caraccess + no.caraccess.wotrips),2)
  milesCycled.noncaraccess <- round((sum(f[(f$now_cycle==1|f$Cycled==1) & f$CarAccess_B01ID %in% c(5,6),'TripDisIncSW']))/(no.noncaraccess + no.noncaraccess.wotrips),2)
  
  #2-co2
  CO2 <- round(carMiles * 1.61 * 1.50 * 1e-4,2)
  CO2R <- round(100 * (CO20-CO2)/CO20,1) 
  CO2.pers <- CO2/(nopeople + notripspeople)
  
  #3-METs
  METh<-round(sum(f$METh),1)
  METhincr <- round(METh - METh0,1)
  MMETh <-round(sum(f$MMETh),1)
  MMETh.pers <- round(MMETh/nopeople ,1)
  MMEThincr <- round(MMETh - MMETh0,1)
  
  #4-TIMES  &DISTANCES
  TripTotalTime1 <-sum(f$TripTotalTime1)       #time in scenario
  TripTravelTime <- sum(f$TripTravelTime)     #original time     
  timeSaved <- round((TripTotalTime0 - TripTotalTime1)/60,1)  #total h, all trips
  
  TripTotalTime1.cyclist <-sum(f$TripTotalTime1[f$now_cycle==1])  #new time
  TripTotalTime.cyclist  <-sum(f$TripTotalTime[f$now_cycle==1])   #old time
  timeSavedCyclists <- round(100 * TripTotalTime1.cyclist/TripTotalTime.cyclist,1)  
  #total in h, saved by cyclists
  
  TripDisIncSW <- round(sum(f$TripDisIncSW),0)
  
  #5-CYCLISTS & RATES
  nocyclists <-length(unique(f$ID[(f$now_cycle==1 | f$Cycled==1)])) #real cyclists     
  newcyclists <-length(unique(f$ID[f$now_cycle==1]))  
  
  cyclists.white <- length(unique(f$ID[ (f$now_cycle==1| f$Cycled==1) & f$EthGroupTS_B02ID==1]))  
  newcyclists.white <-length(unique(f$ID[f$now_cycle==1 & f$EthGroupTS_B02ID==1]))  
  
  cyclists.nonwhite <- length(unique(f$ID[ (f$now_cycle==1| f$Cycled==1) & f$EthGroupTS_B02ID==2]))  
  newcyclists.nonwhite <-length(unique(f$ID[f$now_cycle==1 & f$EthGroupTS_B02ID==2])) 
  
  cyclist.caraccess <- length(unique(f$ID [ (f$now_cycle==1 | f$Cycled==1) & (f$CarAccess_B01ID %in% c(1,2,3,4))]))
  cyclist.noncaraccess <- length(unique(f$ID [(f$now_cycle==1 | f$Cycled==1) & (f$CarAccess_B01ID %in% c(5,6)    )]))
  #      
  #      if (fname=='baseline.csv') {
  #           cyclist.potential <- length(unique(f$ID[f$Cycled==1])) #potential=real cyclists
  #           cyclist.male <-length(unique(f$ID[f$Cycled==1 & f$Sex_B01ID==1]))    
  #           cyclist.female <- length(unique(f$ID[f$Cycled==1 & f$Sex_B01ID==2]))
  #                               }
  #      else {
  cyclist.potential <- length(unique(f$ID[f$cyclist==1])) #potential cyclists
  cyclist.male <-length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$Sex_B01ID==1]))    
  cyclist.female <- length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$Sex_B01ID==2]))                       
  #          }
  
  
  cyclist.male.perc <- round(100*cyclist.male/(no.males+no.males.wotrips),1)     
  cyclist.female.perc <- round(100*cyclist.female/(no.females+no.females.wotrips),1)
  
  cyclist.perc <- round(100 * nocyclists/(nopeople + notripspeople) ,1)     
  cyclist.incr <- round(100 * (nocyclists-ncyclists0)/ncyclists0,1)
  
  cyclist.white.perc <- round(100 * cyclists.white/(no.white+no.white.wotrips),1)     
  cyclist.nonwhite.perc <- round(100 * cyclists.nonwhite/(no.nonwhite+no.nonwhite.wotrips),1)     
  
  cyclist.caraccess.perc <- round(100 * cyclist.caraccess/(no.caraccess+no.caraccess.wotrips),1)     
  cyclist.noncaraccess.perc <- round(100 * cyclist.noncaraccess/(no.noncaraccess+no.noncaraccess.wotrips),1)     
  
  cyclist.nssec1 <- length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==1]))
  cyclist.nssec2 <- length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==2]))
  cyclist.nssec3 <- length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==3]))
  cyclist.nssec4 <- length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==4]))
  cyclist.nssec5 <- length(unique(f$ID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==5]))
  
  cyclist.nssec1.perc <- round(100 * cyclist.nssec1 / (no.nssec1 + no.nssec1.wotrips),1)
  cyclist.nssec2.perc <- round(100 * cyclist.nssec2 / (no.nssec2 + no.nssec2.wotrips),1)
  cyclist.nssec3.perc <- round(100 * cyclist.nssec3 / (no.nssec3 + no.nssec3.wotrips),1)
  cyclist.nssec4.perc <- round(100 * cyclist.nssec4 / (no.nssec4 + no.nssec4.wotrips),1)
  cyclist.nssec5.perc <- round(100 * cyclist.nssec5 / (no.nssec5 + no.nssec5.wotrips),1)
  
  
  #6.0 -BIKE % SHARE WHOLE POPUL.
  no.trips <- length(unique(f$TripID))
  trips.bike <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1)]))
  trips.bike.perc <- round(100 * trips.bike / no.trips ,1)
  
  #6-TRIPS % SHARE NS-SEC
  no.trips.nssec1 <- length(unique(f$TripID[f$NSSec_B03ID==1]))
  no.trips.nssec2 <- length(unique(f$TripID[f$NSSec_B03ID==2]))
  no.trips.nssec3 <- length(unique(f$TripID[f$NSSec_B03ID==3]))
  no.trips.nssec4 <- length(unique(f$TripID[f$NSSec_B03ID==4]))
  no.trips.nssec5 <- length(unique(f$TripID[f$NSSec_B03ID==5]))
  
  trips.nssec1 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==1]))
  trips.nssec2 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==2]))
  trips.nssec3 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==3]))
  trips.nssec4 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==4]))
  trips.nssec5 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & f$NSSec_B03ID==5]))
  
  trips.nssec1.perc <- round( 100 * trips.nssec1 / no.trips.nssec1 ,1)
  trips.nssec2.perc <- round( 100 * trips.nssec2 / no.trips.nssec2 ,1)
  trips.nssec3.perc <- round( 100 * trips.nssec3 / no.trips.nssec3 ,1)
  trips.nssec4.perc <- round( 100 * trips.nssec4 / no.trips.nssec4 ,1)
  trips.nssec5.perc <- round( 100 * trips.nssec5 / no.trips.nssec5 ,1)
  
  # 7-TRIPS % SHARE BY AGE
  no.trips.age20.39 <- length(unique(f$TripID[f$Age_B01ID>=10 & f$Age_B01ID<=13]))
  no.trips.age40.59 <- length(unique(f$TripID[f$Age_B01ID>=14 & f$Age_B01ID<=15]))
  no.trips.age60plus <- length(unique(f$TripID[f$Age_B01ID>=16]))
  
  trips.age20.39 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) &  (f$Age_B01ID>=10 & f$Age_B01ID<=13) ]))
  trips.age40.59 <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & (f$Age_B01ID>=14 & f$Age_B01ID<=15)]))
  trips.age60plus <- length(unique(f$TripID[(f$now_cycle==1 |f$Cycled==1) & f$Age_B01ID>=16 ]))
  
  trips.age20.39.perc <- round( 100 * trips.age20.39 / no.trips.age20.39 ,1)
  trips.age40.59.perc <- round( 100 * trips.age40.59 / no.trips.age40.59 ,1)
  trips.age60plus.perc <- round( 100 * trips.age60plus / no.trips.age60plus,1)
  
  #8. PEOPLE WITH NO CAR TRIPS
  f$MainMode_B04ID[f$now_cycle==1] <- 2
  
  bltrips <- transform(f, car_trip = as.numeric(MainMode_B04ID == 3 
                                                | MainMode_B04ID == 4| MainMode_B04ID == 5 | MainMode_B04ID == 12))
  
  bltrips <- sqldf("Select ID,Sex,CarAccess_B01ID,EthGroupTS_B02ID,
                      sum(car_trip) as [car] from bltrips Group by ID")
  
  nocar.people <- nrow(subset(bltrips,car==0))
  nocar.people.perc <- round(100 * (nocar.people+notripspeople) / (nopeople + notripspeople) ,1)
  
  nocar.males <- nrow(subset(bltrips,car==0 & Sex=='Male'))
  nocar.females  <-nrow(subset(bltrips,car==0 & Sex=='Female'))
  
  nocar.males.perc  <- round(100* (nocar.males+no.males.wotrips) /(no.males+no.males.wotrips),1)
  nocar.females.perc <- round(100* (nocar.females+no.females.wotrips) /(no.females+no.females.wotrips),1)  
  
  nocar.white <- nrow(subset(bltrips,car==0 & EthGroupTS_B02ID==1))
  nocar.nonwhite  <- nrow(subset(bltrips,car==0 & EthGroupTS_B02ID==2))
  
  nocar.white.perc  <- round(100 * (nocar.white+no.white.wotrips)/(no.white + no.white.wotrips),1)
  nocar.nonwhite.perc <- round(100 *(nocar.nonwhite+no.nonwhite.wotrips) /(no.nonwhite + no.nonwhite.wotrips),1)  
  
  nocar.caraccess <- nrow(subset(bltrips,car==0 & CarAccess_B01ID %in% c(1,2,3,4)) )
  nocar.noncaraccess <- nrow(subset(bltrips,car==0 & CarAccess_B01ID %in% c(5,6)) )
  
  nocar.caraccess.perc <- round(100*(nocar.caraccess+no.caraccess.wotrips) / (no.caraccess+no.caraccess.wotrips),1)
  nocar.noncaraccess.perc <- round(100* (nocar.noncaraccess+ no.noncaraccess.wotrips) / (no.noncaraccess + no.noncaraccess.wotrips),1)
  
  
  # WRAPPING     
  info <- c(fname,MS,ebike,equity,
            carMiles,carMilesR,carMiles.pers,carMilesR.pers,carMilesCycled,
            milesCycled, milesCycled.male, milesCycled.female, 
            milesCycled.white, milesCycled.nonwhite,
            milesCycled.caraccess, milesCycled.noncaraccess,
            METh,METhincr,MMETh,MMETh.pers,MMEThincr,
            CO2,CO2R,CO2.pers,
            TripDisIncSW,TripTotalTime1,timeSaved,timeSavedCyclists,
            nocar.people.perc,nocar.males.perc,nocar.females.perc,
            nocar.white.perc,nocar.nonwhite.perc,
            nocar.caraccess.perc,nocar.noncaraccess.perc,
            nopeople,nocyclists,newcyclists,cyclist.potential,
            cyclist.perc,cyclist.incr,cyclist.male.perc,cyclist.female.perc,
            cyclist.white.perc,cyclist.nonwhite.perc, cyclist.caraccess.perc, cyclist.noncaraccess.perc,
            cyclist.nssec1.perc,cyclist.nssec2.perc,cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc,
            trips.bike.perc,
            trips.nssec1.perc,trips.nssec2.perc,trips.nssec3.perc,trips.nssec4.perc,trips.nssec5.perc,
            trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc,
            nopeople,notripspeople)
  
  info
}

flowgram <-function(baseline, MS1,ebikes1,equity1) {
  
  # temporary initiliazaiton of baseline
  baseline <- bl
  #resets all senarios parameters: trip cycled(now_cycle) | person=cyclist | prob cycling a trip (Pcyc)  
  baseline$now_cycle <- 0  
  baseline$cyclist <- 0  
  baseline$Pcyc <- 0
  baseline$METh    <- bl$METh     #recovers basic MMET
  baseline$MMETh   <- bl$MMETh
  baseline$ebike   <- 0
  ## add choice variable to all rows
  baseline$choice <- 0
  
  #resets travel times
  baseline$TripTotalTime1 <- baseline$TripTotalTime
  baseline$TripTravelTime1 <- 0  #not used
  
  justrandom <- runif(nrow(baseline),0,1)   #prob vector for now_cycle (trip probability)
  #NON-EQUITY scenario (mode shift * odds in both)
  #EQUITY scenario  
  # cat(ebikes, equity, MS, "\n")
  # 0 1 1
  MS1 = 1
  ebikes1 = 0
  equity1 = 1
  
  if (equity == 0) {
    Pcyc0 <- MS1 * Pcyc0.eq0
  }else {
    Pcyc0 <- MS1 * Pcyc0.eq1
  }  
  
  #calc new probs
  Pcyc0 <- sapply(Pcyc0,oddsp)
  lookup <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'),
                       Pcyc0)                    
  
  #calculate if people become cyclists
  baseline <- inner_join(baseline,lookup,by='agesex')
  baseline$cyclist <- 0
  baseline$cyclist[baseline$Pcyc0 > baseline$prob] <- 1
  # baseline$cyclist <- ifelse(baseline$Pcyc0 > baseline$prob,1,0)

  
#   baseline[baseline$Sex == 'Female' & baseline$Age == '16.59',]$newtime <- baseline[baseline$Sex == 'Female' & baseline$Age == '16.59',]$TripDisIncSW / 10.12
#   
#   baseline[baseline$Sex == 'Female' & baseline$Age != '16.59',]$newtime <- baseline[baseline$Sex == 'Female' & baseline$Age != '16.59',]$TripDisIncSW / 8.27
#   
#   baseline[baseline$Sex == 'Male' & baseline$Age == '16.59',]$newtime <- baseline[baseline$Sex == 'Male' & baseline$Age == '16.59',]$TripDisIncSW / 10.87
#   
#   baseline[baseline$Sex == 'Male' & baseline$Age != '16.59',]$newtime <- baseline[baseline$Sex == 'Male' & baseline$Age != '16.59',]$TripDisIncSW / 9.08
#   
  
  baseline$newtime <- apply(data.frame(baseline$Age, baseline$Sex), 1, function(x) tripspeed(x[1], x[2], 0))
  
#   if (baseline[k,'Cycled']==1)  {  #already cycling
#     baseline[k,'Pcyc'] <- p <- 1
#     baseline[k,'METh'] <- METcycling * newtime  #we use new time but may consider old (dep. on ebikes!!)
#     baseline[k,'MMETh'] <- (METcycling - 1) * newtime
#   }
   
  baseline[baseline$Cycled == 1,]$Pcyc <- 1
  
  baseline[baseline$Cycled == 1,]$METh <- METcycling * baseline[baseline$Cycled == 1,]$newtime
  
  baseline[baseline$Cycled == 1,]$MMETh <- (METcycling - 1) * baseline[baseline$Cycled == 1,]$newtime
  

  #   if (baseline[k,'cyclist']==0) {       #NOT CYCLISTS PEOPLE
  #     baseline[k,'TripTotalTime1'] = oldtime }
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 0 ,]$TripTotalTime1 <- 
    baseline[baseline$Cycled != 1 & baseline$cyclist == 0 ,]$TripTotalTime
  
  baseline[baseline$Cycled != 1 & baseline$cyclist != 0 ,]$Pcyc <- 
    apply(subset(baseline, Cycled != 1 & cyclist != 0, select = c(Age,Sex,TripDisIncSW)), 1, 
          function(x) pcyc21(x[1],x[2], x[3], ebikes1, equity1, MS1))
  
  ## add random column to the baseline data.frame
  baseline$justrandom <- justrandom
  
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]) > 0)
      baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$now_cycle <- 1
  
  # if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom),]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom),]$choice <- 
      apply(subset(baseline, Cycled != 1 & cyclist == 1 & (Pcyc > justrandom), 
                 select = c(TripDisIncSW)), 1, function(x) bikechoice(x[1]))
    
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$ebike <- 1
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime <- 
    apply(subset(baseline, Cycled != 1 & cyclist == 1 & (Pcyc > justrandom) & choice == 1, select = c(Age, Sex)), 1, function(x) tripspeed(x[1], x[2], 1))
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$METh <- 
    METebikes * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$MMETh <- 
    (METebikes - 1) * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$TripTotalTime1 <- 
    round(60 * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime, 0)
  
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$METh <- 
    METcycling * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$newtime
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$MMETh <- 
    (METcycling - 1) * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$newtime
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$TripTotalTime1 <- 
    round(60 * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$newtime, 0)
  
  # Cycled != 1 & cyclist == 1 & Pcyc <= justrandom
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$now_cycle <- 0
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$TripTotalTime1 <- 
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$TripTotalTime1
    

#   
  
#   baseline$newtime[baseline$Sex == 'Female' && age == '16.59', ] <- baseline$TripDisIncSW[baseline$Sex == 'Female' && age == '16.59', ] / 10.12
#   baseline$newtime <- baseline$TripDisIncSW / tripspeed(baseline$Age, baseline$Sex, 0) 
  
#   for (k in 1:nrow(baseline)) {
# 
#     ########Indiv. traits
#     age <- baseline[k,'Age']     
#     sex <- baseline[k,'Sex']          
#     
#     ######Trip traits
#     distance <- baseline[k,'TripDisIncSW']
#     # cat(k, " : ", distance, "\n")
#     speed <- tripspeed(age,sex,0)          #calculates for normal bike
#     oldtime <- baseline[k,'TripTotalTime']
#     newtime <- distance / speed       #for normal bike, initially
#     
#     ######   ALREADY CYCLED TRIPS, just get MMET  
#     
#     if (baseline[k,'Cycled'] == 1)  {  #already cycling
#       baseline[k,'Pcyc'] <- p <- 1
#       baseline[k,'METh'] <- METcycling * newtime  #we use new time but may consider old (dep. on ebikes!!)
#       baseline[k,'MMETh'] <- (METcycling - 1) * newtime
#     }  
#     
#     else {  #NOT YET CYCLED TRIPS
#       
#       if (baseline[k,'cyclist']==0) {       #NOT CYCLISTS PEOPLE
#         baseline[k,'TripTotalTime1'] = oldtime }     
#       
#       else if (baseline[k,'cyclist']==1) {   #CYCLISTS          *****
#         #CALC p
#         # cat(age," : ",sex," : ",distance," : ",ebikes1," : ",equity1," : ",MS1, "\n")
#         #cat(baseline[k,'Pcyc'], "\n")
#         p = pcyc(age,sex,distance,ebikes1,equity1,MS1)
#         
#         baseline[k,'Pcyc'] <- p     #prob. of new trip cycled                        
#         
#         ######## PCYC CALC & NOW_CYCLE decision              
#         if ( p > justrandom[k]) {   #now cycled
#           
#           baseline[k,'now_cycle'] <- 1                                
#           #baseline[k,'ebike']= choice
#           
#           ###### ANALYSIS OF CASES: normal bikes | ebikes | cycled or not (inc. walked)
#           
#           if (ebikes1==0) { #normal bikes
#             baseline[k,'METh']= METcycling * newtime
#             baseline[k,'MMETh']= (METcycling-1)* newtime
#             baseline[k,'TripTotalTime1']=round(60*newtime,0)
#           } 
#           
#           else            { #ebikes=1
#             choice <- bikechoice(distance)     
#             
#             if (choice == 1)   {     #user goes for ebike
#               #ebike speed calc.
#               baseline[k,'ebike']= 1
#               speed <- tripspeed(age,sex,1)
#               newtime = distance/ speed
#               
#               baseline[k,'METh']= METebikes * newtime
#               baseline[k,'MMETh']= (METebikes-1) * newtime 
#               baseline[k,'TripTotalTime1']= round(60*newtime,0)
#             } 
#             
#             else           {         #ebike user uses push-bike
#               #baseline[k,'ebike'] IS already 0
#               baseline[k,'METh']= METcycling * newtime
#               baseline[k,'MMETh']= (METcycling-1)* newtime
#               baseline[k,'TripTotalTime1']=round(60*newtime,0)
#             }      
#             
#           }       #ebikes=1 end! 
#           
#         } #now_cycle DECISION
#         
#         else { #NOT CYCLED (maybe walked or sth)
#           
#           baseline[k,'now_cycle']=0
#           baseline[k,'TripTotalTime1']= oldtime
#         }
#         
#       }   #END Cyclists         *****
#       if (k%%10000==0)    {cat(k,'lines-')}
#       
#     } #END NOT YET CYCLED TRIPS
#     
#   } #end main FOR         
  
  #actualizar people w. cycled trips > cyclists
  #baseline <-setDT(baseline)[,cyclist:=as.numeric(max(Cycled)),by=IndividualID]
  #baseline <-as.data.frame(baseline)
  
  # nombre <- paste("MS",MS1,"_ebik",ebikes1,"_eq" ,equity1,".csv",sep="")
  nombre <- paste("MS",MS1,"_ebik",ebikes1,"_eq" ,equity1,sep="")
  
  # Fixed a bug: replaced colnames with c
  blsave <- baseline[,c('ID','now_cycle','METh','MMETh','TripTotalTime1','TripTravelTime1','mMETs')]
  
  # assign(paste("MS",MS1,"_ebik",ebikes1,"_eq" ,equity1,sep=""),blsave)
  
  # write.csv(blsave,file=paste(scenarioFolderNameAndPath, nombre, sep = "\\"), row.names=F)
  
  cat("size: ", nrow(blsave), " - ", nombre,'\n',' done !!','\n') 
  blsave
  
}
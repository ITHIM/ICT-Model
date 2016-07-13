
flowgram <-function(baseline, MS,ebikes,equity, pcycl_baseline) {
#   
#   MS = 2
#   ebikes = 0
#   equity = 0

  
  # temporary initialization of baseline
  # baseline <- bl
  #resets all senarios parameters: trip cycled(now_cycle) | person=cyclist | prob cycling a trip (Pcyc)  
  baseline$now_cycle <- 0  
  baseline$cyclist <- 0  
  baseline$Pcyc <- 0
  baseline$ebike   <- 0
  
  ## add choice variable to all rows
  baseline$choice <- 0
  
  #resets travel times
  baseline$TripTotalTime1 <- baseline$TripTotalTime
  baseline$TripTravelTime1 <- 0  #not used
  
  justrandom <- runif(nrow(baseline),0,1)   #prob vector for now_cycle (trip probability)
  #NON-EQUITY scenario (mode shift * odds in both)
  #EQUITY scenario  
  
  if (equity == 0) {
    Pcyc0 <- (MS - 1) * Pcyc0.eq0
  }else {
    Pcyc0 <- (MS - 1) * Pcyc0.eq1
  }  
  
  #calc new probs
  Pcyc0 <- sapply(Pcyc0,oddsp)
  lookup <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'),
                       Pcyc0)                    
  lookup$agesex <- as.character(lookup$agesex)
  #calculate if people become cyclists
  baseline <- inner_join(baseline,lookup,by='agesex')
  baseline$cyclist <- 0
  #baseline$cyclist[baseline$Pcyc0 > baseline$prob] <- 1
  
  # calc how many cyclists should be drawn, don't include ppl who already cycle
  
  howManyCyclistNeeded <- round(MS * length(unique(baseline$ID)) - totalNumberOfCyclistInPop, digits = 0)
  
  # just in case check if number excess # of all ppl
  
  howManyCyclistNeeded <- ifelse(howManyCyclistNeeded > length(unique(baseline$ID)), length(unique(baseline$ID)), howManyCyclistNeeded)
  
  if (equity == 0) {
    
    #print(howManyCyclistNeeded)
    #print(cyclistsPropBySubgroups*howManyCyclistNeeded)
    
    remainingCyclistsBySubgroups <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'))
    
    remainingCyclistsBySubgroups$remaining <- mapply(function(whichGroup){
      
      # calc how many cyclists should be drawn, taking into account cyclists prop
      
      projectedCyclistsInSubgroup <- round(as.numeric(cyclistsPropBySubgroups[cyclistsPropBySubgroups$agesex == whichGroup, ]$prop) * howManyCyclistNeeded, digits = 0)
      
      #print(projectedCyclistsInSubgroup)
      
      # check if there are enough ppl from subgroup in population; if more are selected -> use total number of subgroup members
      
      realCyclistsInSubgroup <- ifelse(projectedCyclistsInSubgroup > length(unique(baseline[baseline$Cycled != 1 & baseline$agesex == whichGroup, ]$ID)), length(unique(baseline[baseline$Cycled != 1 & baseline$agesex == whichGroup, ]$ID)), projectedCyclistsInSubgroup)
      #print(realCyclistsInSubgroup)
      
      # pick up ppl, mark them as cyclists
      
      baseline$cyclist[baseline$ID == sample(unique(baseline[baseline$Cycled != 1 & baseline$agesex == whichGroup,]$ID), realCyclistsInSubgroup, replace = F)] <- 1
      
      # work out remaining diff (if value > 0 this means that sample should be filled with ppl from other subgroups)
      
      ifelse(projectedCyclistsInSubgroup - length(unique(baseline[baseline$Cycled != 1 & baseline$agesex == whichGroup, ]$ID)) <= 0, 0, projectedCyclistsInSubgroup - length(unique(baseline[baseline$Cycled != 1 & baseline$agesex == whichGroup, ]$ID)))
      
    }, remainingCyclistsBySubgroups$agesex)
    
    #print(remainingCyclistsBySubgroups) 
    
    # fill scenario with ppl from other subgroups if remaining ppl exist
    
    if (sum(remainingCyclistsBySubgroups$remaining) > 0){
      
      #print(sum(remainingCyclistsBySubgroups$remaining))
      
      baseline$cyclist[baseline$ID == sample(unique(baseline[baseline$Cycled != 1 & baseline$cyclist != 1,]$ID), sum(remainingCyclistsBySubgroups$remaining), replace = F)] <- 1
      
    }
    
  } else {
    
    # pick up randomly ppl who are not cyclist using same prop for all
    
    baseline$cyclist[baseline$ID == sample(unique(baseline[baseline$Cycled != 1,]$ID), howManyCyclistNeeded, replace = F)] <- 1
    
  }
  
  # cyclists in population should be marked as cyclists
  
  baseline[baseline$Cycled == 1,]$cyclist <- 1
  
  baseline$newtime <- baseline$TripDisIncSW / apply(data.frame(baseline$Age, baseline$Sex), 1, function(x) tripspeed(x[1], x[2], 0))
  
  baseline[baseline$Cycled == 1,]$Pcyc <- 1
  
  baseline[baseline$Cycled == 1,]$METh <- METcycling * baseline[baseline$Cycled == 1,]$newtime
  
  baseline[baseline$Cycled == 1,]$MMETh <- (METcycling - 1) * baseline[baseline$Cycled == 1,]$newtime
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 0 ,]$TripTotalTime1 <- 
    baseline[baseline$Cycled != 1 & baseline$cyclist == 0 ,]$TripTotalTime
  
  #calculate prob of a given trip being cycled
  baseline[baseline$Cycled != 1 & baseline$cyclist != 0 ,]$Pcyc <- 
    apply(subset(baseline, Cycled != 1 & cyclist != 0, select = c(Age,Sex,TripDisIncSW)), 1, 
          function(x) pcyc21(x[1],x[2], x[3], ebikes, equity, MS))
  
  ## add random column to the baseline data.frame
  baseline$justrandom <- justrandom
  cat(class(baseline$Cycled), " : ", class(baseline$cyclist), " : ", class(baseline$Pcyc), " : ", class(baseline$justrandom), "\n")
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$now_cycle <- 1
  
  if (ebikes == 0 && (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]) > 0)){
    
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$METh <- 
      METcycling * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$newtime
    
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$MMETh <- 
      (METcycling - 1) * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$newtime
    
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$TripTotalTime1 <- 
      round(60 * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$newtime, 0)
      
  }
#   cycled != 1 and cyclist == 1 and p > justrandom
#   then now_cycle = 1
#   and if ebikes == 0{
#     baseline[k,'METh']= METcycling * newtime
#     baseline[k,'MMETh']= (METcycling-1)* newtime
#     baseline[k,'TripTotalTime1']=round(60*newtime,0)
#     
#   }
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom),]$choice <- 
    apply(subset(baseline, Cycled != 1 & cyclist == 1 & (Pcyc > justrandom), 
                 select = c(TripDisIncSW)), 1, function(x) bikechoice(x[1], unlist(subset(pcycl_baseline, select = tripsebike), use.names = FALSE)))
  
  if (ebikes == 1){
    if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]) > 0)
      baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$ebike <- 1
    
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime <- 
      baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$TripDisIncSW / 
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
  }
  
  # Cycled != 1 & cyclist == 1 & Pcyc <= justrandom
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$now_cycle <- 0
  
  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$TripTotalTime1 <- 
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$TripTotalTime1
  
  # nombre <- paste("MS",MS,"_ebik",ebikes,"_eq" ,equity,".csv",sep="")
  nombre <- paste("MS",MS,"_ebik",ebikes,"_eq" ,equity,sep="")
  
  # Fixed a bug: replaced colnames with c
  # Removed TripTravelTime1
  blsave <- baseline[,c('ID','HHoldGOR_B02ID','now_cycle','ebike','cyclist','METh','MMETh','TripTotalTime1')]
  
  # write.csv(blsave,file=paste(scenarioFolderNameAndPath, nombre, sep = "\\"), row.names=F)
  cat("size: ", nrow(blsave), " - ", nombre,'\n',' done !!','\n') 
  
  # Return blsave
  blsave
  
}
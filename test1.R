############ fixing Time  journey error

#bl <- readRDS('bl2014_p.rds')
bl <- readRDS('bl2014_p_v2.rds')

# For some reason the age_group is not correctly recorded in the baseline line
# Recalculate the age_group variable
# Read nts age group lookup table
ag_lookup <- read.csv("nts-adjusted-age-groups.csv", header = T, as.is = T)
# Create a new variable 'age_group' for baseline, which converts numeric age categories into age ranges
bl$age_group <- ag_lookup$age[match(bl$Age_B01ID, ag_lookup$nts_group)]


bl1 <- bl
bl1$HHoldGOR_B02ID <- 0
bl <- rbind(bl, bl1)
rm (bl1)
#}

#transforms MainMode_B04ID >> to our own modes
lookup <- data.frame(MainMode_B04ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13),modefinal=c(1,2,3,4,3,7,5,5,5,6,6,7,7))
bl$MainMode_Reduced <- lookup$modefinal[match(bl$MainMode_B04ID, lookup$MainMode_B04ID)]

# Keep a copy of baseline in a local variable
local_bl <- bl

# Remove "80 - 84" age group for the interface
local_bl <- subset(local_bl, age_group != "80 - 84")


listOfScenarios=listOfScenarios[1:9]


library(dplyr)

# Create trips df
tripsdf <- create_trips(bl, listOfScenarios)

# Remove 80+ from the dataset
tripsdf <- subset(tripsdf, age_group != "80 - 84")

# Create trips time df
tripstimedf <- create_triptime(bl, listOfScenarios)

# Remove 80+ from the dataset
tripstimedf <- subset(tripstimedf, age_group != "80 - 84")

# Rename MainMode_Reduced columns to baseline
tripsdf$baseline <- tripsdf$MainMode_Reduced
tripsdf$MainMode_Reduced <- NULL
tripsdf$Cycled <- NULL

# # names(tripTime)[names(tripTime)=="MainMode_Reduced"] <- "baseline"
# 
# # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
# # Reduce the number of modes to 4
# # walk, bicycle, car, others
# lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
# 
# # Replace number of modes in each of the scenarios and the baseline to 4
# for (i in 7:ncol(tripMode)){
#   tripMode[,i] <- lookup$red_mode[match(tripMode[,i], lookup$mode)]
# }

# Get row numbers with NA

temp <- data.frame(rn = which( is.na(tripsdf$baseline), arr.ind=TRUE))

tripsdf$X <- c(1:nrow(tripsdf))

tripstimedf$X <- c(1:nrow(tripstimedf))

# Remove all rows with NA in them

tripsdf <- (subset(tripsdf, !(X %in% temp$rn) ))

tripstimedf <- (subset(tripstimedf, !(X %in% temp$rn) ))

rm(temp)

# moved from the ICT
# # # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
# # # Reduce the number of modes to 4
# # # walk, bicycle, car, others
# # lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
# # 
# # # Replace number of modes in each of the scenarios and the baseline to 4
# # for (i in 7:31){
# #   tripMode[,i] <- lookup$red_mode[match(tripMode[,i], lookup$mode)]
# # }
# 
# 
# end of moved part
names(tripsdf)[names(tripsdf)=="MainMode_Reduced"] <- "baseline"

# Precalculate trips used in "Journey Time" tab-

TripTotalTimeCalcs <- function(tripTime, tripMode){
  # all possible age_group + 'All'
  
  aaAgeGroups <- c(sort(unique(tripTime[, c("age_group")])), 'All')
  
  # all possible Sex + 3 (all)
  
  aaSexs <- c(sort(unique(tripTime[, c("Sex_B01ID")])), '3')
  
  # all possible Ethnicity + 'All'
  
  aaEthnicities <- c(sort(unique(tripTime[, c("EthGroupTS_B02ID")])), 'All')
  
  # all possible SES + 'All'
  
  aaSESs <- c(sort(unique(tripTime[, c("NSSec_B03ID")])), 'All')
  
  # all possible scenarios - not very elegant way
  
  aaScenarios <- colnames(tripTime)[9:(length(colnames(tripTime))-1)]
  
  # all possible regions
  
  aaRegions <- sort(unique(tripTime[, c("HHoldGOR_B02ID")]))
  #aud: aaRegions <- aaRegions[1:3] # ADDED FOR TESTING - 
  
  # for every region
  
  for (aaRegion in aaRegions){
    
    print(aaRegion)
    
    dir.create(paste0('../ICT/app/data/csv/TripTotalTime1_regional/baseline/', aaRegion, '/histogram'), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0('../ICT/app/data/csv/TripTotalTime1_regional/baseline/', aaRegion, '/other'), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0('../ICT/app/data/csv/TripTotalTime1_regional/filtered/', aaRegion, '/histogram'), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0('../ICT/app/data/csv/TripTotalTime1_regional/filtered/', aaRegion, '/other'), showWarnings = FALSE, recursive = TRUE)
    
    # select data for region
    
    tripTimeRegion <- subset(tripTime, HHoldGOR_B02ID == aaRegion)
    tripModeRegion <- subset(tripMode, HHoldGOR_B02ID == aaRegion)
    
    for (aaScenario in aaScenarios){
      
      print(aaScenario)
      
      # scenario baseline shouldn't be calculated every time, only once for every scenario to reduce number of operations
      
      tempScenarioHistogramFreq <- data.frame(stringsAsFactors = FALSE)
      tempScenarioOtherFreq <- data.frame(stringsAsFactors = FALSE)
      
      columnName <- aaScenario
      
      data <- tripTimeRegion
      
      # Get row numbers with NA
      temp <- data.frame(rn = tripTimeRegion[,c("X")])
      
      locatTripModeData <- tripModeRegion[,c("X","baseline", columnName)]
      
      # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
      # Reduce the number of modes to 4
      # walk, bicycle, car, others
      lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
      
      # Replace number of modes in each of the scenarios and the baseline to 4
      locatTripModeData[["baseline"]] <- lookup$red_mode[match(locatTripModeData[["baseline"]], lookup$mode)]
      
      # Replace number of modes in each of the scenarios and the baseline to 4
      locatTripModeData[[columnName]] <- lookup$red_mode[match(locatTripModeData[[columnName]], lookup$mode)]
      
      # Remove all rows with NA in them
      locatTripModeData <- (subset(locatTripModeData, (X %in% temp$rn) ))
      
      rm("temp")
      
      localtripData <- data[,c("X","TripTotalTime1", columnName)]
      
      rm("data")
      
      localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$TripTotalTime1) / localtripData$TripTotalTime1 ) * 100)
      
      locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
      
      names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
      localtripData <- inner_join(localtripData, locatTripModeData, by = "rn")
      
      localtripData <- subset(localtripData, localtripData$baseline != localtripData[[columnName]])
      
      scTripTimeTraveldata <- localtripData
      
      rm(list=c("localtripData", "locatTripModeData"))
      
      # calc freqs
      
      total_col <- "baseline"
      
      umode <- sort(unique(scTripTimeTraveldata[,total_col]))
      # Exclude bicycle mode
      umode <- umode[umode != 2]
      
      # histogram data
      
      for (i in 1:length(umode)){
        
        ldata <- subset(scTripTimeTraveldata, scTripTimeTraveldata[,total_col] == umode[i])
        
        bc <- as.data.frame(table (cut (ldata$diff, breaks = c(-100, -50, -20, 0, 20, 50, 100, Inf))), stringsAsFactors = FALSE)
        
        bc$Freq <- round(bc$Freq / nrow(scTripTimeTraveldata) * 100, digits = 1)
        
        # change column name - use scenario name
        
        colnames(bc) <- c('freq', aaScenario)
        
        # add column with umode
        
        bc[,c('umode')] <- umode[i]
        
        tempScenarioHistogramFreq <- bind_rows(tempScenarioHistogramFreq, bc)
        
      }
      
      # other data
      
      scTripTimeTraveldata <- subset(scTripTimeTraveldata, diff != 0)
      
      for (i in 1:length(umode)){
        ldata <- subset(scTripTimeTraveldata, scTripTimeTraveldata[,total_col] == umode[i])
        
        
        data <- data.frame(counts = c(nrow(subset(ldata, diff < 0)),
                                      #nrow(subset(ldata, diff == 0)),
                                      nrow(subset(ldata, diff > 0))), which_counts = c("diffless0", "diffgr0"))
        data$counts <- round(data$counts / nrow(scTripTimeTraveldata) * 100, 2)
        
        # change column name - use scenario name
        
        colnames(data) <- c(aaScenario, 'counts')
        
        # add column with umode
        
        data[,c('umode')] <- umode[i]
        
        tempScenarioOtherFreq <- bind_rows(tempScenarioOtherFreq, data)
        
      }
      
      # clean up mem
      
      rm("scTripTimeTraveldata")
      
      gc()
      
      # for filtered data
      
      tempScenarioFilteredHistogramFreq <- data.frame(stringsAsFactors = FALSE)
      tempScenarioFilteredOtherFreq <- data.frame(stringsAsFactors = FALSE)
      
      for (aaAgeGroup in aaAgeGroups){
        
        print(paste("age:", aaAgeGroup))
        
        for (aaSex in aaSexs){
          
          print(paste("sex:", aaSex))
          
          for (aaEthnicity in aaEthnicities){
            
            for (aaSES in aaSESs){
              
              data <- tripTimeRegion
              
              if (aaAgeGroup != 'All'){
                data <- subset(data, age_group == aaAgeGroup)
              }
              if (aaSex != 3)
                data <- subset(data, Sex_B01ID %in% as.integer(aaSex))
              
              if (aaSES != "All"){
                data <- subset(data, NSSec_B03ID %in% as.integer(aaSES))
              }
              
              if (aaEthnicity != "All"){
                data <- subset(data, EthGroupTS_B02ID %in% as.integer(aaEthnicity))
              }
              data[is.na(data)] <- 0
              
              locatTripModeData <- tripModeRegion[,c("X","baseline", columnName)]
              
              # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
              # Reduce the number of modes to 4
              # walk, bicycle, car, others
              lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
              
              # Replace number of modes in each of the scenarios and the baseline to 4
              locatTripModeData[["baseline"]] <- lookup$red_mode[match(locatTripModeData[["baseline"]], lookup$mode)]
              
              # Replace number of modes in each of the scenarios and the baseline to 4
              locatTripModeData[[columnName]] <- lookup$red_mode[match(locatTripModeData[[columnName]], lookup$mode)]
              
              # Get row numbers with NA
              #temp <- data.frame(rn = which(sessionData$tripTime[,c("X")] %in% data$X))
              
              # Get row numbers which fulfil selected conditions
              
              temp <- tripTimeRegion[,c("X")] %in% data$X
              
              selectedRows <- tripTimeRegion[temp, ]
              
              rm("temp")
              
              # Remove all rows with NA in them
              locatTripModeData <- (subset(locatTripModeData, (X %in% selectedRows$X) ))
              
              localtripData <- data[,c("X","TripTotalTime1", columnName)]
              
              rm("data")
              
              localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$TripTotalTime1) / localtripData$TripTotalTime1 ) * 100)
              
              #localtripData <- subset(localtripData, diff <= 200 & diff >= -200 )
              
              locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
              
              names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
              localtripData <- inner_join(localtripData, locatTripModeData, by = "rn")
              
              localtripData <- subset(localtripData, localtripData$baseline != localtripData[[columnName]])
              scFilteredTripTimeTraveldata <- localtripData
              
              rm(list=c("localtripData", "locatTripModeData"))
              
              # calc freqs
              
              total_col <- "baseline"
              
              umode <- sort(unique(scFilteredTripTimeTraveldata[,total_col]))
              # Exclude bicycle mode
              umode <- umode[umode != 2]
              
              # histogram data
              
              for (i in 1:length(umode)){
                ldata <- subset(scFilteredTripTimeTraveldata, scFilteredTripTimeTraveldata[,total_col] == umode[i])
                
                bc <- as.data.frame(table (cut (ldata$diff, breaks = c(-100, -50, -20, 0, 20, 50, 100, Inf))), stringsAsFactors = FALSE)
                
                bc$Freq <- round(bc$Freq  / nrow(scFilteredTripTimeTraveldata) * 100, digits = 1)
                
                # change column name - use scenario name
                
                colnames(bc) <- c('freq', aaScenario)
                
                # add column with umode
                
                bc[,c('umode')] <- umode[i]
                bc[,c('agegroup')] <- aaAgeGroup
                bc[,c('gender')] <- aaSex
                bc[,c('ethnicity')] <- aaEthnicity
                bc[,c('ses')] <- aaSES
                
                tempScenarioFilteredHistogramFreq <- bind_rows(tempScenarioFilteredHistogramFreq, bc)
                write.csv(tempScenarioFilteredHistogramFreq,paste('c:/Temp/tempScenarioFilteredHistogramFreq',umode[i],'.csv'))
              }
              
              # other data
              
              #Remove unchanged trips
              scFilteredTripTimeTraveldata <- subset(scFilteredTripTimeTraveldata, diff != 0)
              
              for (i in 1:length(umode)){
                ldata <- subset(scFilteredTripTimeTraveldata, scFilteredTripTimeTraveldata[,total_col] == umode[i])
                
                
                data <- data.frame(counts = c(nrow(subset(ldata, diff < 0)),
                                              #nrow(subset(ldata, diff == 0)),
                                              nrow(subset(ldata, diff > 0))),
                                   which_counts = c("diffless0", "diffgr0"))
                data$counts <- round(data$counts / nrow(scFilteredTripTimeTraveldata) * 100, 2)
                
                # change column name - use scenario name
                
                colnames(data) <- c(aaScenario, 'counts')
                
                # add column with umode
                
                data[,c('umode')] <- umode[i]
                data[,c('agegroup')] <- aaAgeGroup
                data[,c('gender')] <- aaSex
                data[,c('ethnicity')] <- aaEthnicity
                data[,c('ses')] <- aaSES
                
                tempScenarioFilteredOtherFreq <- bind_rows(tempScenarioFilteredOtherFreq, data)
                
              }
              
              rm("scFilteredTripTimeTraveldata", "bc", "data")
              
              gc()
              
            }
            
          }
          
        }
        
      }
      
      # for scenario baseline - add region column, add scenario values
      
      tempScenarioHistogramFreq[,c('region')] <- aaRegion
      
      saveRDS(tempScenarioHistogramFreq, paste0('../ICT/app/data/csv/TripTotalTime1_regional/baseline/', aaRegion, '/histogram/', aaScenario, '.rds'))
      
      tempScenarioOtherFreq[,c('region')] <- aaRegion
      
      saveRDS(tempScenarioOtherFreq, paste0('../ICT/app/data/csv/TripTotalTime1_regional/baseline/', aaRegion, '/other/', aaScenario, '.rds'))
      
      rm(list=c("tempScenarioHistogramFreq", "tempScenarioOtherFreq"))
      
      # for filtered scenario - add region column, add scenario values
      
      tempScenarioFilteredHistogramFreq[,c('region')] <- aaRegion
      
      saveRDS(tempScenarioFilteredHistogramFreq, paste0('../ICT/app/data/csv/TripTotalTime1_regional/filtered/', aaRegion, '/histogram/', aaScenario, '.rds'))
      
      tempScenarioFilteredOtherFreq[,c('region')] <- aaRegion
      
      saveRDS(tempScenarioFilteredOtherFreq, paste0('../ICT/app/data/csv/TripTotalTime1_regional/filtered/', aaRegion, '/other/', aaScenario, '.rds'))
      
      rm(list=c("tempScenarioFilteredHistogramFreq", "tempScenarioFilteredOtherFreq"))
      
      gc()
      
    }
    
  }
}


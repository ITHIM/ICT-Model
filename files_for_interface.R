# Script to generate rds file for the interface
# PLEASE NOTE: It assumes that all scenarios have been generated, and four additional scripts have been run, which are:
# 1. 0_topflow2-2012 (to generate directProbCasesAboveGivenPerc)
# 2. 3_CBM2_Aggregates-2012.R
# 3. 6_CBM2_ScenarioAggregates.R
# 4. /healthcalculations/health_calculations.R

# Check if bl exists. If not, then read it from an rds file
#if (!exists("bl")){
  # Read baseline from the rds file
  #bl <- readRDS('bl2014_p.rds')
  #bl <- readRDS('bl2014_p_v2.rds')
  bl <- readRDS('bl2014_APS_p.rds')
  
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

# Remove "80 - 84" age group for PA_mmets as well
PA_mmets <- subset(PA_mmets, age_group != "80 - 84")

# Save it as an rds file
saveRDS(PA_mmets, "./data/csv/mmets_regional.rds")

# Similar to mmets, remove "80 - 84" for CO2
CO2.Tm <- subset(CO2.Tm, age_group != "80 - 84")

# Save it as an rds file
saveRDS(CO2.Tm, "./data/csv/co2.rds")

# carMiles: remove "80 - 84" age group

carMiles <- subset(carMiles, age_group != "80 - 84")

# carMiles: save it as a rds file

saveRDS(carMiles, "./data/csv/carMiles_regional.rds")

# milesCycled.pers: remove "80 - 84" age group

milesCycled.pers <- subset(milesCycled.pers, age_group != "80 - 84")

# milesCycled.pers: save it as a rds file

saveRDS(milesCycled.pers, "./data/csv/milesCycled.pers_regional.rds")

## HEALTH FILES
# Generated after running health_calculations.R

# YLLs
saveRDS(yll, "./data/csv/ylls.rds")
saveRDS(yll_red, "./data/csv/yll_reduction.rds")

# Deaths
saveRDS(death, "./data/csv/deaths.rds")
saveRDS(death_red, "./data/csv/deaths_reduction.rds")

# 
# Aggregate file

# FileName 	MS 	ebike 	equity 	
# % of trips by bicycle 	% cyclists in the total population 	Miles cycled per person per week 	
# Car miles cycled per week 	Marginal METs per person per week 	Years of Life Lost (YLL) 	
# Car miles per person per week 	Car miles reduced per person per week 	CO2 (kg) from car travel per person per week

#df <- readRDS('../ICT/app/data/csv/ICT_aggr_regional.rds')


# Append yll column to the df
df[["Years of Life Lost (YLL)"]] <- 0
for (i in 1:10){
  for (j in 3:ncol(yll_aggr)){
    cname <- colnames(yll_aggr[j])
    cregion <- yll_aggr[i,"regions"]
    df[df$Scenario == cname & df$Region == cregion,]$`Years of Life Lost (YLL)` <- yll_aggr[i,j]
  }
}

# # Append yll column to the df
# df[["Years of Life Lost (YLL)"]] <- 0
# for (i in 2:ncol(yll_aggr)){
#   sindex <- ((i - 2) * 10 + 1)
#   eindex <- sindex + 9
#   #   cat(sindex, " - ", eindex, "\n")
#   # For baseline set yll to zero
#   val <- round(yll_aggr[1:10, i])
#   if (i == 2)
#     val <- 0
#   df[["Years of Life Lost (YLL)"]][sindex:eindex] <- val
# }


# Append yll column to the df
df[["Years of Life Lost (YLL) reduction (%)"]] <- 0
for (i in 1:10){
  for (j in 3:ncol(yll_red_aggr)){
    cname <- colnames(yll_red_aggr[j])
    cregion <- yll_red_aggr[i,"regions"]
    df[df$Scenario == cname & df$Region == cregion,]$`Years of Life Lost (YLL) reduction (%)` <- yll_red_aggr[i,j]
    # cat(cname, " ", cregion, " ", i , " ", j, " val ", yll_red_aggr[i,j])
  }
}

saveRDS(df, "./data/csv/ICT_aggr_regional.rds")

# save directProbCasesAboveGivenPerc which main role is to store info of every case in which Observed > DP

saveRDS(directProbCasesAboveGivenPerc, "./data/csv/dp_cases_above_given.rds")


# Create trips df
tripsdf <- create_trips(bl, listOfScenarios)

# Remove 80+ from the dataset
tripsdf <- subset(tripsdf, age_group != "80 - 84")

# Create trips time df
tripstimedf <- create_triptime(bl, listOfScenarios)

# Remove 80+ from the dataset
tripstimedf <- subset(tripstimedf, age_group != "80 - 84")

# Rename MainMode_Reduced columns to baseline
# tripsdf$baseline <- tripsdf$MainMode_Reduced
# tripsdf$MainMode_Reduced <- NULL

# Rename MainMode_Reduced columns to baseline - simpler approach 

names(tripsdf)[names(tripsdf)=='MainMode_Reduced'] <- 'baseline'

tripsdf$Cycled <- NULL


# Get row numbers with NA

temp <- data.frame(rn = which( is.na(tripsdf$baseline), arr.ind=TRUE))

tripsdf$X <- c(1:nrow(tripsdf))

tripstimedf$X <- c(1:nrow(tripstimedf))

# Remove all rows with NA in them

tripsdf <- (subset(tripsdf, !(X %in% temp$rn) ))

tripstimedf <- (subset(tripstimedf, !(X %in% temp$rn) ))

rm(temp)


# Precalculate trips used in "Journey Time" tab-

TripTotalTimeCalcs <- function(tripTime, tripMode){
  
  outputMainFolder <- './data/csv/TripTotalTime1_regional/'
  
  # all possible age_group + 'All'
  
  aaAgeGroups <- c(sort(unique(tripTime[, c("age_group")])), 'All')
  
  # all possible Sex + 3 (all)
  
  aaSexs <- c(sort(unique(tripTime[, c("Sex_B01ID")])), '3')
  
  # all possible Ethnicity + 'All'
  
  aaEthnicities <- c(sort(unique(tripTime[, c("EthGroupTS_B02ID")])), 'All')
  
  # all possible SES + 'All'
  
  aaSESs <- c(sort(unique(tripTime[, c("NSSec_B03ID")])), 'All')
  
  # all possible purposes + 'All'
  
  aaPurposes <- c(sort(unique(tripTime[, c("TripPurpose_B04ID")])), 'All')
  
  # all possible scenarios - not very elegant way
  
  aaScenarios <- colnames(tripTime)[11:(length(colnames(tripTime))-1)]
  
  #  as.data.frame(listOfScenarios) 
  # colnames(tripTime)[10:(length(colnames(tripTime))-1)]
  
  print(aaScenarios)
  
  # all possible regions
  
  aaRegions <- 0#sort(unique(tripTime[, c("HHoldGOR_B02ID")]))
  #aud: aaRegions <- aaRegions[1:3] # ADDED FOR TESTING - 
  
  # for every region
  
  for (aaRegion in aaRegions){
    
    print(aaRegion)
    
    dir.create(paste0(outputMainFolder, 'full/', aaRegion, '/histogram'), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0(outputMainFolder, 'full/', aaRegion, '/other'), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0(outputMainFolder, 'filtered/', aaRegion, '/histogram'), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0(outputMainFolder, 'filtered/', aaRegion, '/other'), showWarnings = FALSE, recursive = TRUE)
    
    # select data for region
    
    tripTimeRegion <- subset(tripTime, HHoldGOR_B02ID == aaRegion)
    tripModeRegion <- subset(tripMode, HHoldGOR_B02ID == aaRegion)
    
    for (aaScenario in aaScenarios){
      
      print(aaScenario)
      
      # scenario full data shouldn't be calculated every time, only once for every scenario to reduce number of operations
      
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
      
      #rm("temp")
      
      localtripData <- data[,c("X","TripTotalTime1", columnName)]
      
      #rm("data")
      
      localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$TripTotalTime1) / localtripData$TripTotalTime1 ) * 100)
      
      locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
      
      names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
      localtripData <- inner_join(localtripData, locatTripModeData, by = "rn")
      
      localtripData <- subset(localtripData, localtripData$baseline != localtripData[[columnName]])
      
      scTripTimeTraveldata <- localtripData
      
      #rm(list=c("localtripData", "locatTripModeData"))
      
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
        
        # add total population
        
        bc[,c('total_population')] <- nrow(scTripTimeTraveldata)

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
        
        # add total population
        
        data[,c('total_population')] <- nrow(scTripTimeTraveldata)
        
        tempScenarioOtherFreq <- bind_rows(tempScenarioOtherFreq, data)
        
      }
      
      # clean up mem
      
      #rm("scTripTimeTraveldata")
      
      # gc()
      
      # for filtered data
      
      tempScenarioFilteredHistogramFreq <- data.frame(stringsAsFactors = FALSE)
      tempScenarioFilteredOtherFreq <- data.frame(stringsAsFactors = FALSE)
      
      for (aaAgeGroup in aaAgeGroups){
        
        print(paste("age:", aaAgeGroup))
        
        for (aaSex in aaSexs){
          
          print(paste("sex:", aaSex))
          
          for (aaEthnicity in aaEthnicities){
            
            for (aaSES in aaSESs){
              
              for (aaPurpose in aaPurposes){
              
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
              
              if (aaPurpose != "All"){
                data <- subset(data, TripPurpose_B04ID %in% as.integer(aaPurpose))
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
              
              #rm("temp")
              
              # Remove all rows with NA in them
              locatTripModeData <- (subset(locatTripModeData, (X %in% selectedRows$X) ))
              
              localtripData <- data[,c("X","TripTotalTime1", columnName)]
              
              #rm("data")
              
              localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$TripTotalTime1) / localtripData$TripTotalTime1 ) * 100)
              
              #localtripData <- subset(localtripData, diff <= 200 & diff >= -200 )
              
              locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
              
              names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
              localtripData <- inner_join(localtripData, locatTripModeData, by = "rn")
              
              localtripData <- subset(localtripData, localtripData$baseline != localtripData[[columnName]])
              scFilteredTripTimeTraveldata <- localtripData
              
              #rm(list=c("localtripData", "locatTripModeData"))
              
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
                bc[,c('purpose')] <- aaPurpose
                
                # add sample size
                
                bc[,c('total_population')] <- nrow(scFilteredTripTimeTraveldata)

                tempScenarioFilteredHistogramFreq <- bind_rows(tempScenarioFilteredHistogramFreq, bc)

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
                data[,c('purpose')] <- aaPurpose
                
                # add sample size
                
                data[,c('total_population')] <- nrow(scFilteredTripTimeTraveldata)
                
                tempScenarioFilteredOtherFreq <- bind_rows(tempScenarioFilteredOtherFreq, data)
                
              }
              
              #rm("scFilteredTripTimeTraveldata", "bc", "data")
              
              #gc()
              
            }
          }
            
          }
          
        }
        
      }
      
      # for scenario full - add region column, add scenario values
      
      tempScenarioHistogramFreq[,c('region')] <- aaRegion

      saveRDS(tempScenarioHistogramFreq, paste0(outputMainFolder, 'full/', aaRegion, '/histogram/', aaScenario, '.rds'), compress = F)
      
      tempScenarioOtherFreq[,c('region')] <- aaRegion
      
      saveRDS(tempScenarioOtherFreq, paste0(outputMainFolder, 'full/', aaRegion, '/other/', aaScenario, '.rds'), compress = F)
      
      #rm(list=c("tempScenarioHistogramFreq", "tempScenarioOtherFreq"))
      
      # for filtered scenario - add region column, add scenario values
      
      tempScenarioFilteredHistogramFreq[,c('region')] <- aaRegion

      saveRDS(tempScenarioFilteredHistogramFreq, paste0(outputMainFolder, 'filtered/', aaRegion, '/histogram/', aaScenario, '.rds'), compress = F)
      
      tempScenarioFilteredOtherFreq[,c('region')] <- aaRegion
      
      saveRDS(tempScenarioFilteredOtherFreq, paste0(outputMainFolder, 'filtered/', aaRegion, '/other/', aaScenario, '.rds'), compress = F)
      
      #rm(list=c("tempScenarioFilteredHistogramFreq", "tempScenarioFilteredOtherFreq"))
      
      #gc()
      
    }
    
  }
}

TripTotalTimeCalcs(tripstimedf, tripsdf)

# Precalculate trips used in "Mode Share" tab

tripsDFCalcs <- function(tripMode){
  
  appendMissingFrequencies <- function( df1, df2){
    
    # store names in case if df2 is empty
    
    df2Names <- colnames(df2)
    
    missingModes <- setdiff(df1[,2], df2[,1])
    if (nrow(df2) < 8){
      for (i in (1:length(missingModes))){
        df2 <- rbind(df2,c(missingModes[i], 0))
      }
    }
    
    colnames(df2) <- df2Names
    df2
  }
  
  # tp_mode also defined in data-processing.R in ICT
  
  tp_mode <- data.frame (mode = c("Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"), code = c(1, 2, 2.5, c(3:7)))
  
  outputMainFolder <- './data/csv/tripsdf_regional/'
  
  # all possible age_group + 'All'
  
  aaAgeGroups <- c(sort(unique(tripMode[, c("age_group")])), 'All')
  
  # all possible Sex + 3 (all)
  
  aaSexs <- c(sort(unique(tripMode[, c("Sex_B01ID")])), '3')
  
  # all possible Ethnicity + 'All'
  
  aaEthnicities <- c(sort(unique(tripMode[, c("EthGroupTS_B02ID")])), 'All')
  
  # all possible SES + 'All'
  
  aaSESs <- c(sort(unique(tripMode[, c("NSSec_B03ID")])), 'All')
  
  # all possible scenarios (baseline should be added) - not very elegant way
  
  aaScenarios <- c('baseline', colnames(tripMode)[9:(length(colnames(tripMode))-1)])
  
  print(aaScenarios)
  
  # all possible regions
  
  aaRegions <- sort(unique(tripMode[, c("HHoldGOR_B02ID")]))
  
  # for every region
  
  for (aaRegion in aaRegions){
    
    print(aaRegion)
    
    dir.create(paste0(outputMainFolder, 'full/', aaRegion), showWarnings = FALSE, recursive = TRUE)
    dir.create(paste0(outputMainFolder, 'filtered/', aaRegion), showWarnings = FALSE, recursive = TRUE)
    
    # select data for region
    
    tripModeRegion <- subset(tripMode, HHoldGOR_B02ID == aaRegion)
    
    # for every scenario (including baseline)
    
    for (aaScenario in aaScenarios){
      
      print (aaScenario)
      
      # calc full data - all scenarios + baseline
      
      # TODO: check - in prev version instead of fullScenarioData names: baseline and scenario were used - not sure if this is needed
      
      fullScenario <- data.frame(fullScenarioData = tripModeRegion[[aaScenario]])
      
      fullScenario <- plyr::count(fullScenario)
      
      total_population <- sum(fullScenario$freq, na.rm = T)
      
      fullScenario$freq <- round(fullScenario$freq / sum(fullScenario$freq) * 100, digit = 1)
      # Remove NA row from the dataset
      fullScenario <- subset(fullScenario, !is.na(fullScenario[,1]))
      
      fullScenario <- appendMissingFrequencies(tp_mode, fullScenario)
      
      fullScenario <- arrange(fullScenario, fullScenario[,1])
      
      fullScenario$total_population <- total_population
      
      # calc filtered data - all scenarios + baseline
      
      # df for results
      
      filteredScenario <- data.frame(stringsAsFactors = FALSE)
      
      # get only one scenario/baseline column
      
      colList <- c("ID","age_group", "Sex_B01ID", "NSSec_B03ID", "EthGroupTS_B02ID", aaScenario)
      
      dataRegion <- tripModeRegion[,colList]
      
      # iterate over combination of all filters
      
      for (aaAgeGroup in aaAgeGroups){
        
        print(paste("age:", aaAgeGroup))
        
        for (aaSex in aaSexs){
          
          print(paste("sex:", aaSex))
          
          for (aaEthnicity in aaEthnicities){
            
            for (aaSES in aaSESs){
              
              data <- dataRegion
              
              if (aaAgeGroup != 'All'){
                
                data <- subset(data, age_group == aaAgeGroup)
              }
              
              if (aaSex != 3){
                
                data <- subset(data, Sex_B01ID %in% as.integer(aaSex))
              }
              
              if (aaSES != "All"){
                
                data <- subset(data, NSSec_B03ID %in% as.integer(aaSES))
              }
              
              if (aaEthnicity != "All"){
                
                data <- subset(data, EthGroupTS_B02ID %in% as.integer(aaEthnicity))
              }
              
              filteredScenarioTemp <- plyr::count(data, aaScenario)
              
              names(filteredScenarioTemp)[names(filteredScenarioTemp)== aaScenario] <- "filteredScenarioData"
              
              total_population <- sum(filteredScenarioTemp$freq, na.rm = T)
              
              filteredScenarioTemp$freq <- round(filteredScenarioTemp$freq / sum(filteredScenarioTemp$freq) * 100, digit = 1)
              # Remove NA row from the dataset
              filteredScenarioTemp <- subset(filteredScenarioTemp, !is.na(filteredScenarioTemp[,1]))
              
              filteredScenarioTemp <- appendMissingFrequencies(tp_mode, filteredScenarioTemp)
              
              filteredScenarioTemp <- arrange(filteredScenarioTemp, filteredScenarioTemp[,1])
              
              filteredScenarioTemp$total_population <- total_population
              
              # add columns with filtered combinations
              
              filteredScenarioTemp[,c('agegroup')] <- aaAgeGroup
              filteredScenarioTemp[,c('gender')] <- aaSex
              filteredScenarioTemp[,c('ethnicity')] <- aaEthnicity
              filteredScenarioTemp[,c('ses')] <- aaSES
              
              # add data to whole region-scenario data
              
              filteredScenario <- bind_rows(filteredScenario, filteredScenarioTemp)
              
            }
          }
        }
      }
      
      # add region data
      
      fullScenario[,c('region')] <- aaRegion
      
      filteredScenario[,c('region')] <- aaRegion
      
      # save full data as rds file
      
      saveRDS(fullScenario, paste0(outputMainFolder, 'full/', aaRegion, '/', aaScenario, '.rds'))
      
      saveRDS(filteredScenario, paste0(outputMainFolder, 'filtered/', aaRegion, '/', aaScenario, '.rds'))
      
      rm(list=c("fullScenario", "filteredScenario"))
      
    }
    
    rm("tripModeRegion")
    
    gc()
  }
  
}

tripsDFCalcs(tripsdf)

save.image('after_ff_interface1.RData')
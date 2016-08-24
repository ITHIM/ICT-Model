# Script to generate rds file for the interface
# PLEASE NOTE: It assumes that all scenarios have been generated, and three additional scripts have been run, which are:
# 1. 3_CBM2_Aggregates-2012.R
# 2. 6_CBM2_ScenarioAggregates.R
# 3. /healthcalculations/health_calculations.R

# Check if bl exists. If not, then read it from an rds file
#if (!exists("bl")){
  # Read baseline from the rds file
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

# Create trips df
tripsdf <- create_trips(bl, listOfScenarios)

# Remove 80+ from the dataset
tripsdf <- subset(tripsdf, age_group != "80 - 84")

# Save it as an rds file
# Assuming ICT is a sibling of ICT-Model repo
saveRDS(tripsdf, "../ICT/app/data/csv/tripsdf_regional.rds")


# Create trips time df
tripstimedf <- create_trips(bl, listOfScenarios)

# Remove 80+ from the dataset
tripstimedf <- subset(tripstimedf, age_group != "80 - 84")

# Save it as an rds file
# Assuming ICT is a sibling of ICT-Model repo
saveRDS(tripstimedf, "../ICT/app/data/csv/TripTotalTime1_regional.rds")

# Remove "80 - 84" age group for PA_mmets as well
PA_mmets <- subset(PA_mmets, age_group != "80 - 84")

# Save it as an rds file
saveRDS(PA_mmets, "../ICT/app/data/csv/mmets_regional.rds")

# Similar to mmets, remove "80 - 84" for CO2
CO2.Tm <- subset(CO2.Tm, age_group != "80 - 84")

# Save it as an rds file
saveRDS(PA_mmets, "../ICT/app/data/csv/co2.rds")

# carMiles: remove "80 - 84" age group

carMiles <- subset(carMiles, age_group != "80 - 84")

# carMiles: save it as a rds file

saveRDS(carMiles, "../ICT/app/data/csv/carMiles_regional.rds")

# milesCycled.pers: remove "80 - 84" age group

milesCycled.pers <- subset(milesCycled.pers, age_group != "80 - 84")

# milesCycled.pers: save it as a rds file

saveRDS(milesCycled.pers, "../ICT/app/data/csv/milesCycled.pers_regional.rds")

## HEALTH FILES
# Generated after running health_calculations.R

# YLLs
saveRDS(yll, "../ICT/app/data/csv/ylls.rds")
saveRDS(yll_red, "../ICT/app/data/csv/yll_reduction.rds")

# Deaths
saveRDS(death, "../ICT/app/data/csv/deaths.rds")
saveRDS(death_red, "../ICT/app/data/csv/deaths_reduction.rds")

# 
# Aggregate file

# FileName 	MS 	ebike 	equity 	
# % of trips by bicycle 	% cyclists in the total population 	Miles cycled per person per week 	
# Car miles cycled per week 	Marginal METs per person per week 	Years of Life Lost (YLL) 	
# Car miles per person per week 	Car miles reduced per person per week 	CO2 (kg) from car travel per person per week

# Append yll column to the df
df[["Years of Life Lost (YLL)"]] <- 0
for (i in 2:ncol(yll_aggr)){
  sindex <- ((i - 2) * 10 + 1)
  eindex <- sindex + 9
  #   cat(sindex, " - ", eindex, "\n")
  # For baseline set yll to zero
  val <- round(yll_aggr[1:10, i])
  if (i == 2)
    val <- 0
  df[["Years of Life Lost (YLL)"]][sindex:eindex] <- val
}

saveRDS(df, "../ICT/app/data/csv/ICT_aggr_regional.rds")
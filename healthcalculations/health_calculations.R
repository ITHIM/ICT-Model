source("init.R")
source("healthcalculations/functions.R")
require(dplyr)
require(sqldf)

b_hmmets <- health_mmets

#add baseline (1st colum of results)
local_listOfScenarios <- c('baseline', listOfScenarios)

# Read baseline from the rds file
bl <- readRDS('bl2014_p.rds')

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

# Remove "80 - 84" age group from the baseline
bl <- subset(bl, bl$age_group != "80 - 84")


local_bl <- sqldf("Select ID, age_group,Sex_B01ID,EthGroupTS_B02ID, NSSec_B03ID, HHoldGOR_B02ID from bl Group by ID,
HHoldGOR_B02ID")
mmets <- health_mmets
# Replace NAs with 0
mmets[is.na(mmets)] <- 0

# Remove "80 - 84" age group for the interface
mmets <- subset(mmets, mmets$age_group != "80 - 84")

rr <- mmet2RR(mmets, local_listOfScenarios)

pif <- PAF(rr, T)
pif <- data.frame(pif)
pif <- arrange(pif, regions, age.band, gender)
gbd <- read.csv("healthcalculations/GBDtoNTS/output/GBDtoNTS_output.csv", header = T, as.is = T)
gbd <- arrange(gbd, region, age, gender)

# Convert pif columns' classes from factor to character and numeric
pif$age.band <- as.character(pif$age.band)
for (i in 2:ncol(pif)){
  pif[,i] <- as.numeric.factor(pif[,i])
  
}


yll_dfs <- combine_health_and_pif(pif, gbd, "yll")

yll <- as.data.frame(yll_dfs[1])
yll_red <- as.data.frame(yll_dfs[2])


# tdpif <- PAF(rr, T)
# tdpif <- data.frame(tdpif)
# tdpif <- arrange(tdpif, regions, age.band, gender)
# 
# 
# td <- combine_health_and_pif(pif, gbd, "yll")
# td1 <- as.data.frame(td[2])
# 
# td1$age.band <- reduced_age_lt$red.age.band[match(td1$age.band, reduced_age_lt$age.band)]
# 
# td1 <- aggregate(td1[-c(1, 2, 3)], by=list(td1$age.band, td1$gender, td1$regions),FUN=mean, na.rm=TRUE)
# 
# colnames(td1)[1:3] <- c("age.band", "gender", "regions")

# Convert into percent by multiplying it with 100
# td1[5:32] <- round(td1[5:32] * 100, 2)


death_dfs <- combine_health_and_pif(pif, gbd, "death")

death <- as.data.frame(death_dfs[1])
death_red <- as.data.frame(death_dfs[2])

# yll <- combine_age_groups(yll)

# Read a lookup table with reduced age groups
reduced_age_lt <- read.csv("healthcalculations/reduced_age_lookuptable.csv", header = T)
yll$age.band <- reduced_age_lt$red.age.band[match(yll$age.band, reduced_age_lt$age.band)]

yll <- aggregate(yll[-c(1, 2, 3)], by=list(yll$age.band, yll$gender, yll$regions),FUN=sum, na.rm=TRUE)

colnames(yll)[1:3] <- c("age.band", "gender", "regions")

# Round yll values
yll[,4:ncol(yll)] <- round(yll[,4:ncol(yll)])


death$age.band <- reduced_age_lt$red.age.band[match(death$age.band, reduced_age_lt$age.band)]

death <- aggregate(death[-c(1, 2, 3)], by=list(death$age.band, death$gender, death$regions),FUN=sum, na.rm=TRUE)

colnames(death)[1:3] <- c("age.band", "gender", "regions")


yll_red$age.band <- reduced_age_lt$red.age.band[match(yll_red$age.band, reduced_age_lt$age.band)]

yll_red <- aggregate(yll_red[-c(1, 2, 3)], by=list(yll_red$age.band, yll_red$gender, yll_red$regions),FUN=mean, na.rm=TRUE)

colnames(yll_red)[1:3] <- c("age.band", "gender", "regions")


death_red$age.band <- reduced_age_lt$red.age.band[match(death_red$age.band, reduced_age_lt$age.band)]

death_red <- aggregate(death_red[-c(1, 2, 3)], by=list(death_red$age.band, death_red$gender, death_red$regions),FUN=mean, na.rm=TRUE)

colnames(death_red)[1:3] <- c("age.band", "gender", "regions")

# Convert into percent by multiplying it with 100
yll_red[5:32] <- round(yll_red[5:32] * 100, 2)

# td <- yll_red
# td[5:32] <- round(yll_red[5:32] * 100, 3)
yll_aggr <- aggregate(yll[-c(1, 2, 3)], by=list(yll$regions),FUN=sum, na.rm=TRUE)
colnames(yll_aggr)[1:2] <- c("regions", "baseline")
# Not working
#yll_red <- calculate_health_reductions(pif, gbd, td, hm = "yll")

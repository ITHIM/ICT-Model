rm(list=ls())

library(dplyr)
library(lazyeval)

# 'Scotland', 'Wales' 'Northern Ireland' are not used
regionsList <- c('East of England', 
                 'Greater London', 
                 'North East England', 
                 'North West England',
                 'South East England', 
                 'South West England',
                 'East Midlands',
                 'West Midlands', 
                 'Yorkshire and the Humber')

regions <- c('England'  = 0,
             'North East England' = 1,
             'North West England' = 2,
             'Yorkshire and the Humber' = 3,
             'East Midlands' = 4,
             'West Midlands' = 5,
             'East of England'= 6,
             'Greater London' = 7,
             'South East England' = 8,
             'South West England' = 9)

finalAgeGroups <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79")

outputDF = data.frame()

for (regionName in regionsList){
  
  death <- read.csv(file=file.path('./health calculations/GBDtoNTS/GBD_figures', paste(regionName, '-Deaths.csv_agesex.csv', sep = '')), stringsAsFactors = FALSE)
  yld <- read.csv(file=file.path('./health calculations/GBDtoNTS/GBD_figures', paste(regionName, '-YLDs.csv_agesex.csv', sep = '')), stringsAsFactors = FALSE)
  yll <- read.csv(file=file.path('./health calculations/GBDtoNTS/GBD_figures', paste(regionName, '-YLLs.csv_agesex.csv', sep = '')), stringsAsFactors = FALSE)
  daly <- read.csv(file=file.path('./health calculations/GBDtoNTS/GBD_figures', paste(regionName, '-DALYs.csv_agesex.csv', sep = '')), stringsAsFactors = FALSE)
  
  outputRegion <- data.frame(age=rep(finalAgeGroups, times=2),
                             sex=rep(c(1, 2), each=6), stringsAsFactors = FALSE)

  for (indicator in c('yld', 'yll', 'daly', 'death')){
    
    indicatorInput <- get(indicator)
    
    indicatorSubset <- indicatorInput[indicatorInput$sex %in% c(1, 2) & indicatorInput$age %in% seq(8, 20), c('sex', 'age', 'nm_mean', 'location_name')]
    indicatorName <- indicatorInput[1, c('metric_name')]
    
    # change nm_mean column to indicator name
    
    colnames(indicatorSubset)[3] <- indicatorName
    
    # 2/5 of group number 8 (15-19, from which we want only 18-19)
    
    indicatorSubset[indicatorSubset$age == 8, indicatorName] <- indicatorSubset[indicatorSubset$age == 8, indicatorName] * 2/5
    
    # recode agecat -> groups: 8, 9, 10 should be treated as one group, 11 and 12 as one, 13 and 14 as one etc.
    
    newVa <- data.frame(old = seq(8, 20), new = append(finalAgeGroups[1], rep(finalAgeGroups, each=2)), stringsAsFactors = FALSE)
  
    indicatorSubset$age <- newVa$new[match(indicatorSubset$age, newVa$old)]
  
    # sum by sex, age cat
  
    indicatorSubset <- indicatorSubset %>%
                        group_by_(.dots=c('sex', 'age')) %>%
                        summarise_(.dots=setNames(interp('sum(indName)', indName = as.name(indicatorName)), indicatorName))
     
    outputRegion <- inner_join(outputRegion, indicatorSubset, by = c('age' = 'age', 'sex' = 'sex'))
  
  }

  outputRegion$regionName <- regionName
  
  outputDF <- rbind(outputDF, outputRegion)
  
}

# England sums

england <- outputDF %>%
            select_(.dots=c('-regionName')) %>%
            group_by_(.dots=c('sex', 'age')) %>%
            summarise(YLDs=sum(YLDs), YLLs=sum(YLLs), DALYs=sum(DALYs), Deaths=sum(Deaths))

england$regionName <- 'England'

# add England to output

outputDF <- rbind(outputDF, england)

# recode regions

outputDF$region <- mapply(function(whichRegion) {
  regions[whichRegion]
}, outputDF$regionName)

# remove regions' names

outputDF <- outputDF[-7]

colnames(outputDF) <- c('age', 'gender', 'yld', 'yll', 'daly', 'death', 'region')

write.csv(outputDF, file=file.path('./health calculations/GBDtoNTS/output', 'GBDtoNTS_output.csv'), row.names=F)
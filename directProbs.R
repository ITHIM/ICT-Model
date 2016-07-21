#' Return IDs of all ppl (including also these who were cyclist already) who became potential cyclist

directProbPPLIDs <- function(baselineSubset, DP, ebikes, equity, pcycl_baseline) {
  
  # if DP == 1 -> don't process just return all IDs
  
  if (DP == 1){
    
    return(unique(baselineSubset$ID))
    
  }
  
  # calc number of cyclist in baselineSubset
  
  totalNumberOfCyclistInBaselineSubset <- length(unique(baselineSubset[baselineSubset$Cycled == 1,]$ID))
  
  # if observed proportion of cyclist in baselineSubset > DP value -> don't process, just return IDs of all cyclist, print info in a console.
  # These cases could be filtered out in UI
  
  shareOfCyclistInBaselineSubset <- totalNumberOfCyclistInBaselineSubset / length(unique(baselineSubset$ID))
  
  if (shareOfCyclistInBaselineSubset > DP){
    
    print('Observed > DP')
    print(shareOfCyclistInBaselineSubset)
    
    return(unique(baselineSubset[baselineSubset$Cycled == 1,]$ID))
  }
  
  # just in case reset cyclist column
  
  baselineSubset$cyclist <- 0
  
  # init vector with IDs of ppl who are cyclist already
  
  IDOfPplAlreadyCyclist = c()
  
  # init vector with IDs of ppl who are going to become cyclist
  
  IDOfPplBecomingCyclist = c()
  
  # init vector with IDs for output (combining IDOfPplAlreadyCyclist + IDOfPplBecomingCyclist)
  
  IDOfPplAllCyclistOutput = c()
  
  # init counter of remaining ppl (ppl that should be pick up from other subgroups because of lack of ppl in particular subgroups)
  
  remainingCyclistsCounter = 0
  
  # work out proportion of cyclists by age-sex subgroups in baselineSubset
  
  cyclistsPropBySubgroups <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'))
  
  cyclistsPropBySubgroups$prop <-mapply(function(whichGroup) {
    round(length(unique(baselineSubset[baselineSubset$Cycled == 1 & baselineSubset$agesex == whichGroup,]$ID))/totalNumberOfCyclistInBaselineSubset, digits = 2)
  }, cyclistsPropBySubgroups$agesex)
  
  # calc how many cyclists should be drawn excluding # of ppl who already cycle
  
  howManyCyclistNeeded <- round(DP * length(unique(baselineSubset$ID)), digits = 0) - totalNumberOfCyclistInBaselineSubset
  
  # just in case check if number exceeds # of all ppl
  
  howManyCyclistNeeded <- ifelse(howManyCyclistNeeded > length(unique(baselineSubset$ID)), length(unique(baselineSubset$ID)), howManyCyclistNeeded)
  
  # cyclists in a population should be marked as cyclists for all trips
  
  IDOfPplAlreadyCyclist <- unique(baselineSubset[baselineSubset$Cycled == 1, ]$ID)
  
  baselineSubset[baselineSubset$ID %in% IDOfPplAlreadyCyclist, ]$cyclist <- 1
  
  if (equity == 0) {
    
    for (i in seq_len(nrow(cyclistsPropBySubgroups))){
      
      # calc how many cyclists should be drawn, taking into account cyclists prop
      
      projectedCyclistsInSubgroup <- round(as.numeric(cyclistsPropBySubgroups[i, ]$prop) * howManyCyclistNeeded, digits = 0)
      
      # check if there are enough ppl from subgroup in a population; if more are selected -> use total number of subgroup members
      
      realCyclistsInSubgroup <- ifelse(projectedCyclistsInSubgroup > length(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex), ]$ID)), length(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex), ]$ID)), projectedCyclistsInSubgroup)
      
      # pick up ppl who become cyclist but are not cyclist already
      
      subgroupIDsOfPplBecomeCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex),]$ID), realCyclistsInSubgroup, replace = F)
      
      IDOfPplBecomingCyclist <- append(IDOfPplBecomingCyclist, subgroupIDsOfPplBecomeCyclist)
      
      # work out remaining diff (if value > 0 this means that sample should be filled with ppl from other subgroups)
      
      remainingCyclistsCounter <- remainingCyclistsCounter + ifelse(projectedCyclistsInSubgroup - length(subgroupIDsOfPplBecomeCyclist) <= 0, 0, projectedCyclistsInSubgroup - length(subgroupIDsOfPplBecomeCyclist))
      
    }
    
    # fill scenario with ppl from other subgroups if remaining ppl exist
    
    if (remainingCyclistsCounter > 0){
      
      filledIDsOfPplBecomeCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1 & !(baselineSubset$ID %in% IDOfPplBecomingCyclist),]$ID), remainingCyclistsCounter, replace = F)
      
      IDOfPplBecomingCyclist <- append(IDOfPplBecomingCyclist, filledIDsOfPplBecomeCyclist)
    }
    
    # combine IDOfPplAlreadyCyclist, IDOfPplBecomingCyclist
    
    IDOfPplAllCyclistOutput <- append(IDOfPplAlreadyCyclist, IDOfPplBecomingCyclist)
    
    # return IDs
    
    return(IDOfPplAllCyclistOutput)
    
  } else {
    
    # pick up randomly ppl who are not cyclist using same prob. for all
    
    IDOfPplBecomingCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1,]$ID), howManyCyclistNeeded, replace = F)
    
    # combine IDOfPplAlreadyCyclist, IDOfPplBecomingCyclist
    
    IDOfPplAllCyclistOutput <- append(IDOfPplAlreadyCyclist, IDOfPplBecomingCyclist)
    
    # return IDs
    
    return(IDOfPplAllCyclistOutput)
    
  }
}
context("datasets comparision: mean used, form of datasets: one column = one scenario, all regions")

# which datasets should be compared

datasetsToCompareColumnScenario <- c("carMiles_regional.rds",
                                     "co2.rds",
                                     "deaths_reduction.rds",
                                     "deaths.rds",
                                     "milesCycled.pers_regional.rds",
                                     "mmets_regional.rds",
                                     "yll_reduction.rds",
                                     "ylls.rds")

# datasets in which every column stores data for one scenario 

test_that("for every scenario means in both data versions are almost equal (form of datasets: one column == one scenario)", {
  
  # for every dataset
  
  for (ds in datasetsToCompareColumnScenario){
    
    # read firstData and secondData
    
    firstDataMain <- readRDS(paste0(firstDataDir, ds))
    secondDataMain <- readRDS(paste0(secondDataDir, ds))
    
    # iterate over regions
    
    for (region in regionsToTest){
      
      # subset data files
      
      if ('regions' %in% names(firstDataMain)){ # in "deaths_reduction.rds" there is 'regions' column
        
        firstData <- subset(firstDataMain, regions == region, select = scenariosToTest)
        secondData <- subset(secondDataMain, regions == region, select = scenariosToTest)
        
      } else {
      
        firstData <- subset(firstDataMain, HHoldGOR_B02ID == region, select = scenariosToTest)
        secondData <- subset(secondDataMain, HHoldGOR_B02ID == region, select = scenariosToTest)
      
      }
      
      # for every scenario
      
      for (sc in scenariosToTest){
        
        firstDataSc <- subset(firstData, select = sc)
        secondDatSc <- subset(secondData, select = sc)
        
        expect_equal(mean(firstDataSc[,sc], na.rm = T), mean(secondDatSc[,sc], na.rm = T), info = paste0('|', ds, '| region: ', region, '| ', sc, '| tolerance: ', tolerance), tolerance = tolerance)
        
      }
      
    }
  }
  
  
})


context("datasets comparision: mean used, form of datasets: one column = one scenario, only region 0")

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
    
    firstData <- readRDS(paste0(firstDataDir, ds))
    secondData <- readRDS(paste0(secondDataDir, ds))
    
    # subset datas: at the present moment compare only data for region = 0 and needed scenarios
    
    if ('regions' %in% names(firstData)){ # in "deaths_reduction.rds" there is 'regions' column
      
      firstData <- subset(firstData, regions == 0, select = scenariosToTest)
      secondData <- subset(secondData, regions == 0, select = scenariosToTest)
      
    } else {
    
      firstData <- subset(firstData, HHoldGOR_B02ID == 0, select = scenariosToTest)
      secondData <- subset(secondData, HHoldGOR_B02ID == 0, select = scenariosToTest)
    
    }
    
    # for every scenario
    
    for (sc in scenariosToTest){
      
      firstDataSc <- subset(firstData, select = sc)
      secondDatSc <- subset(secondData, select = sc)
      
      expect_equal(firstDataSc, secondDatSc, info = paste0(ds, ': ', sc))
      
    }
  }
  
  
})


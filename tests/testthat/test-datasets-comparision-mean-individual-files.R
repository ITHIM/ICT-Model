context("datasets comparision: mean used, form of datasets: individual files, only region 0")

test_that("in ICT_aggr_regional.rds for every scenario means in both data versions are almost equal (form of datasets: individual files)", {
  
  filename <- c("ICT_aggr_regional.rds")
  
  colToBeTested <- c("Car Miles Per person (per week)", "Car Miles Reduced Per person (per week)", 
                     "Total Car Miles Cycled (per week)", "Total Miles Cycled (per week)", 
                     "Miles Cycled Per Male (per week)", "Miles Cycled Per Female (per week)", 
                     "Miles Cycled Per White Person (per week)", "Miles Cycled Per Non-White Person (per week)", 
                     "Miles Cycled Per Person with Car-Access (per week)", "Miles Cycled Per Person with No Car-Access (per week)", 
                     "Marginal METs Per Person (per week)", "Transport CO2 Per Person (per week)", 
                     "Total Time Saved in minutes by Cyclists (per week)", "% Cyclists in the Total Population", 
                     "% of Trips by Bicycle", "Years of Life Lost (YLL)")
  
  # read firstData and secondData
  
  firstDataMain <- readRDS(paste0(firstDataDir, filename))
  secondDataMain <- readRDS(paste0(secondDataDir, filename))
  
  # iterate over scenarios
  
  for (region in regionsToTest){
    
    # subset both data versions
    
    firstData <- subset(firstDataMain, Region == region & Scenario %in% scenariosToTest)
    secondData <- subset(secondDataMain, Region == region & Scenario %in% scenariosToTest)
    
    # for every scenario
    
    for (sc in scenariosToTest){
      
      # extract data
      
      firstDataSc <- subset(firstData, Scenario == sc)
      secondDatSc <- subset(secondData, Scenario == sc)
      
      # for every column to be tested
      
      for (ct in colToBeTested){
        
        expect_equal(firstDataSc[[ct]], secondDatSc[[ct]], info = paste0('Region: ', region, ': ', sc, ': ', ct, ' tolerance: ', tolerance), tolerance = tolerance)
        
      }
      
    }
    
  }
  
})

test_that("dp_cases_above_given.rds - both data versions are equal (form of datasets: individual files)", {
  
  filename <- c("dp_cases_above_given.rds")
  
  # read firstData and secondData
  
  firstData <- readRDS(paste0(firstDataDir, filename))
  secondData <- readRDS(paste0(secondDataDir, filename))
  
  # sort both data versions 
  
  firstData <- firstData[with(firstData, order(region, MS, ebikes, equity)),]
  secondData <- secondData[with(secondData, order(region, MS, ebikes, equity)),]
  
  expect_equal(firstData, secondData)
  
})
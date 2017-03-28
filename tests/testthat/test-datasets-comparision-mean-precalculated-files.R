context("datasets comparision: mean used, form of datasets: files with precalcuated data, all regions")

test_that("in tripsdf_regional for every scenario results in both data versions are almost equal (only full)", {

  # iterate over regions
  
  for (region in regionsToTest){
  
    directoryWithData <- paste0(c('tripsdf_regional/full/'), region, '/')
    
    # for every scenario
    
    for (sc in scenariosToTest){
      
      # read firstData and secondData
      
      firstData <- readRDS(paste0(firstDataDir, directoryWithData, sc, '.rds'))
      secondData <- readRDS(paste0(secondDataDir, directoryWithData, sc, '.rds'))
      
      # sort both
      
      firstData <- firstData[with(firstData, order(fullScenarioData, freq, total_population, region)),]
      secondData <- secondData[with(secondData, order(fullScenarioData, freq, total_population, region)),]
      
      # for every row
      
      for (rt in seq_len(nrow(firstData))){
        
        # for every column
        
        colsToBeTested <- c("fullScenarioData", "freq", "total_population")
        
        for (ct in colsToBeTested){
          
          expect_equal(firstData[rt, ct], secondData[rt, ct], info = paste0('tripsdf_regional: |', sc, '| ', ct, '| region: |', region, '| tolerance: ', tolerance), tolerance = tolerance)
          
        }
        
      }
      
    }
  }
})

test_that("in TripTotalTime1_regional for every scenario results in both data versions are almost equal - histogram subgroup (only full)", {

  # iterate over regions
  
  for (region in regionsToTest){
  
    directoryWithData <- paste0(c('TripTotalTime1_regional/full/'), region, c('/histogram/'))
  
    # for every scenario
    
    for (sc in scenariosToTest){
      
      # read firstData and secondData
      
      firstData <- readRDS(paste0(firstDataDir, directoryWithData, sc, '.rds'))
      secondData <- readRDS(paste0(secondDataDir, directoryWithData, sc, '.rds'))
      
      # sort both
      
      firstData <- firstData[with(firstData, order(region, freq, umode, total_population)),]
      secondData <- secondData[with(secondData, order(region, freq, umode, total_population)),]
      
      # for every row
      
      for (rt in seq_len(nrow(firstData))){
        
        # for every column
        
        colsToBeTested <- c("freq", "umode", "total_population", sc)
        
        for (ct in colsToBeTested){
          
          if(is.character(firstData[[ct]])){
            
            expect_true(firstData[rt, ct] == secondData[rt, ct], info = paste0('TripTotalTime1_regional - histogram subgroup |', sc, '|', ct, '| region: |', region, '| row: |', rt))
            
          } else {
          
            expect_equal(firstData[rt, ct], secondData[rt, ct], info = paste0('TripTotalTime1_regional - histogram subgroup |', sc, '|', ct, '| region: |', region, '| row: |', rt, '| tolerance: ', tolerance), tolerance = tolerance)
            
          }
        }
        
      }
      
    }
  }
})

test_that("in TripTotalTime1_regional for every scenario results in both data versions are almost equal - other subgroup", {
  # iterate over regions
  
  for (region in regionsToTest){
    
    directoryWithData <- paste0(c('TripTotalTime1_regional/full/'), region, c('/other/'))
    
    # for every scenario
    
    for (sc in scenariosToTest){
      
      # read firstData and secondData
      
      firstData <- readRDS(paste0(firstDataDir, directoryWithData, sc, '.rds'))
      secondData <- readRDS(paste0(secondDataDir, directoryWithData, sc, '.rds'))
      
      # sort both
      
      firstData <- firstData[with(firstData, order(region, counts, umode, total_population)),]
      secondData <- secondData[with(secondData, order(region, counts, umode, total_population)),]
      
      # for every row
      
      for (rt in seq_len(nrow(firstData))){
        
        # for every column
        
        colsToBeTested <- c("counts", "umode", "total_population", sc)
        
        for (ct in colsToBeTested){
          
          if(is.character(firstData[[ct]])){
            
            expect_true(firstData[rt, ct] == secondData[rt, ct], info = paste0('TripTotalTime1_regional - other subgroup | ', sc, '|', ct, '| region: |', region, '| row: |', rt))
            
          } else if (is.factor(firstData[[ct]])){
              
            expect_true(as.character(firstData[rt, ct]) == as.character(secondData[rt, ct]), paste0('TripTotalTime1_regional - other subgroup |', sc, '|', ct, '| region: |', region, '| row: |', rt))
            
          } else {
            
            expect_equal(firstData[rt, ct], secondData[rt, ct], info = paste0('TripTotalTime1_regional - other subgroup |', sc, '|', ct, '| region: |', region, '| row: |', rt, '|: tolerance: ', tolerance), tolerance = tolerance)
            
          }
        }
        
      }
      
    }
  }
})
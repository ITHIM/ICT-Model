context("datasets comparision: mean used, form of datasets: files with precalcuated data, only region 0")

test_that("in tripsdf_regional for every scenario results in both data versions are almost equal", {
  
  directoryWithData <- c('tripsdf_regional/full/0/')
  
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
        
        expect_equal(firstData[rt, ct], secondData[rt, ct], info = paste0(sc, ': ', ct, ': tolerance: ', tolerance), tolerance = tolerance)
        
      }
      
    }
    
  }
})

test_that("in TripTotalTime1_regional for every scenario results in both data versions are almost equal - histogram subgroup", {
  
  directoryWithData <- c('TripTotalTime1_regional/full/0/histogram/')

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
          
          expect_true(firstData[rt, ct] == secondData[rt, ct], info = paste0(sc, ': ', ct))
          
        } else {
        
          expect_equal(firstData[rt, ct], secondData[rt, ct], info = paste0(sc, ': ', ct, ': tolerance: ', tolerance), tolerance = tolerance)
          
        }
      }
      
    }
    
  }
})

test_that("in TripTotalTime1_regional for every scenario results in both data versions are almost equal - other subgroup", {
  
  directoryWithData <- c('TripTotalTime1_regional/full/0/other/')
  
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
          
          expect_true(firstData[rt, ct] == secondData[rt, ct], info = paste0(sc, ': ', ct))
          
        } else if (is.factor(firstData[[ct]])){
            
          expect_true(as.character(firstData[rt, ct]) == as.character(secondData[rt, ct]), info = paste0(sc, ': ', ct))
          
        } else {
          
          expect_equal(firstData[rt, ct], secondData[rt, ct], info = paste0(sc, ': ', ct, ': tolerance: ', tolerance), tolerance = tolerance)
          
        }
      }
      
    }
    
  }
})
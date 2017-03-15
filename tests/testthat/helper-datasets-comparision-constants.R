mainPath <- c("../../")

# which scenarios should be compared

scenariosToTest <- c("MS0.05_ebik0_eq0", "MS0.05_ebik0_eq1", "MS0.05_ebik1_eq0", 
                     "MS0.05_ebik1_eq1", "MS1_ebik0_eq0", "MS1_ebik0_eq1", "MS1_ebik1_eq0", 
                     "MS1_ebik1_eq1")

# paths of data versions

firstDataDir <- c(paste0(mainPath, '/data/csv/'))
secondDataDir <- c(paste0(mainPath, '/prevdata/csv/'))

# tolerance

tolerance <- 0.01

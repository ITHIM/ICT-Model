rm(list=ls())

source('directProbs.R')

# create same pop as in xlsx file

testBaseline <- data.frame(ID = sample(1:100, size = 100, replace = FALSE), agesex = NA, Cycled = 0, cyclist = 0, stringsAsFactors = FALSE)

testBaseline[1:50, ]$agesex <- '16.59Male'
testBaseline[51:90, ]$agesex <- '16.59Female'
testBaseline[91:94, ]$agesex <- '60plusMale'
testBaseline[95:100, ]$agesex <- '60plusFemale'

# create Cyclist

cyclistIDs <- c()

cyclistIDs <- append(cyclistIDs, sample(testBaseline[testBaseline$agesex == '16.59Male', ]$ID, 7, replace = FALSE))
cyclistIDs <- append(cyclistIDs, sample(testBaseline[testBaseline$agesex == '16.59Female', ]$ID, 1, replace = FALSE))
cyclistIDs <- append(cyclistIDs, sample(testBaseline[testBaseline$agesex == '60plusMale', ]$ID, 1, replace = FALSE))
cyclistIDs <- append(cyclistIDs, sample(testBaseline[testBaseline$agesex == '60plusFemale', ]$ID, 1, replace = FALSE))

testBaseline[testBaseline$ID %in% cyclistIDs, ]$Cycled <- 1

print(addmargins(table(testBaseline$agesex, testBaseline$Cycled)),1)

IDOfPplCyclist = directProbRRPPLIDs(testBaseline, 0.70, 0, 0, testBaseline)

testBaselineAfter <- testBaseline
testBaselineAfter[testBaselineAfter$ID %in% IDOfPplCyclist, ]$cyclist <- 1

print(addmargins(table(testBaselineAfter$agesex, testBaselineAfter$cyclist)),1)

# because IDs of ppl who are cyclist already are not returned via directProbRRPPLIDs, thus it should be added manually

cyclistsAfter <- append(IDOfPplCyclist, unique(testBaseline[testBaseline$Cycled == 1, ]$ID))

length(cyclistsAfter) == 70
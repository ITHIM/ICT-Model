# Initialize the state of the project

# Check if listOfScenarios exists. If not, then read it from a csv file
if (!exists("listOfScenarios") || length(listOfScenarios) != 24){
  listOfScenarios <- read.csv("listofScenarios.csv", header = F, as.is = T)
  listOfScenarios <- as.list(listOfScenarios$V1)
}

# No need to read people with no trips
#people_with_no_trips <- read.csv("People_w_NoTrips2012_ENG_v6_anon.csv", header = T, as.is = T)

mmet2RR_mat <- read.csv("healthcalculations/Wen et al all cause mortality RRs.csv", header = T, as.is = T)


bikechoice <-function(dist) { 
     #calculates prob of switch to cycling depending on: [age-sex-trip distance]
     
     #intervals for distance binning
     distIntervals <-c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,9.5,12.5,15.5,20.5,30.5,40.5,10000)

     
     #get interval to use as nrow
     nrow <- findInterval(dist,distIntervals)+1  # starts @ 0, add 1
     probstrip <-c(0.550,0.680,0.751,0.815,0.889,0.889,0.905,0.929,0.947,0.919,0.919,1.000,1.000,0.000)

     result <- probstrip[nrow]
     x <- runif(1,0,1)
     
     bikechoice <- ifelse(x<result,1,0)
}

pcyc21 <-function(age,sex,dist,ebikes,equity,MS) {

  #intervals for distance binning
  distIntervals <-c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,9.5,12.5,15.5,20.5,30.5,40.5,10000)
  #get interval to use as nrow
  nrow <- findInterval(dist,distIntervals)+1  # starts @ 0, add 1
  prob <- 0
  if (ebikes == 0) {
    ncol <- paste(sex,age,sep="_")
    prob <- pcycl_baseline[nrow,ncol]   
  } #end ebikes=0
  else {    #ebikes=1
    ncol <- 'ebike'   #same w or w/o  equity
    prob <- pcycl_baseline[nrow,ncol] 
  }
  
  pcyc <- prob
  pcyc
}
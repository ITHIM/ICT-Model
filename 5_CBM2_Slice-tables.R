
#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios") #source folder
#setwd("V:/Studies/MOVED/HealthImpact/CBM2/Code/Scenarios2012") #source folder
rm(list=ls())


#aggr <-read.csv('CBMII_aggr2_30K.csv',header=T,as.is=T)
aggr <-read.csv('CBM_aggr_II.csv',header=T,as.is=T)

bd_mode <- read.csv('BD_mode.csv',header=T,as.is=T)

bd_mode.male <-read.csv('BD_mode-male.csv',header=T,as.is=T)
bd_mode.female <-read.csv('BD_mode-female.csv',header=T,as.is=T)

bd_mode.white <-read.csv('BD_mode-white.csv',header=T,as.is=T)
bd_mode.nonwhite <-read.csv('BD_mode-nonwhite.csv',header=T,as.is=T)

bd_mode.caraccess <-read.csv('BD_mode.caraccess.csv',header=T,as.is=T)
bd_mode.noncaraccess <-read.csv('BD_mode.noncaraccess.csv',header=T,as.is=T)

bd_share <- read.csv('BD_share.csv',header=T,as.is=T)
bd_timehist <- read.csv('BD_timehist.csv',,header=T,as.is=T)

todos <-c('cyclist.eq0','cyclist.eq1','cyclist.eb1','cyclist.eb1eq1',
          'CO2R.perc.eq0','CO2R.perc.eq1','CO2R.perc.eb1','CO2R.perc.eb1eq1',
          'nocarTrips.people.eq0','nocarTrips.people.eq1','nocarTrips.people.eb1','nocarTrips.people.eb1eq1',
          'milesCycled.eq0','milesCycled.eq1','milesCycled.eb1','milesCycled.eb1eq1',
     
          'modeshare.eq0.TDR1','modeshare.eq1.TDR1','modeshare.eb1.TDR1','modeshare.eb1eq1.TDR1',
          'modeshare.eq0.TDR.7','modeshare.eq1.TDR.7','modeshare.eb1.TDR.7','modeshare.eb1eq1.TDR.7',
          
          'whichmode.TDR1.eq0','whichmode.TDR1.eq1','whichmode.TDR1.eb1','whichmode.TDR1.eb1eq1',
          
          'timeSavedCyclists.TDR1.perc',
          'timehist.eq0','timehist.eq1','timehist.eb1','timehist.eb1eq1',
          
          
          'cyclist.males','cyclist.females',
          'milesCycled.male','milesCycled.female',
          
          'whichmode.TDR1.males','whichmode.TDR1.females',
          
          
          'cyclist.white.perc','cyclist.nonwhite.perc',
          'milesCycled.white','milesCycled.nonwhite',
          'people.nocar.white','people.nocar.nonwhite',
          'whichmode.TDR1.white','whichmode.TDR1.nonwhite',
          
          
          'cyclist.caraccess.perc','cyclist.noncaraccess.perc',
          'milesCycled.caraccess','milesCycled.noncaraccess',
          'nocar.caraccess','nocar.noncaraccess',
          'whichmode.TDR1.caraccess','whichmode.TDR1.noncaraccess',
          
          
          'cyclshare.nssec.eq0','cyclshare.nssec.eq1','cyclshare.nssec.eb1','cyclshare.nssec.eb1eq1',
          
          'trips.age.eq0','trips.age.eq1','trips.age.eb1','trips.age.eb1eq1'  )


######## HERE STARTs THE SLICING  !!


#SCENARIOS breakdown
cyclist.eq0 <-  subset(aggr,equity==0 & ebike==0,select=c(MS,TDR,cyclists.perc) )
cyclist.eq1 <-  subset( aggr,equity==1 & ebike==0,select=c(MS,TDR,cyclists.perc) )
cyclist.eb1 <-  subset( aggr,equity==0 & ebike==1,select=c(MS,TDR,cyclists.perc) )
cyclist.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,TDR,cyclists.perc) )

# %CO2 reduction
CO2R.perc.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,TDR,CO2R.perc) )
CO2R.perc.eq1 <- subset(aggr,equity==1 & ebike==0,select=c(MS,TDR,CO2R.perc) )
CO2R.perc.eb1 <- subset(aggr,equity==0 & ebike==1,select=c(MS,TDR,CO2R.perc) )
CO2R.perc.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,TDR,CO2R.perc) )

#no car trips
nocarTrips.people.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,TDR,nocarTrips.people.perc) )
nocarTrips.people.eq1 <- subset(aggr,equity==1 & ebike==0,select=c(MS,TDR,nocarTrips.people.perc) )
nocarTrips.people.eb1 <- subset(aggr,equity==0 & ebike==1,select=c(MS,TDR,nocarTrips.people.perc) )
nocarTrips.people.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,TDR,nocarTrips.people.perc) )

# bicycle miles
milesCycled.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,TDR,milesCycled.pers) )
milesCycled.eq1 <- subset(aggr,equity==1 & ebike==0,select=c(MS,TDR,milesCycled.pers) )
milesCycled.eb1 <- subset(aggr,equity==0 & ebike==1,select=c(MS,TDR,milesCycled.pers) )
milesCycled.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,TDR,milesCycled.pers) )

#mode share
 modeshare.eq0.TDR1 <- subset(bd_share,TDR==1 & equity==0 & ebike==0 )
 modeshare.eq1.TDR1 <- subset(bd_share,TDR==1 & equity==1 & ebike==0 )
 modeshare.eb1.TDR1 <- subset(bd_share,TDR==1 & equity==0 & ebike==1 )
 modeshare.eb1eq1.TDR1 <- subset(bd_share,TDR==1 & equity==1 & ebike==1 )
 
 modeshare.eq0.TDR.7  <- subset(bd_share,TDR==0.7 & equity==0 & ebike==0 )
 modeshare.eq1.TDR.7 <- subset(bd_share,TDR==0.7 & equity==1 & ebike==0 )
 modeshare.eb1.TDR.7 <- subset(bd_share,TDR==0.7 & equity==0 & ebike==1 )
 modeshare.eb1eq1.TDR.7 <- subset(bd_share,TDR==0.7 & equity==1 & ebike==1 )

#which mode trips are coming from by scenario
whichmode.TDR1.eq0 <- subset(bd_mode,TDR==1 & equity==0 & ebike==0 )
whichmode.TDR1.eq1 <-subset(bd_mode,TDR==1 & equity==1 & ebike==0 )
whichmode.TDR1.eb1 <-subset(bd_mode,TDR==1 & equity==0 & ebike==1 )
whichmode.TDR1.eb1eq1 <- subset(bd_mode,TDR==1 & equity==1 & ebike==1 )
# 
# # time savings   %, TDR=1 
timeSavedCyclists.TDR1.perc <- subset(aggr,TDR=1, select=c(MS,equity,ebike,timeSavedCyclists.perc) )

# # time histogram   %, TDR=1
timehist.eq0 <- subset(bd_timehist,TDR=1 & equity==0 & ebike==0 ,select=c(MS,cases) )
timehist.eq1 <- subset(bd_timehist,TDR=1 & equity==1 & ebike==0 ,select=c(MS,cases) )
timehist.eb1 <- subset(bd_timehist,TDR=1 & equity==0 & ebike==1 ,select=c(MS,cases) )
timehist.eb1eq1 <- subset(bd_timehist,TDR=1 & equity==1 & ebike==1 ,select=c(MS,cases) )


################SEX
cyclist.males  <- subset(aggr,TDR==1, select=c(MS,equity,ebike,cyclist.male.perc) )
cyclist.females <- subset(aggr,TDR==1, select=c(MS,equity,ebike,cyclist.female.perc) )

people.nocar.male <- subset(aggr,TDR==1, select=c(MS,equity,ebike,nocar.males.perc) )
people.nocar.female <- subset(aggr,TDR==1, select=c(MS,equity,ebike,nocar.females.perc) )

milesCycled.male <- subset(aggr,TDR==1, select=c(MS,equity,ebike,milesCycled.male) )
milesCycled.female <- subset(aggr,TDR==1, select=c(MS,equity,ebike,milesCycled.female) ) 

##which mode
whichmode.TDR1.males <- subset(bd_mode.male,TDR==1, select=c(MS,equity,ebike,modefinal,cases) )
whichmode.TDR1.females <- subset(bd_mode.female,TDR==1, select=c(MS,equity,ebike,modefinal,cases) )

##########

################ETHNICITY

cyclist.white.perc <- subset(aggr,TDR==1, select=c(MS,equity,ebike,cyclist.white.perc) )
cyclist.nonwhite.perc <- subset(aggr,TDR==1, select=c(MS,equity,ebike,cyclist.nonwhite.perc) )
    
milesCycled.white <- subset(aggr,TDR==1, select=c(MS,equity,ebike,milesCycled.white) )
milesCycled.nonwhite <- subset(aggr,TDR==1, select=c(MS,equity,ebike,milesCycled.nonwhite) )

# % people with no car trips
people.nocar.white <- subset(aggr,TDR==1, select=c(MS,equity,ebike,nocar.white.perc) )
people.nocar.nonwhite <- subset(aggr,TDR==1, select=c(MS,equity,ebike,nocar.nonwhite.perc) )

#which mode     
whichmode.TDR1.white  <- subset(bd_mode.white,TDR==1, select=c(MS,equity,ebike,modefinal,cases) )
whichmode.TDR1.nonwhite  <- subset(bd_mode.nonwhite,TDR==1, select=c(MS,equity,ebike,modefinal,cases) )


################CAR ACCESS
cyclist.caraccess.perc <- subset(aggr,TDR==1, select=c(MS,equity,ebike,cyclist.caraccess.perc) )
cyclist.noncaraccess.perc  <- subset(aggr,TDR==1, select=c(MS,equity,ebike,cyclist.noncaraccess.perc) )

milesCycled.caraccess <- subset(aggr,TDR==1, select=c(MS,equity,ebike,milesCycled.caraccess) )
milesCycled.noncaraccess  <- subset(aggr,TDR==1, select=c(MS,equity,ebike,milesCycled.caraccess) )

nocar.caraccess <-  subset(aggr,TDR==1, select=c(MS,equity,ebike,nocar.caraccess) )
nocar.noncaraccess <-  subset(aggr,TDR==1, select=c(MS,equity,ebike,nocar.noncaraccess) )


#which mode     
whichmode.TDR1.caraccess  <- subset(bd_mode.caraccess,TDR==1, select=c(MS,equity,ebike,modefinal,cases) )
whichmode.TDR1.noncaraccess  <- subset(bd_mode.noncaraccess,TDR==1, select=c(MS,equity,ebike,modefinal,cases) )


###################NS-SEC
cyclshare.nssec.eq0 <- subset(aggr,TDR==1 & equity==0 & ebike==0, select=c(MS,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                    cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )
cyclshare.nssec.eq1  <- subset(aggr,TDR==1 & equity==1 & ebike==0, select=c(MS,equity,ebike,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                                cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )
cyclshare.nssec.eb1  <- subset(aggr,TDR==1 &  equity==0 & ebike==1, select=c(MS,equity,ebike,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                                cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )
cyclshare.nssec.eb1eq1  <- subset(aggr,TDR==1 &  equity==1 & ebike==1, select=c(MS,equity,ebike,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                                   cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )

###### AGE
trips.age.eq0 <- subset(aggr,TDR==1 & equity==0 & ebike==0, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )
trips.age.eq1  <- subset(aggr,TDR==1 & equity==1 & ebike==0, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )
trips.age.eb1  <- subset(aggr,TDR==1 &  equity==0 & ebike==1, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )
trips.age.eb1eq1  <- subset(aggr,TDR==1 &  equity==1 & ebike==1, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )


setwd('./tables')
lst <- mget(todos)
lapply(seq_along(lst), function(i) write.csv(lst[[i]], file=paste(todos[i], '.csv', sep='', row.names=F)))






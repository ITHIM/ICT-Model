
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
          
          'whichmode.eq0','whichmode.eq1','whichmode.eb1','whichmode.eb1eq1',
          
          'timeSavedCyclists.perc',
          'timehist.eq0','timehist.eq1','timehist.eb1','timehist.eb1eq1',
          
          
          'cyclist.males','cyclist.females',
          'milesCycled.male','milesCycled.female',
          
          'whichmode.males','whichmode.females',
          
          
          'cyclist.white.perc','cyclist.nonwhite.perc',
          'milesCycled.white','milesCycled.nonwhite',
          'people.nocar.white','people.nocar.nonwhite',
          'whichmode.white','whichmode.nonwhite',
          
          
          'cyclist.caraccess.perc','cyclist.noncaraccess.perc',
          'milesCycled.caraccess','milesCycled.noncaraccess',
          'nocar.caraccess','nocar.noncaraccess',
          'whichmode.caraccess','whichmode.noncaraccess',
          
          
          'cyclshare.nssec.eq0','cyclshare.nssec.eq1','cyclshare.nssec.eb1','cyclshare.nssec.eb1eq1',
          
          'trips.age.eq0','trips.age.eq1','trips.age.eb1','trips.age.eb1eq1'  )


######## HERE STARTs THE SLICING  !!


#SCENARIOS breakdown
cyclist.eq0 <-  subset(aggr,equity==0 & ebike==0,select=c(MS,cyclists.perc) )
cyclist.eq1 <-  subset( aggr,equity==1 & ebike==0,select=c(MS,cyclists.perc) )
cyclist.eb1 <-  subset( aggr,equity==0 & ebike==1,select=c(MS,cyclists.perc) )
cyclist.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,cyclists.perc) )

# %CO2 reduction
CO2R.perc.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,CO2R.perc) )
CO2R.perc.eq1 <- subset(aggr,equity==1 & ebike==0,select=c(MS,CO2R.perc) )
CO2R.perc.eb1 <- subset(aggr,equity==0 & ebike==1,select=c(MS,CO2R.perc) )
CO2R.perc.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,CO2R.perc) )

#no car trips
nocarTrips.people.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,nocarTrips.people.perc) )
nocarTrips.people.eq1 <- subset(aggr,equity==1 & ebike==0,select=c(MS,nocarTrips.people.perc) )
nocarTrips.people.eb1 <- subset(aggr,equity==0 & ebike==1,select=c(MS,nocarTrips.people.perc) )
nocarTrips.people.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,nocarTrips.people.perc) )

# bicycle miles
milesCycled.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,milesCycled.pers) )
milesCycled.eq1 <- subset(aggr,equity==1 & ebike==0,select=c(MS,milesCycled.pers) )
milesCycled.eb1 <- subset(aggr,equity==0 & ebike==1,select=c(MS,milesCycled.pers) )
milesCycled.eb1eq1 <- subset(aggr,equity==1 & ebike==1,select=c(MS,milesCycled.pers) )

#mode share
 modeshare.eq0 <- subset(bd_share,equity==0 & ebike==0 )
 modeshare.eq1 <- subset(bd_share,equity==1 & ebike==0 )
 modeshare.eb1 <- subset(bd_share,equity==0 & ebike==1 )
 modeshare.eb1eq1 <- subset(bd_share,equity==1 & ebike==1 )

#which mode trips are coming from by scenario
whichmode.eq0 <- subset(bd_mode,equity==0 & ebike==0 )
whichmode.eq1 <-subset(bd_mode,equity==1 & ebike==0 )
whichmode.eb1 <-subset(bd_mode,equity==0 & ebike==1 )
whichmode.eb1eq1 <- subset(bd_mode,equity==1 & ebike==1 )
# 
# # time savings   %
timeSavedCyclists.perc <- subset(aggr,select=c(MS,equity,ebike,timeSavedCyclists.perc) )

# # time histogram   %
timehist.eq0 <- subset(bd_timehist,equity==0 & ebike==0 ,select=c(MS,cases) )
timehist.eq1 <- subset(bd_timehist,equity==1 & ebike==0 ,select=c(MS,cases) )
timehist.eb1 <- subset(bd_timehist,equity==0 & ebike==1 ,select=c(MS,cases) )
timehist.eb1eq1 <- subset(bd_timehist,equity==1 & ebike==1 ,select=c(MS,cases) )


################SEX
cyclist.males  <- subset(aggr,select=c(MS,equity,ebike,cyclist.male.perc) )
cyclist.females <- subset(aggr,select=c(MS,equity,ebike,cyclist.female.perc) )

people.nocar.male <- subset(aggr, select=c(MS,equity,ebike,nocar.males.perc) )
people.nocar.female <- subset(aggr, select=c(MS,equity,ebike,nocar.females.perc) )

milesCycled.male <- subset(aggr, select=c(MS,equity,ebike,milesCycled.male) )
milesCycled.female <- subset(aggr, select=c(MS,equity,ebike,milesCycled.female) ) 

##which mode
whichmode.males <- subset(bd_mode.male, select=c(MS,equity,ebike,modefinal,cases) )
whichmode.females <- subset(bd_mode.female, select=c(MS,equity,ebike,modefinal,cases) )

##########

################ETHNICITY

cyclist.white.perc <- subset(aggr, select=c(MS,equity,ebike,cyclist.white.perc) )
cyclist.nonwhite.perc <- subset(aggr, select=c(MS,equity,ebike,cyclist.nonwhite.perc) )
    
milesCycled.white <- subset(aggr, select=c(MS,equity,ebike,milesCycled.white) )
milesCycled.nonwhite <- subset(aggr, select=c(MS,equity,ebike,milesCycled.nonwhite) )

# % people with no car trips
people.nocar.white <- subset(aggr, select=c(MS,equity,ebike,nocar.white.perc) )
people.nocar.nonwhite <- subset(aggr, select=c(MS,equity,ebike,nocar.nonwhite.perc) )

#which mode     
whichmode.white  <- subset(bd_mode.white, select=c(MS,equity,ebike,modefinal,cases) )
whichmode.nonwhite  <- subset(bd_mode.nonwhite, select=c(MS,equity,ebike,modefinal,cases) )


################CAR ACCESS
cyclist.caraccess.perc <- subset(aggr, select=c(MS,equity,ebike,cyclist.caraccess.perc) )
cyclist.noncaraccess.perc  <- subset(aggr, select=c(MS,equity,ebike,cyclist.noncaraccess.perc) )

milesCycled.caraccess <- subset(aggr, select=c(MS,equity,ebike,milesCycled.caraccess) )
milesCycled.noncaraccess  <- subset(aggr, select=c(MS,equity,ebike,milesCycled.caraccess) )

nocar.caraccess <-  subset(aggr, select=c(MS,equity,ebike,nocar.caraccess) )
nocar.noncaraccess <-  subset(aggr, select=c(MS,equity,ebike,nocar.noncaraccess) )


#which mode     
whichmode.caraccess  <- subset(bd_mode.caraccess, select=c(MS,equity,ebike,modefinal,cases) )
whichmode.noncaraccess  <- subset(bd_mode.noncaraccess, select=c(MS,equity,ebike,modefinal,cases) )


###################NS-SEC
cyclshare.nssec.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                    cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )
cyclshare.nssec.eq1  <- subset(aggr,equity==1 & ebike==0, select=c(MS,equity,ebike,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                                cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )
cyclshare.nssec.eb1  <- subset(aggr, equity==0 & ebike==1, select=c(MS,equity,ebike,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                                cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )
cyclshare.nssec.eb1eq1  <- subset(aggr, equity==1 & ebike==1, select=c(MS,equity,ebike,cyclist.nssec1.perc,cyclist.nssec2.perc,
                                                   cyclist.nssec3.perc,cyclist.nssec4.perc,cyclist.nssec5.perc) )

###### AGE
trips.age.eq0 <- subset(aggr,equity==0 & ebike==0, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )
trips.age.eq1  <- subset(aggr,equity==1 & ebike==0, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )
trips.age.eb1  <- subset(aggr, equity==0 & ebike==1, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )
trips.age.eb1eq1  <- subset(aggr, equity==1 & ebike==1, select=c(MS,equity, ebike,trips.age20.39.perc,trips.age40.59.perc,trips.age60plus.perc) )


setwd('./tables')
lst <- mget(todos)
lapply(seq_along(lst), function(i) write.csv(lst[[i]], file=paste(todos[i], '.csv', sep='', row.names=F)))






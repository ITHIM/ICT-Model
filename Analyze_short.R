Analyze_short <- function (fname,iter)   {

#get the basics
     if (fname=='baseline.csv') 
     {MS<-1
      equity<-0
      ebike <-0}     #baseline special case
     
     else {
          params <-str_extract_all(fname, "([0-9]+(?:\\.[0-9]+)?)")
          params <-as.numeric(params[[1]] )
          MS <- as.integer(params[1])+1
          ebike <- as.integer(params[3])
          equity <- as.integer(params[4])
               }   #rest of files      
     
f <-read.csv(file=fname) 
f <-left_join(f,lookup)        #recateg MainMode_B04ID + get all modefinal
#f <-inner_join(f,modes)

# #1-MODE ORIGIN (where cycling trips come from) 
# 
# if (fname=='baseline.csv') {
#      #f.all <-left_join(modefinal,f[f$Cycled==1,])
#      #f.all <-left_join(modes,f[f$Cycled==1,])
#     f.all <-f[f$Cycled==1,]
#     f.gr <- group_by(.data=f.all, modefinal)
#      
#      f.gr$Cycled[is.na(f.gr$MainMode_B04ID)] <- 0
#      #f.gr$Cycled[is.na(f.gr$modefinal)] <- 0
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(Cycled))     }    
# 
# else {
#      #2:f.all <-left_join(modefinal,f[f$now_cycle==1,])
#      #f.all <-left_join(modes,f[f$now_cycle==1,])
#      f.all <-f[f$now_cycle==1,]
#      f.gr <- group_by(.data=f.all, modefinal)
#       
#       #f.gr$Cycled[is.na(f.gr$MainMode_B04ID)] <- 0
#       #f.gr$Cycled[is.na(f.gr$modefinal)] <- 0
#       f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#       
#       result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode <<-result, BD_mode <<- rbind(BD_mode,result) )
# 
# #1 TIME SAVINGS
# if(fname=='baseline.csv') {
#      f$TripTotalTime1 <-f$TripTotalTime
#      f <-cbind(f,f$TripTotalTime1)
#      f.all <-f[f$Cycled==1,]   
#                           }
# 
# else {    f.all <-f[f$now_cycle==1,]   
#      }
# 
# ratio <-f.all$TripTotalTime1 / f.all$TripTotalTime
# timeband <- findInterval(ratio,intervals)
# f.all <-cbind(f.all,ratio,timeband)
# 
# #create subsets to analyse by GENDER | CAR ACCESS | FASTER-SLOWER TRIPS
# f.male <-f.all[f.all$Sex=='Male',]
# f.female <-f.all[f.all$Sex=='Female',]
# 
# #Faster/slower subsets, to compare faster/slower mode origins 
# if (fname=='baseline.csv') {f.faster=f.slower <-f.all[f.all$ratio<=1,]}
# else {
# f.faster <-f.all[f.all$ratio<=1,]
# f.slower <-f.all[f.all$ratio >1,]
#      }
# 
# # CAR ACCESS, to analyse time savings  
# f.caraccess <- f.all[f.all$CarAccess_B01ID %in% c(1,2,3,4),]
# f.noncaraccess <- f.all[f.all$CarAccess_B01ID %in% c(5,6),]
# 
# #time histogram, GLOBAL no break down
# f.gr <-group_by(f.all,timeband)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_timehist <<-result, BD_timehist <<- rbind(BD_timehist,result) )
# 
# #1.1 TIME HISTOGRAM BY SEX
# f.gr <-group_by(f.male,timeband)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_timehist.male <<-result, BD_timehist.male <<- rbind(BD_timehist.male,result) )
# 
# f.gr <-group_by(f.female,timeband)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_timehist.female <<-result, BD_timehist.female <<- rbind(BD_timehist.female,result) )
# 
# #1.2 TIME HISTOGRAM BY CAR ACCESS/NON C.A
# f.gr <-group_by(f.caraccess,timeband)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_timehist.caraccess <<-result, BD_timehist.caraccess <<- rbind(BD_timehist.caraccess,result) )
# 
# f.gr <-group_by(f.noncaraccess,timeband)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_timehist.noncaraccess <<-result, BD_timehist.noncaraccess <<- rbind(BD_timehist.noncaraccess,result) )
# 
# 
# #1.3 MODE ORIGIN FASTER/SLOWER TRIPS
# f.gr <-group_by(.data=f.faster,modefinal)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.fastertrips <<-result, BD_mode.fastertrips <<- rbind(BD_mode.fastertrips,result) )
# 
# f.gr <-group_by(.data=f.slower,modefinal)
# result <- summarise(f.gr,cases=n())
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.slowertrips <<-result, BD_mode.slowertrips <<- rbind(BD_mode.slowertrips,result) )
# 
# 
# #2-MODE by SEX
# #MALES
# if (fname=='baseline.csv') {
#      #2f.all <-left_join(modefinal,f[f$Cycled==1 & f$Sex=='Male',])
#     f.all <-f[f$Cycled==1 & f$Sex=='Male',]
#     
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(Cycled))     }    
# 
# else {
#      #f.all <-left_join(modefinal,f[f$now_cycle==1 & f$Sex=='Male',])
#      f.all <-f[f$now_cycle==1 & f$Sex=='Male',]
#     
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.male <<-result, BD_mode.male <<- rbind(BD_mode.male,result) )
# 
# #FEMALES
# if (fname=='baseline.csv') {
#      #2f.all <-left_join(modefinal,f[f$Cycled==1 & f$Sex=='Female',])
#      f.all <-f[f$Cycled==1 & f$Sex=='Female',]
#      
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      result <- summarise(f.gr,cases=sum(Cycled))     }    
# 
# else {
#      #2f.all <-left_join(modefinal,f[f$now_cycle==1 & f$Sex=='Female',])
#   f.all <- f[f$now_cycle==1 & f$Sex=='Female',]
#      
#   f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#      result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.female <<-result, BD_mode.female <<- rbind(BD_mode.female,result) )
# 
# 
# #  3 -ETHNICITY
# #WHITE
# if (fname=='baseline.csv') {
# #      #f.all <-left_join(modefinal,f[f$Cycled==1 & f$EthGroupTS_B02ID==1,])
# #      f.all <- f[f$Cycled==1 & f$EthGroupTS_B02ID==1,] 
# #   
# #      f.gr <- group_by(.data=f.all, modefinal)
# #      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
# #      result <- summarise(f.gr,cases=sum(Cycled))    
#      
#      f.all <-left_join(modefinal,f[f$Cycled==1 & f$EthGroupTS_B02ID==2,])
#      
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(Cycled))
#                                    }    
# 
# else {
#      #f.all <-left_join(modefinal,f[f$now_cycle==1 & f$EthGroupTS_B02ID==1,])
#      f.all <- f[f$now_cycle==1 & f$EthGroupTS_B02ID==1,]   
#   
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)   #
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.white <<-result, BD_mode.white <<- rbind(BD_mode.white,result) )
# # 
# #NON-WHITE
# if (fname=='baseline.csv') {
#      f.all <-left_join(lookup,f[f$Cycled==1 & f$EthGroupTS_B02ID==2,])
#   #f.all <- f[f$Cycled==1 & f$EthGroupTS_B02ID==2,]
#   
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(Cycled))     }    
# 
# else {
#     #f.all <-left_join(modefinal,f[f$now_cycle==1 & f$EthGroupTS_B02ID==2,])
#   f.all <- f[f$now_cycle==1 & f$EthGroupTS_B02ID==2,]  
#   
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.nonwhite <<-result, BD_mode.nonwhite <<- rbind(BD_mode.nonwhite,result) )
# 
# # 4. CAR ACCESS
# if (fname=='baseline.csv') {
#     # f.all <-left_join(modefinal,f[f$Cycled==1 & f$CarAccess_B01ID %in% c(1,2,3,4),])
#   f.all <- f[f$Cycled==1 & f$CarAccess_B01ID %in% c(1,2,3,4),]
#      
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(Cycled))     }    
# 
# else {
#      #f.all <-left_join(modefinal,f[f$now_cycle==1 & f$CarAccess_B01ID %in% c(1,2,3,4),])
#      f.all <- f[f$now_cycle==1 & f$CarAccess_B01ID %in% c(1,2,3,4),]
#   
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.caraccess <<-result, BD_mode.caraccess <<- rbind(BD_mode.caraccess,result) )
# 
# #NON CAR ACCESS
# if (fname=='baseline.csv') {
#      #f.all <-left_join(modefinal,f[f$Cycled==1 & f$CarAccess_B01ID %in% c(5,6),])
#   f.all <- f[f$Cycled==1 & f$CarAccess_B01ID %in% c(5,6),]
#      
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$Cycled[is.na(f.gr$Cycled)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(Cycled))     }    
# 
# else {
#      #f.all <-left_join(modefinal,f[f$now_cycle==1 & f$CarAccess_B01ID %in% c(5,6),])
#   f.all <- f[f$now_cycle==1 & f$CarAccess_B01ID %in% c(5,6),]
#      
#      f.gr <- group_by(.data=f.all, modefinal)
#      f.gr$now_cycle[is.na(f.gr$now_cycle)] <- 0
#      
#      result <- summarise(f.gr,cases=sum(now_cycle))  }
# 
# total <- sum(result$cases)
# result$cases <-round(100*result$cases/total,2)
# result <- cbind(fname,MS,TDR,equity,ebike,result)
# ifelse(iter==1,BD_mode.noncaraccess <<-result, BD_mode.noncaraccess <<- rbind(BD_mode.noncaraccess,result) )

#5-MODE SHARE
#f$modefinal[f$now_cycle==1] <- 2   #all bike types confounded
f$modefinal[(f$now_cycle==1 & f$ebike==0) | (f$Cycled==1)] <- 20   #pedal bikes
f$modefinal[f$now_cycle==1 & f$ebike==1] <- 21   #ebikes 
#f.all <- left_join(lookup,f)


#SUBSETS for car access mode share
f.all.caraccess <- f[f$CarAccess_B01ID %in% c(1,2,3,4),]
f.all.noncaraccess <- f[f$CarAccess_B01ID %in% c(5,6),]

#global mode share
#f.gr <-group_by(.data=f.all,MainMode_B04ID)
f.gr <- group_by(.data=f, modefinal)
result <-summarise(f.gr,cases=n())   #check w. alternative formula
total <- sum(result$cases)   

result$cases <-round(100*result$cases/total,1)
result <- cbind(fname,MS,equity,ebike,result)
ifelse(iter==1, BD_share <<-result, BD_share <<-rbind(BD_share,result) )

#5.1 MODE SHARE by CAR-ACCESS
#WITH car access
f.gr <- group_by(.data=f.all.caraccess, modefinal)
result <-summarise(f.gr,cases=n())
total <- sum(result$cases)

result$cases <-round(100*result$cases/total,1)
result <- cbind(fname,MS,equity,ebike,result)
ifelse(iter==1, BD_share.caraccess <<-result, BD_share.caraccess <<-rbind(BD_share.caraccess,result) )

#NO car access
f.gr <- group_by(.data=f.all.noncaraccess, modefinal)
result <-summarise(f.gr,cases=n())
total <- sum(result$cases)

result$cases <-round(100*result$cases/total,1)
result <- cbind(fname,MS,equity,ebike,result)
ifelse(iter==1, BD_share.noncaraccess <<-result, BD_share.noncaraccess <<-rbind(BD_share.noncaraccess,result) )


}
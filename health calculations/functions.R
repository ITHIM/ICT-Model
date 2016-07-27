as.numeric.factor <-
  function(x) {as.numeric(levels(x))[x]}
combine_health_and_pif <-
  function(pop, hc, hm){
    m <- as.matrix(pop)
    #cn <- grep('mmet$',colnames(m), value = TRUE)
    cn <- append("baseline_mmet", grep('MS',names(pop), value = TRUE))
    for (i in 1: length(cn)){
      for (j in 1:nrow(m)){
        
        sub <- subset(as.data.frame(hc), gender == m[j, 2] & age == m[j, 1])
        if (length(sub) > 0){
          #cat(m[j, 2], " : ",m[j, 1], " : ", i, " : ", j, cn[i], "\n")
          if (length(sub[[hm]]) > 0){
            m[j, cn[i]] <- as.numeric(as.character(m[j, cn[i]])) * as.numeric(as.character(sub[[hm]]))
            #cat(as.numeric(as.character(m[j, cn[i]])), " : ",as.numeric(as.character(sub[[hm]])) , "\n")
          }else{
            m[j, cn[i]] <- 0
          }
        }else{
          m[j, cn[i]] <- 0
        }
      }
    }
    m
  }


combine_scenarios <-
  function(baseline){
    #baseline <- b
    library(sqldf)
    f <- list.files(path="./scenarios/trips",pattern=".*\\.csv") 
    for (i in 1:length(f)){
      f1 <- read.csv(paste(getwd(), "/scenarios/trips/", f[i], sep=""), header = T)
      f1 <- sqldf("Select Age_B01ID, Sex_B01ID, IndividualID, sum(MMETh + mMETs) as mmet from f1 Group by IndividualID")
      baseline[[paste(f[i], "mmet", sep = "_")]] <- f1$mmet
    }
    baseline
  }
getaguniform <-
  function(m){}
getstratameasure <-
  function(gbd){
    library(stringr)
    ag <- unique(gbd$age_name)
    gender <- unique(gbd$sex)
    #cat(ag, " , ", gender, "\n")
    m <- matrix(nrow = length(ag) * length(gender), ncol = 6)
    colnames(m) <- c("ag_group", "gender", "death", "daly", "yld", "yll")
    index <- 0
    for (i in 1:length(ag)){
      #m [index, 1] <- ag[i]
      for (j in 1:length(gender)){
        m [index, 1] <- str_trim(ag[i])
        m[index, 2] <- str_trim(gender[j])
        m[index, 3] <- subset(gbd, age_name == ag[i] & sex == gender[j] & measure == "death")$nm_mean
        m[index, 4] <- subset(gbd, age_name == ag[i] & sex == gender[j] & measure == "daly")$nm_mean
        m[index, 5] <- subset(gbd, age_name == ag[i] & sex == gender[j] & measure == "yld")$nm_mean
        m[index, 6] <- subset(gbd, age_name == ag[i] & sex == gender[j] & measure == "yll")$nm_mean                  
        index <- index + 1            
      }
      
      
    }
    m
  }
mean_cycle <-
  function(){
    #baseline <- b
    library(sqldf)
    f <- list.files(path="./scenarios/trips",pattern=".*\\.csv") 
    #m <- data.frame(a = character, b = numeric())
    m <- matrix(nrow=length(f), ncol=2)
    
    #colnames(m) <- c("scenario", "cyc_avg)")
    index <- 1
    for (i in 1:length(f)){
      f1 <- read.csv(paste(getwd(), "/scenarios/trips/", f[i], sep=""), header = T)
      val <- (sum(f1$Cycled) + sum(f1$now_cycle)) / nrow(f1)
      #val <- sqldf("Select sum(Cycled + now_cycle) as avg from f1")
      #cat(val, "\n")
      m[i, 1] <- f[i]
      m[i, 2] <- val#sqldf("Select sum(Cycled + now_cycle) as avg from f1")        
      #index <- index + 1
    }
    m
  }
mmet2RR <-
  function(m, mmet2RR, cn){
    #cn <- grep('\\mmet$',names(m), value = TRUE)
    for (i in 1:length(cn)){
      cnrr <- paste(cn[i], "rr", sep="")
      for (j in 1:nrow(m)){
        val <- m[j, cn[i]]
        #cat(i, j, cn[i], " : ", val, " : ", mmet2RR[round(val) + 1, 2], "\n")
        if (val <= 2000){
          m[j, cn[i]] <- mmet2RR[round(val) + 1, 2]
        }else {
          m[j, cn[i]] <- 0.5
        }
      }
    }
    
    m
  }

PAF <-
  function(pop, non_travel_mmet){
    
    library(stringr)
    unique_age_group <- unique(as.character(pop$age_group))
    unique_gender <- unique(pop$Sex_B01ID)
    combinations <- length(unique_age_group)*length(unique_gender)
    #cn <- grep('mmet$',names(pop), value = TRUE)
    
    cn <- append("baseline_mmet", grep('MS',names(pop), value = TRUE))
    
    m = matrix(nrow=combinations,ncol=(2 + length(cn)))
    
    colnames(m) <- append(c("age band", "gender"), cn)
    mi <- 1
    
    for (i in 1:length(unique_age_group)){
      
      for (j in 1:length(unique_gender)){
        
        reduced_pop <- subset(pop, Sex_B01ID == unique_gender[j] & age_group == unique_age_group[i])
        #cat(unique_gender[j], " : ", unique_age_group[i] , "\n")
        nm_reduced_pop <- subset(non_travel_mmet, Sex_B01ID == unique_gender[j] & age_group == unique_age_group[i])
        
        total_pop <- nrow (reduced_pop) + nrow (nm_reduced_pop)
        active_percent <- nrow (reduced_pop) / total_pop * 100
        non_active_percent <- 100 - active_percent
        #active_percent <- nm_reduced_pop$ActivePer
        #non_active_percent <- 1 - active_percent
        
        size <- nrow (reduced_pop)
        #cat(active_percent, ' : ', non_active_percent , "\n")
        sumPRR <- 0
        #sumPRR <- ((active_percent * sum (reduced_pop[,2])) / size) + (nm_rr * non_active_percent)
        if (nrow(nm_reduced_pop) > 0){
          sumPRR <- (active_percent * sum (reduced_pop$baseline_mmet) / nrow(reduced_pop)) + 
            (sum(nm_reduced_pop$health_mmets) * non_active_percent) / nrow(nm_reduced_pop)
        }
        else{
          sumPRR <- (active_percent * sum (reduced_pop$baseline_mmet) / nrow(reduced_pop))
        }
        #cat("sumPRR ", sumPRR, "\n")
        
        m[mi, 1] = unique_age_group[i]
        m[mi, 2] = unique_gender [j]
        for (k in 1:length(cn)){
          sumPRRi <- 0
          #sumPRRi <- ((active_percent * sum (reduced_pop[, cn[k]])) / size) + (nm_rr * non_active_percent)
          if (nrow(nm_reduced_pop) > 0){
            sumPRRi <- (active_percent * sum (reduced_pop[[cn[k]]]) / nrow(reduced_pop)) + 
              (sum(nm_reduced_pop$health_mmets) * non_active_percent) / nrow(nm_reduced_pop)
          }
          else{
            sumPRRi <- (active_percent * sum (reduced_pop[[cn[k]]]) / nrow(reduced_pop))
          }
          
          PRA <- (sumPRR - sumPRRi) / sumPRR
          #cat(cn[k], " : ", size, " : " , unique_gender[j], " : ", unique_age_group[i], " : ", sumPRR, " : ", sumPRRi, " : ", PRA, "\n")
          m[mi, 2 + k] = round(PRA, digits = 3)
          
        }
        mi <- mi + 1
      }
    }
    
    m
    
  }



populate_health_measures <-
  function(uag, sub){
    nr <- nrow(uag)
    nc <- ncol(uag)
    total_rows <- nr
    m = matrix(nrow=nr, ncol=4 + nc)
    colnames(m) <- c("nts_age_band", "gbd_age_band", "gender", "yll", "yld", "daly", "death")
    m[,1] = uag[,1]
    m[,2] = uag[,2]
    m[,3] = uag[,3]
    #m[1:nr, 1:nr] <- 0
    
    #m = matrix(nrow=combinations,ncol=(3))
    
    #colnames(m) <- mapply(c, list("age band", "gender"), cn, SIMPLIFY=FALSE)
    #colnames(m) <- append(c("age band", "gender"), cn)
    
    for (i in 1:total_rows){
      for (j in 1:2){
        gend <- "Females"
        gend_num <- 2
        col <- 3
        if (j == 1){
          gend <- "Males"
          gend_num <- 1
          col <- 4
        }
        #m[i, col] <- gend_num
        if (nchar(uag[i, 2]) > 0){
          #cat(i, " : ", length(subset(sub, gender == gend & ag_group == uag[i, 2])), ":", gend, " : ", uag[i, 2],  " END \n")
          pop <- subset(sub, gender == gend & ag_group == uag[i, 2])
          #cat(i, " : ", j, " : ", m[i, col], " : ", pop$yll, " END \n")
          m[i, 4]  <- as.numeric.factor(pop$yll)
          m[i, 5]  <- as.numeric.factor(pop$yld)
          m[i, 6]  <- as.numeric.factor(pop$daly)
          m[i, 7]  <- as.numeric.factor(pop$death)
          
        }
      }
      
    }
    
    m[, 4:7] <- sapply(m[, 4:7], as.numeric)
    
    for (i in 1:total_rows){
      if (i + 2 < total_rows){
        if (!nchar(m[i + 1, 1]) > 0){
          
          cat(m[i, 4], " : ", m[i + 1, 4],  "\n")
          m[i, 4]  <- as.numeric(m[i, 4]) + as.numeric(m[i + 1, 4])
          m[i, 5]  <- as.numeric(m[i, 5]) + as.numeric(m[i + 1, 5])
          m[i, 6]  <- as.numeric(m[i, 6]) + as.numeric(m[i + 1, 6])
          m[i, 7]  <- as.numeric(m[i, 7]) + as.numeric(m[i + 1, 7])
          
          m[i + 1, 4] <- 0
          m[i + 1, 5] <- 0
          m[i + 1, 6] <- 0
          m[i + 1, 7] <- 0
          
        }
        
      }
    }
    
    m
    
    
    
  }

create_baseline <-
  function(bl){
    require(sqldf)
    baseline <- sqldf("Select ID, age_group, Sex_B01ID, EthGroupTS_B02ID, NSSec_B03ID from bl group by ID")
    
    baseline
  }

calculate_pif_reduction <-
  function(pop, non_travel_mmet){
    
    library(stringr)
    unique_age_group <- unique(as.character(pop$age_group))
    unique_gender <- unique(pop$Sex_B01ID)
    combinations <- length(unique_age_group)*length(unique_gender)
    cn <- "baseline_mmet"
    m = matrix(nrow=combinations,ncol=(2 + length(cn)))
    colnames(m) <- append(c("age band", "gender"), cn)
    mi <- 1
    for (i in 1:length(unique_age_group)){
      for (j in 1:length(unique_gender)){
        
        reduced_pop <- subset(pop, Sex_B01ID == unique_gender[j] & age_group == unique_age_group[i])
        nm_reduced_pop <- subset(non_travel_mmet, Sex_B01ID == unique_gender[j] & age_group == unique_age_group[i])
        
        total_pop <- nrow (reduced_pop) + nrow (nm_reduced_pop)
        active_percent <- nrow (reduced_pop) / total_pop * 100
        non_active_percent <- 100 - active_percent
        size <- nrow (reduced_pop)
        sumPRR <- 0
        if (nrow(nm_reduced_pop) > 0){
          sumPRR <- (active_percent * sum (reduced_pop$baseline_mmet) / nrow(reduced_pop)) + 
            (sum(nm_reduced_pop$health_mmets) * non_active_percent) / nrow(nm_reduced_pop)
        }
        else{
          sumPRR <- (active_percent * sum (reduced_pop$baseline_mmet) / nrow(reduced_pop))
        }
        m[mi, 1] = unique_age_group[i]
        m[mi, 2] = unique_gender [j]
        for (k in 1:length(cn)){
          m[mi, 2 + k] = round((100 - sumPRR) / 100, digits = 3)
        }
        mi <- mi + 1
      }
    }
    m
    
    m <- data.frame(m)
    m <- arrange(m, age.band, gender)
  }


calculate_health_reductions <-
  function(pop, hc, pr, hm){
    m <- as.matrix(pop)
    #cn <- grep('mmet$',colnames(m), value = TRUE)
    cn <- append("baseline_mmet", grep('MS',names(pop), value = TRUE))
    for (i in 1: length(cn)){
      for (j in 1:nrow(m)){
        
        sub <- subset(as.data.frame(hc), gender == m[j, 2] & age == m[j, 1])
        spr <- subset(pr, pr[,2] == m[j, 2] & pr[,1] == m[j, 1])
        #cat(m[j, 2], " : ", as.character(m[j, 1]), "\n")
        if (length(sub) > 0){
          #cat(m[j, 2], " : ",m[j, 1], " : ", i, " : ", j, cn[i], "\n")
          if (length(sub[[hm]]) > 0){
            m[j, cn[i]] <- round((as.numeric(as.character(m[j, cn[i]])) * 
                                    as.numeric(as.character(sub[[hm]])) / as.numeric(as.character(spr$baseline_mmet))), 2)
            #cat(as.numeric(as.character(m[j, cn[i]])), " : ",as.numeric(as.character(sub[[hm]])) , "\n")
          }else{
            m[j, cn[i]] <- 0
          }
        }else{
          m[j, cn[i]] <- 0
        }
      }
    }
    m
  }


create_trips <-
  function (bl, lObj)
  {
    
    bl <- subset(bl, select = c(ID, Sex_B01ID, age_group, EthGroupTS_B02ID, NSSec_B03ID, MainMode_Reduced, Cycled, HHoldGOR_B02ID))
    
    for (i in 1:length(lObj)){
      tbl <- bl
      sc <- get(as.character(lObj[i]) )
      tbl$now_cycle <- sc$now_cycle
      tbl$ebike <- sc$ebike
      tbl$cyclist <- sc$cyclist
      # f$now_cycle==1 | f$Cycled==1)
      tbl[tbl$now_cycle == 1 | tbl$Cycled == 1 ,]$MainMode_Reduced <- 2
      if (nrow(tbl[tbl$ebike == 1,]) > 0)
        tbl[tbl$ebike == 1,]$MainMode_Reduced <- 2.5
      
      
      bl[[as.character(lObj[i])]] <- tbl$MainMode_Reduced
    }
    bl
    
  }
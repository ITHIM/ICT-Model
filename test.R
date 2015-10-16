
for (i in 1:nrow(bl)) {
      
               if (bl[i,'Age']=='16.59') {
                        if (bl[i,'Sex']=='Male') bl[i,'Pcyc2'] <-0.232
                            else bl[i,'Pcyc2'] <-0.11 
                                }
          
               else {
                        if (bl[i,'Sex']=='Male') bl[i,'Pcyc2']<- 0.092
                            else bl[i,'Pcyc2']<- 0.035
                         }
     
     if (i%%1000==0) message('Iteration: ', i)          
}     
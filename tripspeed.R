tripspeed <-function(age,sex,biketype)  {
  #ageIntervals <-c(16,30,45,60,70,80)
  
if (biketype==0) {     
  if (sex=='Female'){
     if (age=='16.59') speed=10.12
     else           speed=8.27}
  
  else    #sex=male
    {if (age=='16.59') speed=10.87
       else          speed=9.08}
          }     

if (biketype==1) { speed=11.58 }     
  
    tripspeed= speed
  
  
}
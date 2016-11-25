#The "corr" function takes a directory of data files and a threshold for complete cases 
#and calculates the correlation between sulfate and nitrate for monitor locations where 
#the number of completely observed cases (on all variables) is greater than the threshold. 
#The function returns a vector of correlations for the monitors that meet the threshold requirement.

corr<- function (directory,threshold=0){
  
  files <- list.files(directory, full.names = TRUE)
  
  df<- data.frame()
  correlations<- numeric()
  for (i in seq_along(files)) {                                
    current_id<- read.csv(files[i])
    
    nobs<-sum(complete.cases(current_id))
    if (nobs >threshold) {
      corr<-cor(current_id$nitrate,current_id$sulfate,use="complete.obs")
      correlations<-append(correlations,corr)
    }
   
  }
  correlations
}



#cor(test$nitrate,test$sulfate,use="complete.obs")
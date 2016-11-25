# The "complete" function reads a directory full of files and reports the number of completely
# observed cases in each data file. The function returns a data frame where the first column 
# is the name of the file and the second column is the number of complete cases. 

complete<- function (directory,id=1:332){
  
  #Build path for files considering they have 00 in front
  id_new<- sprintf("%03d",id)
  id_new<-paste0(id_new,".csv")
  
  #Read the files, compute complete cases, store into a data frame and rbind in case of multiple id.  dat <- data.frame()
  df<- data.frame()
  for (i in seq_along(id_new)) {                                
    read_current_id<- read.csv(paste0(directory,"/",id_new[i]))
    
    nobs<-sum(complete.cases(read_current_id))
    tmp_df<-data.frame(id[i],nobs)
    df<- rbind(df,tmp_df)
  }
  #Modify column names and return final data frame
  names(df)<-c("id","nobs")
  df
}
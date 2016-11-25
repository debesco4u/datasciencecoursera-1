#Assignment 1 week 2 - R Programming
#pollutantmean function

#Download and unzip the data into "specdata" directory
dataset_url <-"https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip" 
download.file(dataset_url, "rprog%2Fdata%2Fspecdata.zip")
unzip("rprog%2Fdata%2Fspecdata.zip", exdir = "specdata")


#The "pollutantmean" function reads a directory containing multiple files, and
#calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. 

pollutantmean<- function (directory,pollutant,id=1:332){
  ## "directory" and pollutant are character vector of length 1 indicating 
  ## the location of the csv files;
  
  ## "pollutant" is a character vector of length 1 indicating the name of the
  ## pollutant for which we will calculate the mean: either "sulfate" or "nitrate";
  
  ## id is an integer vector indicating the monitor ID numbers to be used: can be 1 or more
  
  ## The function returns the mean of the pollutant across all monitors list in the
  ## "id" vector (ignoring NA values); note: do not round results.
  
  ##Build path for files considering they have 00 in front
  id_new<- sprintf("%03d",id)
  id_new<-paste0(id_new,".csv")
  
  #Read the files, rbind in case of multiple id and store into en empty data frame
  dat <- data.frame()
  for (i in seq_along(id_new)) {                                
    dat <- rbind(dat, read.csv(paste0(directory,"/",id_new[i])))
  }
  
  ##Extract the pollutant and calculate the mean excuding NA values
  polMean <- mean(dat[[pollutant]], na.rm = TRUE)
  polMean
}
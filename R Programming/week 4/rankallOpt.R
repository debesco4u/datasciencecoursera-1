##### double check: it does not work when selected "worst" it gives NA

rankallOpt<- function (outcome, num="best") {
  ## Read outcome data 
  data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  outcomeVec<- c("heart attack","heart failure", "pneumonia")
  stateVec<- unique(data$State)
  
#   if (!(state%in%statevec)) {
#     stop ("invalid state") 
#   }
  if (!(outcome%in%outcomeVec)) {
    stop ("invalid outcome") 
  }
  
  ## Keep only columns needed, rename and convert to numeric
  df1<-data[,c(2,7,11,17,23)]
  names(df1)<-c("hospital","state.name",outcomeVec)
  df1[outcomeVec] <- sapply(df1[outcomeVec],as.numeric)
  
  ## Build the final data frame depending on the outcome selected by the user
  stat<-df1[[outcome]]
  hosState<-df1[,1:2]
  df2<-cbind(hosState,stat)
  
  splited = split(df2, df2$state.name)
  ans = lapply(splited, function(x, num) {
    # Order by Deaths and then HospitalName
    x = x[order(x$stat, x$hospital),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$hospital[1])
      }
      else if(num == "worst") {
        return (x$hospital[nrow(x)])
      }
    }
    else {
      return (x$hospital[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
  
  
}


  
  ## Count the number of rows
  n<-nrow(ordered)
  
  # Extract best, worst or specific number as per selected by user in the argument
  if (num=="best") {
    library(dplyr)
    df2 %>%
      group_by(hospital,state.name) %>%
      summarize(stat=max(stat))
    
    
    hospitalname<-ordered[1,1]
    x<-as.character(hospitalname)
  }
  if (num=="worst") {
    hospitalname<-ordered[n,1]
    x<-as.character(hospitalname)
  }
  
  if (num<=n){
    hospitalname<-ordered[num,1]
    x<-as.character(hospitalname)
  } 
  if (num>n) {
    x<-NA
  }
  
  
  return (x)
}


#######################################################
Here is some sample output from the function.

> source("rankall.R")
> head(rankall("heart attack", 20), 10)
hospital state
AK <NA> AK
AL D W MCMILLAN MEMORIAL HOSPITAL AL
AR ARKANSAS METHODIST MEDICAL CENTER AR
AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
CA SHERMAN OAKS HOSPITAL CA
CO SKY RIDGE MEDICAL CENTER CO
CT MIDSTATE MEDICAL CENTER CT
DC <NA> DC
DE <NA> DE
FL SOUTH FLORIDA BAPTIST HOSPITAL FL
> tail(rankall("pneumonia", "worst"), 3)
hospital state
WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
WV PLATEAU MEDICAL CENTER WV
WY NORTH BIG HORN HOSPITAL DISTRICT WY
> tail(rankall("heart failure"), 10)
hospital state
TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
TX FORT DUNCAN MEDICAL CENTER TX
UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
VA SENTARA POTOMAC HOSPITAL VA
VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
VT SPRINGFIELD HOSPITAL VT
WA HARBORVIEW MEDICAL CENTER WA
WI AURORA ST LUKES MEDICAL CENTER WI
WV FAIRMONT GENERAL HOSPITAL WV
WY CHEYENNE VA MEDICAL CENTER WY

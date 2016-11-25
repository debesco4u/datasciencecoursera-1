best<- function (state, outcome) {
  
  ## Read outcome data 
  data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  outcomeVec<- c("heart attack","heart failure", "pneumonia")
  stateVec<- unique(data$State)
  
  if (!(state%in%stateVec)) {
    stop ("invalid state") 
  }
  if (!(outcome%in%outcomeVec)) {
    stop ("invalid outcome") 
  }
  
  ## Keep only columns needed, rename and convert to numeric
  df1<-data[,c(2,7,11,17,23)]
  names(df1)<-c("hospital","state.name",outcomeVec)
  df1[outcomeVec] <- sapply(df1[outcomeVec],as.numeric)

  ## Subset by state selected by user
  sub<- subset(df1, state.name==state)
  
  ## Order by outcome selected by user, extract best hospital and return it.
  ordered <- sub[order(sub[[outcome]], sub[,1],na.last=NA),]
  x<-ordered[1,1]
  
  return (x)
}


# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

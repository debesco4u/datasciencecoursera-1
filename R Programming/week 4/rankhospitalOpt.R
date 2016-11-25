##### double check: it does not work when selected "worst" it gives NA

rankhospitalOpt<- function (state, outcome, num="best") {
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
  
  ## Count the number of rows
  n<-nrow(ordered)
  
  # Extract best, worst or specific number as per selected by user in the argument
    if (num=="best") {
      hospitalname<-ordered[1,1]
      x<-as.character(hospitalname)
    }
    if (num=="worst") {
      #ordered = ordered[!is.na(ordered[[outcome]]),]
      
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
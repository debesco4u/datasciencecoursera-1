rankall<- function (outcome, num="best") {
  data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcomevec<- c("heart attack","heart failure", "pneumonia")
  
  if (!(outcome%in%outcomevec)) {
    stop ("invalid outcome") 
  }
  ha<- as.numeric(data[, 11]) 
  #print(ha)
  hf<- as.numeric(data[, 17])
  pn<- as.numeric(data[, 23])
  hospital<-data[,2]
  state<-data[,7]
  #print(sta)
  data3<- data.frame(hospital=hospital,state=state,ha=ha,hf=hf,pn=pn)
  #print(head(data3))
  
  if (outcome=="heart attack") {
    data3_ha <- data3[order(data3[,3], data3[,1],na.last=NA), ]
    #data3_hf <- data3[order(data3[,4], data3[,1],na.last=NA), ]
    #data3_pn <- data3[order(data3[,5], data3[,1],na.last=NA), ]
    s_ha<- split(data3_ha, data3_ha$state)
    length_split <- lapply(s_ha, function(x) length(x[,"state"]))
    n<-as.numeric (lengthsplit)  # non so se va bene?
    
    if (num=="best") {
      lap_ha <- lapply(s_ha, function(x) x[1,])
      df<- as.data.frame(do.call(rbind, lap_ha))
      h<- df[, 1]
      s<- df[, 2]
      df1<-data.frame(hospital=h, state=s)
      x<-df1
    }
    #non funziona
    if (num=="worst") {
      lap_ha <- lapply(s_ha, function(x) x[n,1:2])
      x<- as.data.frame(do.call(rbind, lap_ha)) # ha senso?
    }
    
    #funziona
    if (num<=n){
      lap_ha <- lapply(s_ha, function(x) x[num,])
      df<- as.data.frame(do.call(rbind, lap_ha))
      h<- df[, 1]
      s<- df[, 2]
      df1<-data.frame(hospital=h, state=s)
      x<-df1
    }
  }
  
  return (x)
  
}

  
  
 
    
    
    #l<- lapply (s, function(x)s[order(s[,3], s[,1], na.last=NA)])
    

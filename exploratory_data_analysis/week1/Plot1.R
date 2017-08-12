#Plot1: histogram

## Read file and subset only necessary rows
houseConsumption<- read.table("household_power_consumption.txt",sep=";",header=TRUE)
library(dplyr) #install dplyr package if you don't have it
houseConSub<- houseConsumption%>%
  filter(Date=="1/2/2007"|Date=="2/2/2007")

## Convert to Date format and create new variable concatenating Date and Time
houseConSub$Date<-as.Date(houseConSub$Date,"%d/%m/%Y")
houseConSub<- houseConSub%>%
  mutate(DateTime= as.POSIXct(paste(Date, Time)))

## Convert measure columns to numeric to be able to create plots later
for(i in 3:9) {
  houseConSub[,i]<-as.numeric(as.character((houseConSub[,i])))
}


## Plot
png("plot1.png", width=480, height=480)
hist(houseConSub$Global_active_power,col = "red", main = "Global Active Power",
          xlab = "Global Active Power (killowatts)", ylab = "Frequency")
dev.off()

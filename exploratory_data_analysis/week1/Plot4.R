#Plot4: multiple plots

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


#Plot
png("plot4.png", width=480, height=480)
par(mfrow=c(2,2))
with(houseConSub, {
    plot(DateTime,Global_active_power,type="l",xlab="",
         ylab="Global Active Power")
    plot(DateTime,Voltage,type="l",xlab="datetime",
       ylab="Voltage")
    plot(DateTime,Sub_metering_1,type="l", ylab="Energy sub metering", xlab="",col="grey")
    lines(DateTime,Sub_metering_2,type="l", col="red")
    lines(DateTime,Sub_metering_3,type="l", col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           lty=1, lwd=2,col=c("grey", "red", "blue"))
    plot(DateTime,Global_reactive_power,type="l",xlab="datetime",
         ylab="Global_reactive_power")
})

dev.off()
loadData<-function() {
  table<-read.table("household_power_consumption.txt",header=TRUE,sep=";",colClasses=c("Date","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),na.strings="?")
}

makePlot<-function(reloadData=FALSE) {
  if(reloadData) {
    loadData()
  }
  
  #Fix date format
  table$Date<-as.Date(table$Date,"%d/%m/%Y")
  
  # Filter to February 1-2, 2007
  filtered<-table
  filtered<-filtered[filtered$Date>as.Date("31/1/2007","%d/%m/%Y"),]
  filtered<-filtered[filtered$Date<as.Date("3/2/2007","%d/%m/%Y"),]
  
  # Add date/time on pared down table
  filtered$datetime<-as.POSIXct(paste(filtered$Date, filtered$Time), format="%Y-%m-%d %H:%M:%S")
  
  # Make graph
  par(mfrow=c(2,2))
  
  
  # (1,2)
  plot(filtered$datetime,filtered$Global_active_power,xlab="",ylab="Global Active Power (kilowatts)",type="l")
  
  # (2,1)
  plot(filtered$datetime,filtered$Voltage,xlab="datetime",ylab="Voltage",type="l")
  
  # (1,2)
  plot(range(filtered$datetime), range(filtered$Sub_metering_1,filtered$Sub_metering_2,filtered$Sub_metering_3),xlab="",ylab="Energy sub metering",type='n')
  lines(filtered$datetime,filtered$Sub_metering_1, type="l",col="black")
  lines(filtered$datetime,filtered$Sub_metering_2, type="l",col="red")
  lines(filtered$datetime,filtered$Sub_metering_3, type="l",col="blue")
  legend('topright',c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),bty="n",lwd=2,cex=0.7)
  
  # (2,2)
  plot(filtered$datetime,filtered$Global_reactive_power,xlab="datetime",ylab="Global_reactive_power",type="l")
  #axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5),label=c(0,0.1,0.2,0.3,0.4,0.5))
  
  # Export to PNG
  dev.copy(png, file = "plot4.png",width=1024,height=768)
  dev.off()
}
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
  filtered$DateTime<-as.POSIXct(paste(filtered$Date, filtered$Time), format="%Y-%m-%d %H:%M:%S")
  
  # Make graph
  plot(range(filtered$DateTime), range(filtered$Sub_metering_1,filtered$Sub_metering_2,filtered$Sub_metering_3),xlab="",ylab="Energy sub metering",type='n')
  lines(filtered$DateTime,filtered$Sub_metering_1, type="l",col="black")
  lines(filtered$DateTime,filtered$Sub_metering_2, type="l",col="red")
  lines(filtered$DateTime,filtered$Sub_metering_3, type="l",col="blue")
  legend('topright',c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lwd=2)
  
  # Export to PNG
  dev.copy(png, file = "plot3.png",width=640)
  dev.off()
}
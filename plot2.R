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
  plot(filtered$DateTime,filtered$Global_active_power,xlab="",ylab="Global Active Power (kilowatts)",type="l")
  
  # Export to PNG
  dev.copy(png, file = "plot2.png",width=640)
  dev.off()
}
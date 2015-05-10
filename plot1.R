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
  
  # Make graph
  hist(filtered$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
  
  # Export to PNG
  dev.copy(png, file = "plot1.png")
  dev.off()
}
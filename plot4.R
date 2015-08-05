plot4 <- function(){
  
  file<-"household_power_consumption.txt" #read file
  table<-read.table(file,sep=";",header=TRUE) #read file to table
  dt<-paste(table$Date,table$Time) #Dummy variable to hold Date & Time 
  dtstrp<-strptime(dt, format="%d/%m/%Y %H:%M:%S") #Date & Time strp'ed
  dtstrp<-format(dtstrp, tz="") #Remove Time Zone
  table2<-table[,3:9] #Create new table with date/time removed
  index<-grepl("2007-02-01|2007-02-02", dtstrp) #Indexing vector to subset two dates
  table3<-cbind(dtstrp[index],table2[index,]) #pick only subsetted values from table 2
  newcolname<-c("Date/Time",colnames(table2)) #Defining column names for table2
  colnames(table3)<-newcolname #apply new column name
  table3$`Date/Time`<-strptime(table3$`Date/Time`, format="%Y-%m-%d %H:%M:%S")
  
  
  #Code to Generate Plot 4:
  png(filename="plot4.png", width=480, height=480, units="px") #Make png with width/height as 400 pxs
  par(mfrow=c(2,2))
  
  
  #Plot 2 - Top Left
  plot(as.numeric(as.character(table3$Global_active_power))~as.POSIXct(table3$`Date/Time`), type="l", ylab = "Global Active Power (kilowatts)", xlab="")
  
  #Voltage vs datetime - Top Right
  plot(as.numeric(as.character(table3$Voltage))~as.POSIXct(table3$`Date/Time`), type="l", xlab="datetime", ylab="Voltage")
  
  #Plot 3 - Bottom Left
  plot(as.numeric(as.character(table3$Sub_metering_1))~as.POSIXct(table3$`Date/Time`), type="l", xlab="",ylab="Energy sub metering")
  lines(as.numeric(as.character(table3$Sub_metering_2))~as.POSIXct(table3$`Date/Time`), col="red")
  lines(as.numeric(as.character(table3$Sub_metering_3))~as.POSIXct(table3$`Date/Time`), col="blue")
  legend("topright", col=c("black","red","blue"), lwd=1, legend=colnames(table3[6:8]))
  
  #Plot 4 - Bottom Right
  plot(as.numeric(as.character(table3$Global_reactive_power))~as.POSIXct(table3$`Date/Time`), type="l", xlab="datetime", ylab="Global_reactive_power")
  
  
  dev.off()
  
  
}
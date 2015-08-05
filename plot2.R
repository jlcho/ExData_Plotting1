plot2 <- function(){
  
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
  
  #Code to Generate Plot 2:
  png(filename="plot2.png", width=480, height=480, units="px") #Make png with width/height as 400 pxs
  plot(as.numeric(as.character(table3$Global_active_power))~as.POSIXct(table3$`Date/Time`), type="l", ylab = "Global Active Power (kilowatts)", xlab="")
  #Generate Plot 2
  dev.off()

  
}
# read the data from given link using temp 
URL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
temp<- tempfile()
download.file(URL,temp)
# unzip th efile in current WD
unzip(temp,exdir=getwd())
# delete the zip from temp 
unlink (temp)
# avoid types, assign file name in var
file_name <- "household_power_consumption.txt"
# read file
mydata <- read.table(file_name, sep = ";", header = TRUE, na.strings = "?")
# merge date & time & put in new col
mydata$DateTime<-paste(mydata$Date, mydata$Time)


# get the data in interest
mydata$DateTime<-strptime(mydata$DateTime, "%d/%m/%Y %H:%M:%S")
start<-which(mydata$DateTime==strptime("2007-02-01", "%Y-%m-%d"))
end  <-which(mydata$DateTime==strptime("2007-02-02 23:59:00", "%Y-%m-%d %H:%M:%S"))

# assign interest data in new var
Jdata<-mydata[start:end,]

par(mfcol=c(2,2))

# Plot for Global Active Power 
plot(Jdata$DateTime, 
      as.numeric(as.character(Jdata$Global_active_power)),
      type="l",
      ylab="Global Active Power", 
      xlab="")

# Plot for Energy sub meterings

plot(Jdata$DateTime, 
      as.numeric(as.character(Jdata$Sub_metering_1)),
      type="l", 
      xlab="",
      ylab ="Energy sub metering")

# setting the lines metering 2
lines(Jdata$DateTime, 
        as.numeric(as.character(Jdata$Sub_metering_2)),
        type="l", 
        col='red')
# setting the lines for metering 3
lines(Jdata$DateTime, 
        Jdata$Sub_metering_3,
        type="l", 
        col="blue")

# setting the legends 
legend('topright', 
          c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
          lty=c(1,1,1),
          col=c("black","red","blue"))

# Voltage v/s day graph
plot(Jdata$DateTime, 
      as.numeric(as.character(Jdata$Voltage)),
      type="l", 
      ylab="Voltage",
      xlab="datetime" )
# Global reactive power v/s day graph
plot(Jdata$DateTime, 
      as.numeric(as.character(Jdata$Global_reactive_power)),
      type="l", 
      ylab="Global_reactive_power",
      xlab="datetime" )

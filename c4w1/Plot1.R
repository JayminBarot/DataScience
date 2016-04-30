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

hist(as.numeric(as.character(Jdata$Global_active_power)), 
     main= "Global Active Power", 
     col = "red" ,
     xlab="Global Active Power (Kilowatts)"  )

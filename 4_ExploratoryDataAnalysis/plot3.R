plot3 <- function() {
  df <- read.csv.sql( file='household_power_consumption.txt', sep=";", 
                      sql="select * from file where Date = '1/2/2007' or Date = '2/2/2007'", header=TRUE)
  
  ## combining the first two columns Date & Time & converting to required datetime format for plotting.
  dt <- paste(df[,1], df[,2])
  datetime <- strptime(dt, '%d/%m/%Y %H:%M:%S')
  
  ## plotting the 3 metering plots with block, red & blue with lines.
  plot(datetime, df$Sub_metering_1,  type="l", ylab="Energy sub metering", xlab="")
  lines(datetime, df$Sub_metering_2, type="l", col="red")
  lines(datetime, df$Sub_metering_3, type="l", col="blue")
  
  ## applying legent on top right.
  legend("topright", col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
         lty="solid", cex=0.7, xjust=0.9, yjust=0.5, y.intersp=0.7)
  
  ## creating the plot2 png file for submission.
  dev.copy(png, file = "plot3.png")
  dev.off()
  
  # now close the connection and destroy the database
  sqldf()
}
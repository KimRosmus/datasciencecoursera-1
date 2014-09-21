plot4 <- function() {
  
  df <- read.csv.sql( file='household_power_consumption.txt', sep=";", 
                      sql="select * from file where Date = '1/2/2007' or Date = '2/2/2007'", header=TRUE)
  
  ##a. combining the first two columns Date & Time & converting to required datetime format for plotting.
  dt <- paste(df[,1], df[,2])
  datetime <- strptime(dt, '%d/%m/%Y %H:%M:%S')
  
  ##b. setting the plot region into 2x2 plot region.
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))
  
  ##1. first plot
  c3 <- df$Global_active_power
  plot(datetime, c3, type="l", ylab="Global Active Power", xlab="", cex.lab="0.9")
 
  ##2. second plot
  plot(datetime, df[,5], type="l", ylab="Voltage", xlab="datetime", cex.lab="0.9")
  
  ##3 third plot
  plot(datetime, df$Sub_metering_1,  type="l", ylab="Energy sub metering", xlab="", cex.lab="0.9")
  lines(datetime, df$Sub_metering_2, type="l", col="red")
  lines(datetime, df$Sub_metering_3, type="l", col="blue")
  legend("topright", col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty="solid", 
         bty="n", cex=0.7, xjust=0.9, yjust=0.5, y.intersp=0.7)
  
  ##4 fourth plot
  plot(datetime, df[,4], type="l", ylab="Global_reactive_power", xlab="datetime", cex.lab="0.9")
  
  ## creating the plot4 png file for submission and closing device connection.
  dev.copy(png, file = "plot4.png")
  dev.off()
  
  # now close the connection and destroy the database
  sqldf()
  
}
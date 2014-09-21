plot2 <- function() {
  df <- read.csv.sql( file='household_power_consumption.txt', sep=";", 
                      sql="select * from file where Date = '1/2/2007' or Date = '2/2/2007'", header=TRUE)
  
  ##1 combining the first two columns Date & Time
  dt <- paste(df[,1], df[,2])
  
  ##2 converting to required datetime format for plotting.  
  datetime <- strptime(dt, '%d/%m/%Y %H:%M:%S')
  
  ##3 plotting the requested graph
  c3 <- df$Global_active_power
  plot(datetime, c3, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  
  ##4 creating the plot2 png file for submission.
  dev.copy(png, file = "plot2.png")
  dev.off()
  
  # now close the connection and destroy the database
  sqldf()
}
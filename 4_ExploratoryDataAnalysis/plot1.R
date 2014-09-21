plot1 <- function() {
  df <- read.csv.sql( file='household_power_consumption.txt', sep=";", 
                      sql="select * from file where Date = '1/2/2007' or Date = '2/2/2007'", 
                      header=TRUE)
  
  ## create the required plot.
  hist(as.numeric(df$Global_active_power), 
       col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
  
  ## copy the plot and close the device connection
  dev.copy(png, file = "plot1.png")
  dev.off()
  
  # now close the connection and destroy the database
  sqldf()
}
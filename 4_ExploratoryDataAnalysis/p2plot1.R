p2plot1 <- function() {
  ##1.read the data.
  NEI <- readRDS("summarySCC_PM25.rds")
  
  ##2.extract the relevant columns from original data
  nei1 <- NEI[,c("Emissions", "year")]
  
  ##3.calculated total PM2.5 emission from all sources for each of 
  ## the years 1999, 2002, 2005 & 2008.
  fnei <- aggregate(nei1$Emissions ~ nei1$year, nei1, sum)
  names(fnei) <- c("year", "emissions");
  
  ##4.plot the data obtained in step 3. 
  plot(fnei$year, fnei$emissions, type="l", col="red", xlab="Year", ylab="PM2.5 Emissions")
  
  ##5 creating the p2plot2 png file for submission.
  dev.copy(png, file = "p2plot1.png")
  dev.off()
}
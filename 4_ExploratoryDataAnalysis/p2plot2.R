p2plot2 <- function() {
  
  ##1.read the data.
  NEI <- readRDS("summarySCC_PM25.rds")

  ##2. extract emissions in the Baltimore City, Maryland (fips == 24510) 
  balt_nei <- NEI[NEI$fips == 24510,]
  
  ##3. calculate total emissions from PM2.5 in the Baltimore City, 
  ## Maryland (fips == 24510) from 1999 to 2008
  fnei <- aggregate(balt_nei$Emissions ~ balt_nei$year, balt_nei, sum)
  names(fnei) <- c("year", "emissions")
  
  ##4. plot for submission - project 2 plot 2
  plot(fnei$year, fnei$emissions, type="l", col="red", xlab="Year", ylab="Baltimore PM2.5 Emissions")
  
  ##5 creating the p2plot2 png file for submission.
  dev.copy(png, file = "p2plot2.png")
  dev.off()
}
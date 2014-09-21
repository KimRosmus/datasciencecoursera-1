p2plot3 <- function() {

  ##1. read the file and extract data related to baltimore city
  NEI <- readRDS("summarySCC_PM25.rds")
  balt_nei <- NEI[NEI$fips == 24510,]

  ##2. extracting year, type & emission data
  bnei <- balt_nei[,4:6]
  
  ##3. aggregating based on year and type.
  r1 <- aggregate(bnei$Emissions ~ bnei$type + bnei$year, bnei, sum)
  names(r1) <- c("type", "year", "emissions")
  
  ##4. plotting the required graph.
  qplot(r1$year, r1$emissions, data=r1, geom=c("point", "smooth"), 
        color=r1$type, xlab="Year", ylab="Baltimore PM2.5 Emissions",method="loess")
  
  ##5 creating the p2plot3 png file for submission.
  dev.copy(png, file = "p2plot3.png")
  dev.off()
}


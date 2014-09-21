p2plot5 <- function() {
  ##1. reading the relevant data
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
 
  ##2 for the definition of "motor vehicle" am quering SCC$EI.Sector for "vehicle" 
  ## unique(grep("vehicle", SCC$EI.Sector, ignore.case=T, value=T))
  ## [1] "Mobile - On-Road Gasoline Light Duty Vehicles" 
  ## [2] "Mobile - On-Road Gasoline Heavy Duty Vehicles"
  ## [3] "Mobile - On-Road Diesel Light Duty Vehicles"   
  ## [4] "Mobile - On-Road Diesel Heavy Duty Vehicles"  
  mv <- grep("vehicle", SCC$EI.Sector, ignore.case=T)
  mv_scc <- as.character(SCC[mv,c("SCC")])
  
  ##3a.extracting "Baltimore city" rows from NEI with SCC data obtained from step 2.
  nei5 <- NEI[NEI$fips == "24510",c("SCC", "Emissions", "year")] 
  nei51 <- nei5[nei5$SCC %in% mv_scc,]
  x <- SCC[mv,c("SCC", "EI.Sector")]
  
  ##3b. Extracting the relevant motor vehicle sources and pasting with nei51 data source.
  vect <- character()
  nro <- nrow(nei51)
  for (i in 1:nro) {
    ind <- grep(nei51[i,1], x[,1],)
    val <- as.character(x[ind,2])
    vect <- c(vect, val)
  }
  nei51$motor_vehicle <- vect
 
  ##4. aggregating emissions from various motor vehicle resources according to year
  v1 <- aggregate(nei51$Emissions ~ nei51$year + nei51$motor_vehicle, nei51, sum)
  names(v1) <- c("year", "vehicle", "emissions")
  
  ##5. Plotting the requested graph.
  fit <- qplot(v1$year, v1$emissions, data=v1, geom=c("point", "smooth"), color=vehicle, 
        xlab="Year", ylab=expression("Motor Vehicle " * PM[2.5] * " Emissions"), method="loess")
  
  ##6. Copying the plot to p2plot5.png and closing the connection.
  png("p2plot5.png")
  plot(fit)
  dev.off()
}
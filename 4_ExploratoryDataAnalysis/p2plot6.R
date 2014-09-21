p2plot6 <- function() {
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
  
  ## extracting "Baltimore city" rows from NEI with SCC data obtained from step 2.
  nei5 <- NEI[NEI$fips == "24510",c("SCC", "Emissions", "year")] 
  nei51 <- nei5[nei5$SCC %in% mv_scc,]
  
  ##extracting "LA County California" rows from NEI with SCC data obtained from step 2.
  nei6 <- NEI[NEI$fips == "06037",c("SCC", "Emissions", "year")]
  nei61 <- nei6[nei6$SCC %in% mv_scc,]

  
  x <- SCC[mv,c("SCC", "EI.Sector")]
  
  ## Extracting the relevant motor vehicle sources and pasting with nei51 data source.
  vect <- character()
  nro <- nrow(nei51)
  for (i in 1:nro) {
    ind <- grep(nei51[i,1], x[,1],)
    val <- as.character(x[ind,2])
    vect <- c(vect, val)
  }
  nei51$motor_vehicle <- vect
  ## aggregating emissions from various motor vehicle resources according to year-Baltimore
  b1 <- aggregate(nei51$Emissions ~ nei51$year, nei51, sum)
  names(b1) <- c("year", "emissions")
  
  ##Extracting the relevant motor vehicle sources and pasting with nei61 
  ## data source - LA County - California
  vect <- character()
  nro <- nrow(nei61)
  for (i in 1:nro) {
    ind <- grep(nei61[i,1], x[,1],)
    val <- as.character(x[ind,2])
    vect <- c(vect, val)
  }
  nei61$motor_vehicle <- vect
  ## aggregating emissions from various motor vehicle resources according 
  ## to year for LA County - California
  c1 <- aggregate(nei61$Emissions ~ nei61$year, nei61, sum)
  names(c1) <- c("year", "emissions")
  
  ##unifying the range for Emissions in Baltimore & LosAngeles County - California
  yc1 <- log10(c1$emissions)
  yb1 <- log10(b1$emissions)
  rng <- range(yb1, yc1)

  ## merging the b1 & c1 dataframes and converting the total emissions in log10 scale.
  t <- data.frame(b1$year, log10(b1$emissions))
  t$cali <- log10(c1$emissions)

  ##appropriate names for merged dataframes.
  names(t) <- c("year", "Baltimore-City", "LA-California")
  
  ## we need library(reshape) for below step.
  dfm <- melt(t, id="year")
  
  ## plotting the melted data for Baltimore & California PM2.5 Emissions.
  ggplot(data = dfm, aes(x = year, y = value, color=variable)) +geom_point()  + stat_smooth(method="lm") +
    labs(y=expression(log[10](PM[2.5]*Emissions)), x="Year") 

  ## Copying the plot to p2plot6.png and closing the connection.
  ggsave(file="p2plot6.png", width=5, height=5, dpi=100)
}
p2plot4 <- function() {
  ##1. reading the relevant data
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ##2. indentifying & extracting coal combustion related sources.
  icoal <- grep("coal", SCC$EI.Sector, ignore.case=T)
  fscc <- as.character(SCC[icoal,c("SCC")])
  
  ##3. extracting coal combustion related SCCs from NEI data source
  nei1 <- NEI[,c("SCC", "Emissions", "year")]  
  nei2 <- nei1[nei1$SCC %in% fscc,]
  z <- SCC[icoal,c("SCC", "EI.Sector")]
  

  vec <- character()
  nr <- nrow(nei2)
  for (i in 1:nr) {
    ind <- grep(nei2[i,1], z[,1],)
    val <- as.character(z[ind,2])
    vec <- c(vec, val)
  }
  nei2$cct <- vec
  
  ##4. aggregate emissions from coal according to year
  c1 <- aggregate(nei2$Emissions ~ nei2$year + nei2$cct, nei2, sum)
  names(c1) <- c("year", "cct", "emissions")
  
  ##6. create the plot
  qplot(c1$year, c1$emissions, data=c1, geom=c("point", "smooth"), color=cct, 
        xlab="Year", ylab=expression("Coal Combustion Type " * PM[2.5] * " Emissions"), method="loess")

  ##6 creating the p2plot4 png file for submission.
  dev.copy(png, file = "p2plot4.png")
  dev.off()
}
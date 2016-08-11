#############################################################
#### Obtain summary statistics for XCMS data 
#############################################################


# Summary statistics function
sumstats <- function(z) {
  	Mean <- apply(z, 1, mean)
  	Median <- apply(z, 1, median)
  	SD <- apply(z, 1, sd)
  	SE <- apply(z, 1, function(x) sd(x)/sqrt(length(x)))
  	CV <- apply(z, 1, function(x) sd(x)/mean(x))
  	result <- data.frame(Mean, Median, SD, SE, CV)
  	return(result)
}

data <- read.csv("xcmsresults.csv", header=T, row.names=1)
newdf <- sumstats(data)
write.csv(newdf, "summary_statistics.csv")

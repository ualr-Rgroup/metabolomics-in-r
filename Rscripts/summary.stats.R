#############################################################
# Obtain summary statistics for XCMS data 
#############################################################

# This script will determine the mean, median, standard deviation, 
# standard error, and coefficient of variation for each variable 
# and return the results into a data frame

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

# Import data
data <- read.csv("xcmsresults.csv", header=T, row.names=1)

# Apply summary statistics function
newdf <- sumstats(data)

# Save results to file
write.csv(newdf, "summary_statistics.csv")

# end

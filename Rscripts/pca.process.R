#############################################################
# Principal Component Analysis (PCA) in R
#############################################################

# This script will perform PCA on peak integration
# results from XCMS processed data.  Refer to the
# xcms.process.R file to see how the data were generated.

# Copy the "xcmsresults.csv" file into the working directory.
# This file contains intensity values for 308 peaks from
# 38 samples belonging to four experimental groups.

# Set working directory
setwd("~/R/pca/")

# Import data
dat1 <- read.csv("xcmsresults.csv", header=T, row.names=1, 
dat1[1:6,1:4]  # look at the first few rows

# Replace zeroes with a small value

# This function will find the minimum value greater than zero 
# and divide that value by two
replacezero = function(x) "[<-"(x, !x | is.na(x), min(x[x > 0], na.rm = TRUE) / 2)

# Apply function across rows
dat2 <- as.data.frame(t(apply(dat1, 1, replacezero)))
dat2[1:6,1:4]  # look at the first few rows

# Log transform the data (base 2 log)
logdata <- log(dat2, 2)

# Function for pareto scaling
paretoscale <- function(z) {
  rowmean <- apply(z, 1, mean) # row means
  rowsd <- apply(z, 1, sd)  # row standard deviation
  rowsqrtsd <- sqrt(rowsd) # sqrt of sd
  rv <- sweep(z, 1, rowmean,"-")  # mean center
  rv <- sweep(rv, 1, rowsqrtsd, "/")  # dividing by sqrtsd
  return(rv)
}

# Pareto scale log transformed data
logdata.pareto <- paretoscale(logdata)

# Run PCA (note use of "t" to transpose matrix)
pca <- prcomp(t(logdata.pareto), center=F, scale=F)

# Create a container called "results" for PCA results
results <- summary(pca)

results$importance  # summary table of explained variance
results$x           # scores matrix
results$rotation    # loadings matrix

# Make a simple scree plot
plot(results$importance[2,1:10], type="b", 
     main="Proportion of Explained Variance",
     xlab="PC", ylab="Proportion of Variance")

# Plot the cumulative proportion of variance
plot(results$importance[3,1:10], type="b", 
     main="Cumulative Proportion of Variance",
     xlab="PC", ylab="Proportion of Variance")

# Make a simple scores plot
plot(pca$x[,1], pca$x[,2], type='p', cex=0, pch=20, 
     main="Scores Plot", xlab="PC1", ylab="PC2")

# add text labels to data points
text(pca$x[,1], pca$x[,2], labels=rownames(pca$x), cex=1.0)
abline(h=0, v=0, col="red")

# Make a simple loadings plot (variance among variables)
plot(pca$rotation[,1], pca$rotation[,2], type='p', cex=0.5, pch=20, 
     main="Loadings Plot", xlab="PC1", ylab="PC2")

# add text labels for data points
text(pca$rotation[,1], pca$rotation[,2], labels=rownames(pca$rotation), cex=1.0)
abline(h=0, v=0, col="red")

# Extract PCA results into data frames

scree.data <- as.data.frame(results$importance)
score.data <- as.data.frame(results$x)
loadings.data <- as.data.frame(results$rotation)

# Save PCA results to file (we'll use later)
write.csv(scree.data, "pca_scree.csv")
write.csv(score.data, "pca_scores.csv")
write.csv(loadings.data, "pca_loadings.csv")

# Find important variables (Loadings)

plot(loadings.data$PC1, loadings.data$PC2)
abline(v=0.09, col="red")
abline(v=-0.09, col="red")
abline(h=0.09, col="red")
abline(h=-0.09, col="red")

# Make a new data frame with PC1, PC2, and PC3 loadings
loadings.PC1.PC2 <- loadings.data[,1:3]
loadings.PC1.PC2[1:6,1:3]  # look at the first few rows

# subset significant loadings
loadings.sig <- subset(loadings.PC1.PC2,
                       PC1 > 0.09 | PC1 < -0.09 |
                         PC2 > 0.09 | PC2 < -0.09)

# sanity check - plot the results
plot(loadings.sig$PC1, loadings.sig$PC2)

# Use the "ifelse" function to mark high loadings

# PC1 loadings
loadings.sig$pc1.change <-
  ifelse(loadings.sig$PC1 > 0.09,"UP",
         ifelse(loadings.sig$PC1 < -0.09,"DOWN",
                "none"))

# PC2 loadings
loadings.sig$pc2.change <-
  ifelse(loadings.sig$PC2 > 0.09,"UP",
         ifelse(loadings.sig$PC2 < -0.09,"DOWN",
                "none"))

# Number of signficant PC1 loadings
length(which(loadings.sig$pc1.change=="UP"))
length(which(loadings.sig$pc1.change=="DOWN"))
length(which(loadings.sig$pc1.change=="none"))

# Number of signficant PC2 loadings
length(which(loadings.sig$pc2.change=="UP"))
length(which(loadings.sig$pc2.change=="DOWN"))
length(which(loadings.sig$pc2.change=="none"))

# Write significant loadings to file for later use.
write.csv(loadings.sig, "sig_loadings.csv")

# Merge significant PC1 and PC2 loadings with raw data
# Note: use missing values corrected data
pca.sig.vars <- merge(dat2, loadings.sig, by="row.names")


# use the "arrange" function in the plyr package to order
library(plyr)
pca.sig.vars <- arrange(pca.sig.vars, pc1.change, pc2.change)

# Re-assign row names and delete "Row.names" column
row.names(pca.sig.vars) <- pca.sig.vars$Row.names
pca.sig.vars$Row.names <- NULL

# Write the results to file for later use.
write.csv(pca.sig.vars, "dat_sig_loadings.csv")

# end

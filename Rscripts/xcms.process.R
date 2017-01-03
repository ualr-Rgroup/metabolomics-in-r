#############################################################
# Processing LCMS data with XCMS in R
#############################################################

# set working directory
setwd("~/R/xcms/rawdata")

# load XCMS library
library(xcms)

# LCMS data files must be saved in cdf format
# PUT ALL RAW DATA FILES IN A SUB-FOLDER CALLED "cdf"

# set path to raw data files
cdffiles <-list.files("./cdf", recursive=T, full=T)
cdffiles   # view list of raw data files
length(cdffiles)  # number of raw data files

#############################################################
# DATA PROCESSING PIPELINE FOR XCMS
# xcmsSet() is the primary function of XCMS; 
# it creates a peak table for each sample
# the following parameters can be changed
# method = "matchedFilter" (default) or "centWave"
# fwhm = 30 (default)
# max = 5 (default)
# snthresh = 10 (default)
# step = 0.1 (default)
# steps = 2 (default)
# mzdiff = 0.8 (default)
# profmethod = "bin" (default), "binlin"

# Step 1. Create xcmsSet object
xset <- xcmsSet(cdffiles, 
                method = "matchedFilter", 
                fwhm = 30, 
                max = 5,
                snthresh = 20, 
                step = 0.5, 
                steps = 2 , 
                mzdiff = 0.8,
                profmethod = "bin")

# Step 2. Peak Alignment
xsg <- group(xset, minfrac=c(0.5))

# Step 3. Retention Time Correction
xsg2 <- retcor(xsg, method="obiwarp", plottype="deviation")

# Step 4. Regroup and change bandwidth as desired
xsg3 <- group(xsg2, bw=10)

# Step 5. Fill in missing peaks
xsg4 <- fillPeaks(xsg3)

# get peak intensity matrix
results <- groupval(xsg4, "medret", "into")

# write results to file
# THIS FILE CAN BE USED AS INPUT FOR PCA, PLS, etc.
write.csv(results, file="xcmsresults.csv")

#############################################################
# Additional information

# Number of missing peaks per row (value == 0)
zeroval <- apply(results,1,function(x)sum(x == 0))

# number of rows with at least one missing peak
length(which(zeroval > 0))

# number of rows with at least 10 missing peaks
length(which(zeroval >= 10))

# number of zero values in the peak table
sum(zeroval)

# Fraction of missing values in data matrix
round(sum(zeroval) / (nrow(results) * ncol(results)),3)

# Output table summary to file
sink("summary.txt")
cat("Results Summary\n")
cat("\n")
cat("(*) changed from default\n")
cat("\n")
cat("method = matchedFilter\n")
cat("fwhm = 30\n")
cat("max = 5\n")
cat("snthresh = 20 (*)\n")
cat("step = 0.5 (*)\n")
cat("steps = 2\n")
cat("mzdiff = 0.8\n")
cat("profmethod = bin\n")
cat("\n")
cat("Number of variables\n")
nrow(results)
cat("\n")
cat("Number of variables with at least one missing value\n")
length(which(zeroval > 0))
cat("\n")
cat("Number of variables with at least 10 missing values\n")
length(which(zeroval >= 10))
cat("\n")
cat("Total number of missing values\n")
sum(zeroval)
cat("\n")
cat("Fraction of missing values in the data table\n")
round(sum(zeroval) / (nrow(results) * ncol(results)),3)
sink()

# end

###########################################################
# Script for making a volcano plot
###########################################################

# This script will produce a volcano plot from XCMS data.
# It will determine fold change and significance values
# for each variable in a two-group comparison and plot the 
# results as a volcano plot using the ggplot2 package.

# The following file needs to be placed in the working directory:
# xcmsresults.csv

# Refer to the xcms.process.R file to see how the data were generated.

# Set working directory
setwd("~/R/volcano/")

# Import data
data <- read.csv("xcmsresults.csv", header=T, row.names=1, stringsAsFactors=F)

# Function to replace zeroes with a small value
# This function will find minimum value greater than zero and divide by two
replacezero = function(x) "[<-"(x, !x | is.na(x), min(x[x > 0], na.rm = TRUE) / 2)

# Sum normalization function - apply to each column
sumnorm <- function(z) {
  colsum <- apply(z, 2, sum)
  rv <- sweep(z, 2, colsum, "/")
  return(rv)
}

# Subset data for 2-Group Comparison

# SET 1: GREEN FRUITS
dat1 <- data[,1:18]

# Apply replacezero function across rows
dat2 <- as.data.frame(t(apply(dat1, 1, replacezero)))

# Sum normalize data
set1.norm <- sumnorm(dat2)

# Log transform data
logd1.norm = log2(set1.norm)

# Perform t-test to get p-values 
pvalue <- apply(logd1.norm, 1, function(x) { t.test(x[1:8], x[9:18])$p.value } )

# Apply Benjamini-Hochberg correction (FDR)
p.BHcorr <- p.adjust(pvalue, method = "BH")

# Calculate negative log of FDR-adjusted p value
p.BH.nlog <- -log10(p.BHcorr)

# Calculate row-wise group means for sum normalized data
hp <- apply(set1.norm[1:8], 1, FUN=mean)
Man <- apply(set1.norm[9:18], 1, FUN=mean)

# Calculate log2 Fold Change from group means
FC <- hp/Man
log2FC <- log(FC,2)

# Make new data frame with volcano plot data
volcano.dat1 <- data.frame(hp, Man, pvalue, p.BHcorr, p.BH.nlog, FC, log2FC)

# Sort in ascending order by FDR
volcano.dat1 <- volcano.dat1[order(p.BHcorr),]

# make a simple volcano plot
plot(volcano.dat1$log2FC, volcano.dat1$p.BH.nlog)

# add cutoff lines to show significant variables
abline(v=2, col="red")
abline(v=-2, col="red")
abline(h=2, col="red")

# Create new column based on p.BHcorr and log2FC values
## log2FC > 2 (FC>4) & p.BH.nlog > 2 (p<0.01), then "UP";
## log2FC < -2 (FC>-2) & p.BH.nlog > 2 (p<0.01), then "DOWN";
## otherwise "nochange"
volcano.dat1$change <- ifelse(volcano.dat1$log2FC>2 & volcano.dat1$p.BH.nlog>2,"UP",
                              ifelse(volcano.dat1$log2FC< -2 & volcano.dat1$p.BH.nlog>2,"DOWN",
                                     "nochange"))

# Create vector for developmental stage
stage <- rep("Green Fruits", nrow(volcano.dat1))

# Add stage vector as new column
volcano.dat1$stage <- stage

# create new column with rownames for text labels
volcano.dat1$feature <- rownames(volcano.dat1)

# write csv file of volcano plot data
write.csv(volcano.dat1, "volcano_data_green.csv")

# Determine number of UP and DOWN bins
nrow(subset(volcano.dat1, change=="UP"))
nrow(subset(volcano.dat1, change=="DOWN"))
nrow(subset(volcano.dat1, change=="nochange"))

# subset data to only include UP or DOWN variables
v.sub1 <- subset(volcano.dat1, change=="UP" | change=="DOWN")

# sort in descending order by log2FC
v.sub1 <- v.sub1[order(-v.sub1$log2FC),]

# write csv file of significant variables
write.csv(v.sub1, "volcano_significant_green.csv")

# SET 2: RED FRUITS

# Select columns for RED FRUITS
dat3 <- data[,19:38]

# Apply replacezero function across rows
dat4 <- as.data.frame(t(apply(dat3, 1, replacezero)))

# Sum normalize data
set2.norm <- sumnorm(dat4)

# Log transform
logd2.norm = log2(set2.norm)

# Perform t-test to get p-values 
pvalue <- apply(logd2.norm, 1, function(x) { t.test(x[1:10], x[11:20])$p.value } )

# Apply Benjamini-Hochberg correction (FDR)
p.BHcorr <- p.adjust(pvalue, method = "BH")

# Calculate negative log of FDR-adjusted p value
p.BH.nlog <- -log10(p.BHcorr)

# Calculate row-wise group means for sum normalized data
hp <- apply(set2.norm[1:10], 1, FUN=mean)
Man <- apply(set2.norm[11:20], 1, FUN=mean)

# Calculate log2 Fold Change from group means
FC <- hp/Man
log2FC <- log(FC,2)

# Make new data frame with volcano plot data
volcano.dat2 <- data.frame(hp, Man, pvalue, p.BHcorr, p.BH.nlog, FC, log2FC)

# Sort in ascending order by FDR
volcano.dat2 <- volcano.dat2[order(p.BHcorr),]

# make a simple volcano plot
plot(volcano.dat2$log2FC, volcano.dat2$p.BH.nlog)

# add cutoff lines to show significan variables
abline(v=2, col="red")
abline(v=-2, col="red")
abline(h=2, col="red")

# Create new column based on p.BHcorr and log2FC values
## log2FC > 2 (FC>4) & p.BH.nlog > 2 (p<0.01), then "UP";
## log2FC < -2 (FC>-2) & p.BH.nlog > 2 (p<0.01), then "DOWN";
## otherwise "nochange"
volcano.dat2$change <- ifelse(volcano.dat2$log2FC>2 & volcano.dat2$p.BH.nlog>2,"UP",
                         ifelse(volcano.dat2$log2FC< -2 & volcano.dat2$p.BH.nlog>2,"DOWN",
                                "nochange"))

# Create vector for developmental stage
stage <- rep("Red Fruits", nrow(volcano.dat2))
volcano.dat2$stage <- stage

# create new column with rownames for text labels
volcano.dat2$feature <- rownames(volcano.dat2)

# write csv file of volcano plot data
write.csv(volcano.dat2, "volcano_data_red.csv")

# Determine number of UP and DOWN bins
nrow(subset(volcano.dat2, change=="UP"))
nrow(subset(volcano.dat2, change=="DOWN"))
nrow(subset(volcano.dat2, change=="nochange"))

# subset data to only include UP or DOWN variables
v.sub2 <- subset(volcano.dat2, change=="UP" | change=="DOWN")

# sort in descending order by log2FC
v.sub2 <- v.sub2[order(-v.sub2$log2FC),]

# write csv file of significant variables
write.csv(v.sub2, "volcano_significant_red.csv")

# MERGE VOLCANO DATA FOR GREEN AND RED FRUITS
volcano.alldata <- rbind(volcano.dat1, volcano.dat2)

#########################################################
# Make volcano plot in ggplot2
#########################################################

library(ggplot2)

my.theme <- theme(axis.text = element_text(colour="black", size=15),
                  text = element_text(size=16),
                  title = element_text(size=16, face="bold", vjust=2),
                  panel.background = element_rect(fill = 'gray95',
                                                  colour = "black", 
                                                  size=1),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=1.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line(),
                  panel.grid.major = element_line(colour = "gray65"),
                  panel.grid.minor = element_line(colour = "white"),
                  legend.title = element_text(size=16, face="bold"),
                  legend.text = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size=14, face="bold"),
                  strip.background = element_rect(fill="gray90", colour="black", size=1))

# Make data frame called "plottext" for letters on graph
xpos <- c(-5.5, 5.5, -5.5, 5.5)
ypos <- c(7.5, 7.5, 8.6, 8.6)
label <- c("Higher in Manapal","Higher in hp2-dg",
              "Higher in Manapal","Higher in hp2-dg")
grp <- c("Green Fruits","Green Fruits","Red Fruits", "Red Fruits")
plottext <- data.frame(xpos, ypos, Label=label, stage=grp)

# make volcano plots
g1 <-
  ggplot(volcano.alldata, aes(log2FC, p.BH.nlog)) +
  geom_hline(yintercept=2, size=0.8, colour="#B03060") +
  geom_vline(xintercept=2, size=0.8, colour="#B03060") + 
  geom_vline(xintercept=-2, size=0.8, colour="#B03060") +
  geom_point(size=4.5, shape=21, aes(fill=change)) +
  scale_fill_manual(name='Change',values=c("#228B22", "#FFF68F", "#FF3030"),
                    guide=FALSE) +
  ggtitle("Volcano Plot of Significant Features") +
  xlab("Log2 Fold Change") +
  ylab("Significance (-logP)") +
  geom_text(data=plottext, aes(xpos, ypos, label=Label), 
            size=5, color=c("#228B22","#FF3030","#228B22","#FF3030"),
            fontface="bold", show.legend=FALSE) + 
  facet_wrap(~stage, nrow=1) +
  my.theme

# display plot
g1

# Make png file
png(file="volcano_plots.png", height=3000, width=5000, res=500)
g1
dev.off()

#end

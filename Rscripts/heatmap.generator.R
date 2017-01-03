######################################################
# Make a heatmap using heatmap.2 from gplot2 package
######################################################

# This script will produce a heatmap using the heatmap.2 
# function in the gplots2 package.  Refer to the PCA.process.R 
# file to see how these variables were identified.

# The following file needs to be placed in the working directory:
# dat_sig_loadings.csv

# Set working directory
setwd("~/R/heatmap/")

# Import data
data <- read.csv("dat_sig_loadings.csv", header=T)

# Subset to include only data columns
data <- data[, c(1:39)]

# Make a vector from "X" column for Row Labels
Feature <- as.vector(data$X)

# Log Transform the data (base 2 log)
ldat1 <- log(data[, 2:39], 2)

# Function for Pareto Scaling
paretoscale <- function(z) {
  rowmean <- apply(z, 1, mean) # row means
  rowsd <- apply(z, 1, sd)  # row standard deviation
  rowsqrtsd <- sqrt(rowsd) # sqrt of sd
  rv <- sweep(z, 1, rowmean,"-")  # mean center
  rv <- sweep(rv, 1, rowsqrtsd, "/")  # dividing by sqrtsd
  return(rv)
}

# Pareto scale log transformed data
normdata <- paretoscale(ldat1)

# Load libraries
library(gplots)
library(RColorBrewer)

# color palette for heatmap
my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 40)

# function to add legend in desired position
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

# make heatmap and save as a .png file
png(file="heatmap.png", height=3800, width=5500, res=600)
par(mar = c(5, 4, 1.4, 0.2))

heatmap.2(as.matrix(normdata), #input data as matrix
          trace="none", #remove lines on cells
          density.info="none", #remove lines on legend
          colsep=1:ncol(normdata), #column border separator
          rowsep=1:nrow(normdata), #row border separator
          scale="row", #row wise centering
          col=my_palette, #colors
          keysize=1, #legend size
          sepwidth=c(.01,.01), #line separator width
          sepcolor="gray25", #line  separator color
          ColSideColors=c(rep("darkgreen",8), rep("chartreuse3",10),
                          rep("darkred",10), rep("coral1", 10)),
          cexCol=1.2, #column label font size
          cexRow=0.8, #row label font size
          margins=c(4,8), #plot margins
          srtCol=90, #angle column labels
          main="Heatmap of significant features from PCA loadings",
          labRow=Feature #row labels from feature list
)

par(cex.main=1.3) # title font
par(lend = 1) # square line ends for the color legend
add_legend(0.82,1.05, # legend position 
           legend = c("MG", "hG", "MR", "hR"), # category labels
           col = c("chartreuse3", "darkgreen", "coral1", "darkred"),  # color key
           bty= "n",           # line style
           lty= 1,             # line style
           lwd = 10            # line width
)

dev.off()

# end

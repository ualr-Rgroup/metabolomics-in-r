###############################################################
# Make PCA scores and loadings plots with ggplot2
###############################################################

# This script will produce a scores plot and loadings plot from 
# the output of PCA using the ggplot2 package.  Refer to the 
# PCA.process.R file to see how the data were generated.

# The following files need to be placed in the working directory:
# pca_scores.csv
# pca_scree.csv
# pca_loadings.csv

# Set working directory
setwd("~/R/pca/")

######################
# Scores plot
######################

# import scores matrix
data <- read.csv("pca_scores.csv", header=T)

# subset to include only PC1 to PC3 scores
data <- data[, c(1:4)]

# look at first few rows
data[1:6,1:4]

# Get variance percentages for first 3 PC's
screedat <- read.csv("pca_scree.csv", header=T)
var1 <- round(screedat[2,2:4] * 100, 1)

# Change "X" column to "Sample"
colnames(data)[colnames(data)=="X"] <- "Sample"

# Add Group label to the data frame
Group <- c(rep("hp2-dg green", 8),
           rep("Manapal green", 10),
           rep("hp2-dg red", 10),
           rep("Manapal red", 10))

data <- cbind(data, Group)

# install ggplot2 (can skip if already installed)
install.packages("ggplot2")

# load ggplot2
library(ggplot2)

# Make custom theme for ggplot
my.theme <- theme(axis.text = element_text(colour="black", size=15),
                  text = element_text(size=16),
                  title = element_text(size=18, face="bold", vjust=2),
                  panel.background = element_rect(fill = 'gray99',
                                                  colour = "black", 
                                                  size=0.5),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=1.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line(),
                  panel.grid.major = element_line(colour = "gray40", linetype="dotted"),
                  panel.grid.minor = element_line(colour = "gray40", linetype="dashed"),
                  legend.justification=c(1,1),
                  legend.position=c(1,1),
                  legend.title=element_blank(),
                  legend.text = element_text(size = 14))

# check variances for PC1 and PC2
var1

# Calculate 95% ellipse values for PC1,PC2
library(ellipse)
centroids <- aggregate(cbind(PC1,PC2)~Group,data,mean)
conf.rgn1  <- do.call(rbind,lapply(unique(data$Group),function(t)
  data.frame(Group=as.character(t),
             ellipse(cov(data[data$Group==t,2:3]),
                     centre=as.matrix(centroids[t,2:3]),
                     level=0.95),
             stringsAsFactors=FALSE)))

# make plot for PC1 vs. PC2
g1 <-
  ggplot(data, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "gray50") +
  geom_vline(xintercept = 0, colour = "gray50") +
  geom_polygon(data=conf.rgn1,
               aes(fill=Group), colour="black", alpha = 0.2, 
               linetype="blank", show.legend=FALSE) +
  geom_point(aes(shape=Group, bg=Group), colour="black", size=4.5) +
  geom_text(aes(label=data$Sample), colour="black", 
            size=4, hjust=-0.25, vjust=1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values=c(21,22,23,24)) +
  ggtitle("PCA Scores Plot") +
  xlab("PC 1 (31.3%)") +
  ylab("PC 2 (23.5%)") +
  my.theme +
  theme(legend.position=c(0.30,0.25))
  
# draw scores plot
g1

# save as png file
png(file="scores.plot.png", height=2400, width=2800, res=350)
g1
dev.off()

######################
# Loadings plot
######################

# Import loadings matrix
loadings <- read.csv("pca_loadings.csv", header=T)

# subset to include only PC1 to PC3 loadings
loadings <- loadings[, c(1:4)]

# look at first few rows
loadings[1:6,1:4]

# Change "X" column to "Variable"
colnames(loadings)[colnames(loadings)=="X"] <- "Variable"

# make a quick loadings plot
plot(loadings$PC1, loadings$PC2)

# add lines for cutoff values
abline(v=0.09, col="red")
abline(v=-0.09, col="red")
abline(h=0.09, col="red")
abline(h=-0.09, col="red")

# Create new column based on PC1 loadings
loadings$pc1.change <-
  ifelse(loadings$PC1 > 0.09,"UP",
         ifelse(loadings$PC1 < -0.09,"DOWN",
                "zeit"))

# Create new column based on PC2 loadings
loadings$pc2.change <-
  ifelse(loadings$PC2 > 0.09,"UP",
         ifelse(loadings$PC2 < -0.09,"DOWN",
                "zeit"))

# Create label column for PC1 loadings
loadings$pc1.label <-
  ifelse(loadings$PC1 > 0.09, as.character(loadings$Variable),
         ifelse(loadings$PC1 < -0.09, as.character(loadings$Variable),
                "null"))

# Create label column for PC2 loadings
loadings$pc2.label <-
  ifelse(loadings$PC2 > 0.09, as.character(loadings$Variable),
         ifelse(loadings$PC2 < -0.09, as.character(loadings$Variable),
                "null"))

# subset significant loadings
loadings.sig <- subset(loadings,
                       PC1 > 0.09 | PC1 < -0.09 |
                         PC2 > 0.09 | PC2 < -0.09)

library(plyr)

# use the "arrange" function in the plyr package to sort
loadings.sig <- arrange(loadings.sig, pc1.change, pc2.change)

# Write the results to file 
write.csv(loadings.sig, "significant_loadings.csv", row.names=F)

# make loadings plot
g2 <-
  ggplot(loadings, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "gray40") +
  geom_vline(xintercept = 0, colour = "gray40") +
  geom_point(size=2.5, pch=21, color="gray20", bg="khaki1") +
  stat_ellipse(level=0.15, colour="gray40", linetype="dashed", type="euclid") +
  geom_point(data=subset(loadings, pc1.change=="UP"),
             size=4, pch=21, color="black", bg="blue") +
  geom_point(data=subset(loadings, pc1.change=="DOWN"),
             size=4, pch=22, color="black", bg="orange") +
  geom_point(data=subset(loadings, pc2.change=="UP"),
             size=4, pch=23, color="black", bg="green") +
  geom_point(data=subset(loadings, pc2.change=="DOWN"),
             size=4, pch=24, color="black", bg="red") +
  scale_x_continuous(limits = c(-0.18, 0.16)) +
  scale_y_continuous(limits = c(-0.16, 0.16)) +
  ggtitle("PCA Loadings Plot") +
  my.theme

# draw loadings plot
g2

# add text annotations using the grid package
library(grid)

PC1.pos <- grobTree(textGrob("Positively \n correlated \n with PC1",
                             x=0.82, y=0.22, gp=gpar(col="blue", fontsize=14, fontface="bold")))
PC1.neg <- grobTree(textGrob("Negatively \n correlated \n with PC1",
                             x=0.15, y=0.7, gp=gpar(col="orange", fontsize=14, fontface="bold")))
PC2.pos <- grobTree(textGrob("Positively \n correlated \n with PC2",
                             x=0.63, y=0.88, gp=gpar(col="green", fontsize=14, fontface="bold")))
PC2.neg <- grobTree(textGrob("Negatively \n correlated \n with PC2",
                             x=0.17, y=0.1, gp=gpar(col="red", fontsize=14, fontface="bold")))

g2a <-
  g2 +
  annotation_custom(PC1.pos) +
  annotation_custom(PC1.neg) +
  annotation_custom(PC2.pos) +
  annotation_custom(PC2.neg)

g2a

# save as png file
png(file="loadings.plot.png", height=2400, width=2800, res=350)
g2a
dev.off()

# end

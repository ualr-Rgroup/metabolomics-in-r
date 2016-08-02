######################################################
# Make a boxplot with ggplot2 package
######################################################

# This script will produce a multipanel boxplot using the ggplot2 package.

# The following file needs to be placed in the working directory:
# dat_sig_loadings.csv

# Set working directory
setwd("~/R/boxplot/")

# Import data
data <- read.csv("dat_sig_loadings.csv", header=T)

# Subset to include only data columns
data <- data[, c(1:39)]

# Change "X" column to "Variable"
colnames(data)[colnames(data)=="X"] <- "Variable"

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

# PARETO SCALE LOG TRANSFORMED DATA
normdata <- paretoscale(ldat1)

# Transpose log normalized data
normdata.t <- as.data.frame(t(normdata))

# Rename columns by Variable name
colnames(normdata.t) <- data$Variable

# Create "Group" vector
Group <- c(rep("hp-2dg Green", 8),
           rep("Manapal Green", 10),
           rep("hp-2dg Red", 10),
           rep("Manapal Red", 10))

# Add Group vector to data frame
normdata.t <- cbind(normdata.t, Group)

# Subset variables of interest
subnormdata.t <- subset(normdata.t, 
                        select=c("495.2/2285",
                                 "529.8/992",
                                 "805.2/2198",
                                 "1136.4/2038",
                                 "Group"))

# Load reshape2 library
library(reshape2)

# Melt data into long format for ggplot2
subnormdata.m <- melt(subnormdata.t, id.var = "Group")

# Load ggplot2 library
library(ggplot2)

# create custom theme
my.theme <- theme(axis.text.x = element_text(colour="black", size=12),
                   axis.text.y = element_text(colour="black", size=14),
                   title = element_text(size=12, vjust=2, face="bold"),
                   panel.background = element_rect(fill = 'gray95',
                                                   colour = "black", 
                                                   size=0.5),
                   axis.title.x =  element_text(size=14, vjust=-0.45, face="bold"),
                   axis.title.y = element_text(size=14, vjust=1),
                   axis.ticks = element_blank(),
                   axis.line = element_line(),
                   panel.grid.major = element_line(colour = "gray60", linetype="solid"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major.x = element_blank(),
                   legend.justification=c(1,1),
                   legend.background= element_rect(fill = "white"),
                   legend.title=element_text(size=14),
                   legend.text = element_text(size = 12),
                   strip.text = element_text(size=12, face="bold"),
                   strip.background = element_rect(fill="gray90", colour="black"))

# Make data frame called "letters" for letters on graph
x.letter <- c(0.75,4.25, 0.75, 4.25)
y.letter <- c(3, 3, 3, 3)
label.letter <- c("A","B","C","D")
grp <- colnames(subnormdata.t[1:4])
letters <- data.frame(x=x.letter, value=y.letter, 
                      Label=label.letter, variable=grp)

# make boxplot plot of selected variables with error bars
bp2 <-
  ggplot(subnormdata.m, aes(x=Group, y=value)) +
  stat_boxplot(geom ='errorbar', width = 0.4, size =  0.5) + 
  geom_boxplot(aes(fill=Group), size=0.5, outlier.shape=16, outlier.size=2) +
  scale_fill_manual(values=c("darkgreen", "darkred", "chartreuse3", "coral1")) +
  xlab("") +
  ylab("Normalized Intensity\n") +
  facet_wrap(~variable, nrow=2) +
  ggtitle("Boxplot of Significant Peaks") +
  geom_text(data=letters, aes(x=x, y=value, label=Label), 
            size=5, color="black", fontface="bold", show.legend=FALSE) + 
  scale_x_discrete(labels=c("hG", "hR", "MG", "MR")) +
  my.theme

# display plot
bp2

# save plot as .png file
png(file="boxplot.png", height=3200, width=4000, res=600)
bp2
dev.off()

# end

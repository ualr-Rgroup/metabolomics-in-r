########################################################
# Script for making TIC plots from a group of files
########################################################

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

# create an xcmsRaw object from a data file
xr1 <- xcmsRaw(cdffiles[6])  # sample #6
xr1@filepath   # check file name
xr1   # view properties of raw data file

# Make a quick TIC and BPC plot
plotTIC(xr1)  # TIC plot
plotChrom(xr1, base=TRUE) # BPC plot

# create additional xcmsRaw objects
xr2 <- xcmsRaw(cdffiles[16]) # sample 16
xr3 <- xcmsRaw(cdffiles[26]) # sample 26
xr4 <- xcmsRaw(cdffiles[36]) # sample 36

# make data frames with TIC data

# xr1
t1 <- xr1@scantime # time (sec)
t1 <- t1 / 60  # time (min)
int1 <- xr1@tic # intensity
s1 <- rep("HG5", 1896)
g1 <- rep("hp-2dg Green", 1896)
tic1 <- data.frame(time=t1,intensity=int1, Sample=s1, Group=g1)

# xr2
t2 <- xr2@scantime # time (sec)
t2 <- t2 / 60  # time (min)
int2 <- xr2@tic # intensity
s2 <- rep("HR5", 1894)
g2 <- rep("hp-2dg Red", 1894)
tic2 <- data.frame(time=t2,intensity=int2, Sample=s2, Group=g2)

# xr3
t3 <- xr3@scantime # time (sec)
t3 <- t3 / 60  # time (min)
int3 <- xr3@tic # intensity
s3 <- rep("MG5", 1893)
g3 <- rep("Manapal Green", 1893)
tic3 <- data.frame(time=t3,intensity=int3, Sample=s3, Group=g3)

# xr4
t4 <- xr4@scantime # time (sec)
t4 <- t4 / 60  # time (min)
int4 <- xr4@tic # intensity
s4 <- rep("MR5", 1893)
g4 <- rep("Manapal Red", 1893)
tic4 <- data.frame(time=t4,intensity=int4, Sample=s4, Group=g4)

# Combine all data into 1 data frame
tic.data <- rbind(tic1, tic2, tic3, tic4)
head(tic.data)

# Make TIC plot using ggplot2 package

# install ggplot2 (can skip if already installed)
install.packages("ggplot2")

library(ggplot2)

# make customized theme
my.theme <- theme(axis.text = element_text(colour="black", size=15),
                  text = element_text(size=12),
                  title = element_text(size=16, face="plain", vjust=2),
                  panel.background = element_rect(fill = "white",
                                                  colour = "black", 
                                                  size=0.5),
                  axis.title.x=  element_text(size=16, face="plain", vjust=-0.45),
                  axis.title.y = element_text(size=16, face="plain", vjust=1.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line(),
                  panel.grid.major = element_line(colour = "gray65"),
                  panel.grid.minor = element_line(colour = "gray85"),
                  legend.title = element_text(size=14, face="plain"),
                  legend.text = element_text(size = 14),
                  legend.key = element_rect(),
                  legend.position=c(.12,.55),
                  strip.text = element_text(size=16),
                  strip.background = element_rect(colour="black", fill="gray90", size=0.5))

# Make data frame called "letters" for letters on graph
x.letter <- c(rep(44,4))
y.letter <- c(600000, 500000, 250000, 150000)
label.letter <- c("A","B","C","D")
grp <- c("hp-2dg Green", "hp-2dg Red",
         "Manapal Green", "Manapal Red")
letters <- data.frame(time=x.letter, intensity=y.letter, 
                      Label=label.letter, Group=grp)

# make plot
plot1 <-
  ggplot(tic.data, aes(x=time, y=intensity, color=Group)) +
  geom_line(size=0.6) +
  scale_color_manual(values=c("darkgreen", "darkred", "chartreuse3", "coral1")) +
  xlab("Time (minutes)") +
  ylab("Intensity") + 
  ggtitle("Representative TICs of Green and Red Fruits") +
  geom_text(data=letters, aes(x=time, y=intensity, label=Label), 
            size=8, color="black", fontface="bold", show.legend=FALSE) + 
  facet_wrap( ~ Group, scales="free_y", ncol=2) +
  my.theme  +
  theme(legend.position="none")

# display plot
plot1

# write plot to file
png(file="./my_tic/tic.multiplot.png", height=3000, width=4800, res=400)
plot1
dev.off()

# end

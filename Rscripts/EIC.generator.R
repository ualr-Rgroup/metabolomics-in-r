#################################################
# Script for making EICs from a group of files
#################################################

# This script will generate overlapping extracted ion chromatograms (EICs)
# from a group of LCMS raw data files.

# The packages xcms and ggplot2 are required.

# Put all data files in a sub-folder called "cdf"
# Note: do not put them in separate subfolders

# set working directory
setwd("~/R/eic/")

# load xcms package
library(xcms)

# set path to data files
cdffiles <-list.files("./cdf", recursive = TRUE, full=T)
cdffiles   # view list of data files
length(cdffiles)  # number of data files

# create a list of xcmsRaw objects
dat.raw <- lapply(cdffiles, xcmsRaw)

# set upper and lower time limits for EICs
t.start <- 1
t.end <- 2690

# set upper and lower mass limits for EICs
m.start <- 804.5
m.end <- 805.5

# create vectors for time and mass limits
trange <- c(t.start,t.end)
mrange <- c(m.start,m.end)

# create title for ggplot2
title <- paste("Mass Range", mrange[1], "-", mrange[2])

# make EICs for all files
eicraw <- lapply(dat.raw, plotEIC, mzrange=mrange, rtrange=trange)

# pull EIC results into a data frame
eic.df <- do.call(rbind, lapply(eicraw, data.frame))

# re-set column names
colnames(eic.df) <- c("Seconds", "Intensity")

# create column with time in minutes
eic.df$Minutes <- eic.df$Seconds / 60

# re-order columns
eic.df <- eic.df[c("Seconds", "Minutes", "Intensity")]

# get number of rows for each EIC
eic.length <- do.call(rbind, lapply(eicraw, function(x) nrow(x)))

# make a vector of filenames for each EIC
Path <- c(rep(cdffiles[1], eic.length[1,]),
          rep(cdffiles[2], eic.length[2,]),
          rep(cdffiles[3], eic.length[3,]),
          rep(cdffiles[4], eic.length[4,]),
          rep(cdffiles[5], eic.length[5,]),
          rep(cdffiles[6], eic.length[6,]),
          rep(cdffiles[7], eic.length[7,]),
          rep(cdffiles[8], eic.length[8,]),
          rep(cdffiles[9], eic.length[9,]),
          rep(cdffiles[10], eic.length[10,]),
          rep(cdffiles[11], eic.length[11,]),
          rep(cdffiles[12], eic.length[12,]),
          rep(cdffiles[13], eic.length[13,]),
          rep(cdffiles[14], eic.length[14,]),
          rep(cdffiles[15], eic.length[15,]),
          rep(cdffiles[16], eic.length[16,]),
          rep(cdffiles[17], eic.length[17,]),
          rep(cdffiles[18], eic.length[18,]),
          rep(cdffiles[19], eic.length[19,]),
          rep(cdffiles[20], eic.length[20,]),
          rep(cdffiles[21], eic.length[21,]),
          rep(cdffiles[22], eic.length[22,]),
          rep(cdffiles[23], eic.length[23,]),
          rep(cdffiles[24], eic.length[24,]),
          rep(cdffiles[25], eic.length[25,]),
          rep(cdffiles[26], eic.length[26,]),
          rep(cdffiles[27], eic.length[27,]),
          rep(cdffiles[28], eic.length[28,]),
          rep(cdffiles[29], eic.length[29,]),
          rep(cdffiles[30], eic.length[30,]),
          rep(cdffiles[31], eic.length[31,]),
          rep(cdffiles[32], eic.length[32,]),
          rep(cdffiles[33], eic.length[33,]),
          rep(cdffiles[34], eic.length[34,]),
          rep(cdffiles[35], eic.length[35,]),
          rep(cdffiles[36], eic.length[36,]),
          rep(cdffiles[37], eic.length[37,]),
          rep(cdffiles[38], eic.length[38,]),
          rep(cdffiles[39], eic.length[39,]),
          rep(cdffiles[40], eic.length[40,]))

# add file names to EIC data frame
eic.df <- cbind(eic.df, Path)

# make a vector of Sample names
Sample <- substring(eic.df$Path, 7)
Sample <- gsub(".cdf", "", Sample)

# add Sample names to data frame
eic.df <- cbind(eic.df, Sample)

# use plyr to get number of rows per group
library(plyr)
res <- count(eic.df, "Sample")

# make a vector of Group names
Group <- c(rep("hp-2dg Green Fruits", sum(res$freq[1:10])),
           rep("hp-2dg Red Fruits", sum(res$freq[11:20])),
           rep("Manapal Green Fruits", sum(res$freq[21:30])),
           rep("Manapal Red Fruits", sum(res$freq[31:40])))

# add Group names to data frame
eic.df <- cbind(eic.df, Group)

# make data frame called "letters" for letters on graph
x.letter <- c(rep(650,4))
y.letter <- c(rep(1000,4))
label.letter <- c("A","B","C","D")
grp <- c("hp-2dg Green Fruits", "hp-2dg Red Fruits",
         "Manapal Green Fruits", "Manapal Red Fruits")
letters <- data.frame(Minutes=x.letter, Intensity=y.letter, 
                      Label=label.letter, Group=grp)

# make EIC plots in ggplot2
library(ggplot2)

# make customized graph theme
mytheme <- theme(axis.text = element_text(colour="black", size=15),
                 text = element_text(size=12),
                 title = element_text(size=14, face="bold", vjust=2),
                 panel.background = element_rect(fill = 'white',
                                                 colour = "black", 
                                                 size=1),
                 axis.title.x=  element_text(size=16, face="plain", vjust=-0.45),
                 axis.title.y = element_text(size=16, face="plain", vjust=1.2),
                 axis.ticks = element_line(colour="black"),
                 axis.line = element_line(),
                 panel.grid.major = element_line(colour = "gray85", size=0.8),
                 panel.grid.minor = element_line(colour = "gray85"),
                 legend.title = element_text(size=12, face="plain"),
                 legend.text = element_text(size = 12),
                 legend.key = element_rect(),
                 strip.text = element_text(size=14),
                 strip.background = element_rect(fill="gray90", colour="black"))

# make graph
g1 <-
  ggplot(eic.df, aes(Seconds, Intensity, color=Group)) +
  geom_line(size=0.6) +
  scale_color_manual(values=c("darkgreen", "darkred", "chartreuse3", "coral1")) +
  xlab("Time (seconds)") +
  xlim(2000, 2400) +
  ylim(0, 15000) +
  ggtitle(title) +
  facet_wrap(~Group) +
  geom_text(data=letters, aes(x=Minutes, y=Intensity, label=Label), 
            size=6, color="black", fontface="bold", show.legend=FALSE) + 
  mytheme +
  theme(legend.position="none")
  
# display plot
g1

# write output to file
png(file="eic.805.png", height=2500, width=3000, res=400)
g1
dev.off()

# end

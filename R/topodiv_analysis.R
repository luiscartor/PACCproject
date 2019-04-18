#topodiv_analysis.R
# on 18th April 2019
# by Luis
# analyze topodiv data extracted from topodivextract.R and plot results
##################################################################################################################


library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)



# 1. INPUTS
OUTtopodivfolder <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/'
INtopodivtable <- 'C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\topodiv\\PAstopodivtable_tillMSR.txt'

INgadmfolder <- 'C:\\Users\\lcarrasc\\Documents\\research\\chinese_infrastructures\\datasets\\GADM'
INgadmfile <- 'gadm36_0_simplify'
  
# 2. READ DATA
tdivtable <- read.table(INtopodivtable,header = TRUE)
gadm <- readOGR(INgadmfolder, INgadmfile)

# 3. ANALYSIS
# Creates column with difference between PAs tdiv mean and outside
tdivtable$difpas_out <- (tdivtable$pamean - tdivtable$outpamean)/1000
tdivtable$difpas_coun <- (tdivtable$pamean - tdivtable$counmean)/1000


# Add columns to gadm
colnames(tdivtable)[1] <- "GID_0"
gadm@data <- merge(gadm@data,tdivtable[,c("GID_0","difpas_out","difpas_coun")],all.x=TRUE)

# 4. PLOTS
ggplot(gadm, aes(long, lat, group=group,fill = gadm@data$difpas_out)) +
  geom_polygon() +
  #scale_fill_viridis(option = 'plasma')+ 
  ggtitle("Map of World")+ 
  theme_bw()

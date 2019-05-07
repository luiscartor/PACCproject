# globalneeds.R
# on May 6 2019
# by Luis
# maps the needs for increasing refugia, diversity or protconn by country
##################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(plyr)
library(scales)
library(broom)
library(mapproj)


# 1. INPUTS
OUTccvelfolder <- '/home/lcarrasco/Documents/research/protectedareas/globalneeds/'


INccveltable <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/PAsccveltable.txt'
INtopodivtable <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/PAstopodivtable.txt'
INconnectall <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/results/protconnbound_allyears_dis10.txt'


INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INgadmfile <- 'gadm36_0_simplify'
#INgadmfile <- 'gadm36_0_simplify_robinson_buff0'

INgdpfile <- '/home/lcarrasco/Documents/research/protectedareas/data/GDP/IMF_GDPPPP_Data.csv'

# 2. READ DATA
ccveltable <- read.table(INccveltable,header = TRUE)
tdivtable <- read.table(INtopodivtable,header = TRUE)
connectall <- read.table(INconnectall,header = TRUE)

gadm <- readOGR(INgadmfolder, INgadmfile)

#gdp <- read.csv2(INgdpfile, header = TRUE, sep=",", stringsAsFactors=FALSE)


# 3. ANALYSIS
needs <- data.frame()

needs$country <- ccveltable$country

needs$
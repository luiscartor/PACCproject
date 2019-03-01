# extractnetworks.R
# on 1st March 2019
# by Luis
# extract PAs networks and creates shapefiles to ingest to conefor in QGIS
##################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)

# 1. INPUTS
# Read country boundaries data (each piece of land is a different feature)
INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INpasfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA/'


# 2. READ DATA
gadm <-readOGR(INgadmfolder, 'gadm36_0_multipart')
pas <- readOGR(INpasfolder, 'WDPA_2011on_final')


# 3. MAIN ROUTINE

# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)


for (c in 1:length(countrynames)){
  
  # 3.1 Create First network dataset (ProtUnconnDesign; Saura 2018, appendix)
  
  
  
  
}





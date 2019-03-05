# extractnetworks.R
# on 1st March 2019
# by Luis
# extract PAs networks and creates shapefiles to ingest to conefor in QGIS
##################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)

# 1. INPUTS
# Read country boundaries data (each piece of land is a different feature)
INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INpasfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA/'

OUTfolder <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/networks/'

# 2. READ DATA
#gadm <- readOGR(INgadmfolder, 'gadm36_0_simplify')
gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')
gadmmulti <-readOGR(INgadmfolder, 'gadm36_0_multipart')
#pas <- readOGR(INpasfolder, 'WDPA_2011on_final')
pas <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_2011on_final')

# 3. MAIN ROUTINE

# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)


for (c in 1:length(countrynames)){
  
  # 3.1 Create First network dataset (ProtUnconnDesign; Saura 2018, appendix)
  # The first network uses all PAs within the country, with an attribute equal to area, all transboundary PAs,
  # with attribute equal zero, and all the land portions of the country.
  
  # 3.1.1 Extracts country pas and add area
  counpas <- subset(pas, ISO3==c)
  # Adds attribute (area) column
  counpas@data$attribute <- round(area(counpas),0)
  # Leaves only attribute column
  counpas <- counpas[,-(1:2)]
  
  # 3.1.2 Extract transboundary pas and add 0 attribute
  countrans <- subset(gadmtrans, GID_0==c)
  
  # Select PAs included in countrans
  transbound
  # But excluding the country PAs
  transbound <- subset(transbound, ISO3!=c)
  # Adds attribute column (equal to zero)
  transbound <- rep(0,nrow(transbound@data))
  
  
  # 3.1.3 Extract land portions and add 0 attribute
  counland <- subset(gadmmulti, GID_0==c)
  # Adds attribute column (equal to zero)
  counland@data$attribute <- rep(0,nrow(counland@data))
  
  counland <- counland[,-(1:2)]
  
  
  # 3.1.4 Merges all polygons of first network and adds polygon ID
  firstnet <- rbind(counpas,counland)
  firstnet@data$ID <- seq(1,nrow(firstnet@data))
  
  # 3.1.5 Writes to shapefile to disk
  writeOGR(obj=firstnet, dsn=OUTfolder, layer="firstnetwork", driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  # 3.2 Create Second network dataset.
  # The second network uses only the Pas within the country, with attribute equal to area, and the transboundary
  # PAs with attribute equal zero.
  
  
  
  
}





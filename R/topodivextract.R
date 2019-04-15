# topodivextract.R
# on 13th April 2019
# by Luis
# extract topography diversity for new pas at a country level
##################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)


# 1. INPUTS
OUTtopodivfolder <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/'


# 2. READ DATA
# Read global topo diversity at 1km
topodiv <- raster('/home/lcarrasco/Documents/research/protectedareas/data/Topodiv_Alos/GlobalALOStopographicDiversity270mMasked1km.tif')

# Read GADM data
gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')
gadmmulti <-shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart')

# Read PAs
pastill2010 <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_till2010_big_dissolved_filled_simp')

pas <- shapefile()


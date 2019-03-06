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
gadmmulti <-shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart')
gadmbuffer <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_500kmbuffer')
pas <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_2011on_final')

pastilldate <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_till2010_final')

# 3. MAIN ROUTINE

# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)


for (c in countrynames){
  
  #Run only for countries with PAs
  if(any(pas@data$ISO3==c)){
  
    print(paste("Extracting networks for ",c,sep=""))
    
    # 3.1 Create First network dataset (ProtUnconnDesign; Saura 2018, appendix)
    # The first network uses all PAs within the country, with an attribute equal to area, all transboundary PAs,
    # with attribute equal zero, and all the land portions of the country.
    
    # 3.1.1 Extracts country pas and add area
    counpas <- subset(pas, ISO3==c)
    # Adds attribute (area) column
    counpas@data$attribute <- area(counpas)
    # Leaves only attribute column
    counpas <- counpas[,-(1:2)]
    
    # 3.1.2 Extract transboundary pas and add 0 attribute
    countrans <- subset(gadmbuffer, GID_0==c)
    # Merge PAs of target period with till date
    alldatespas <- bind(pas,pastilldate)
    # Select PAs included in countrans
    transbound <- intersect(alldatespas,countrans)
    # But excluding the country PAs
    transbound <- subset(transbound, ISO3!=c)
    # Adds attribute column (equal to zero)
    transbound@data$attribute <- rep(0,nrow(transbound@data))
    # Leaves only attribute column
    transbound <- transbound[,-(1:6)]
    
    # 3.1.3 Extract land portions and add 0 attribute
    counland <- subset(gadmmulti, GID_0==c)
    # Adds attribute column (equal to zero)
    counland@data$attribute <- rep(0,nrow(counland@data))
    # Leaves only attribute column
    counland <- counland[,-(1:2)]
    
    
    # 3.1.4 Merges all polygons of first network and adds polygon ID
    firstnet <- rbind(counpas,transbound,counland)
    firstnet@data$ID <- seq(1,nrow(firstnet@data))
    
    # 3.1.5 Writes to shapefile to disk
    writeOGR(obj=firstnet, dsn=OUTfolder, layer=paste(c,"firstnetwork",sep=""), driver="ESRI Shapefile",
             overwrite_layer = TRUE)
    
    # !!! Writting gives a warning, but is an unnecessary one (known issue): https://trac.osgeo.org/gdal/ticket/6803
    
    
    # 3.2 Create Second network dataset.
    # The second network uses only the Pas within the country, with attribute equal to area, and the transboundary
    # PAs with attribute equal zero.
    
    # 3.2.1 Merges all polygons from country PAs and transboundary PAs with 0 attribute
    secondnet <- rbind(counpas,transbound)
    secondnet@data$ID <- seq(1,nrow(secondnet@data))
    
    # 3.1.5 Writes to shapefile to disk
    writeOGR(obj=secondnet, dsn=OUTfolder, layer=paste(c,"secondnetwork",sep=""), driver="ESRI Shapefile",
             overwrite_layer = TRUE)
  
  
  }
}





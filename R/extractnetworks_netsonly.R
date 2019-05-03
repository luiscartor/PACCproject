# extractnetworks.R
# on 1st March 2019
# by Luis
# extract PAs networks and creates shapefiles to ingest to conefor input files plugin
####################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)


# 1. INPUTS

# Protected areas file (for all years: WDPA_cleaned_all_final, or until 2010: WDPA_cleaned_till2010_final)
INpasfile <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_till2010_final'
# Country boundaries
INgadmfile <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify'
# Country boundaries where each piece of land is a different feature
INgadmmultifile <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart'
# Country boundaries with a 500km buffer
INgadmbufferfile <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_300kmbuffer'

# Output folder for the networks
OUTnetworkfolder <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/networks_till2010_300km/'


# 2. READ DATA

# Reads PAs data
pas <- shapefile(INpasfile)
# Reads country boundaries data 
gadm <- shapefile(INgadmfile)
gadmmulti <-shapefile(INgadmmultifile)
gadmbuffer <- shapefile(INgadmbufferfile)


# 3. MAIN ROUTINE

# 3.0.1 Calculates area of each PA polygon and delete polygons <1km2
pas@data$area <- area(pas)
pas <- subset(pas,area>=1000000)

# 3.0.2 Calculates area of each country piece of land and delete polygons <1km2
gadmmulti@data$area <- area(gadmmulti)
gadmmulti <- subset(gadmmulti,area>=1000000)

# 3.0.3 Obtains number of countries and create loop
countrynames <- unique(gadm@data$GID_0)

for (c in countrynames){
  
  #Run only for countries with existing PAs and avoid ANT (antartica)
  if(any(pas@data$ISO3==c & c != "ATA")){
  
    print(paste("Extracting networks for ",c,sep=""))
    
    # 3.1 Creates "First Network" dataset (ProtUnconnDesign; Saura 2018, appendix)
    # The first network uses all PAs within the country, with an attribute equal to area, all transboundary PAs,
    # with attribute equal zero, and all the land portions of the country.
    
    # 3.1.1 Extracts country pas and add area as attribute
    counpas <- subset(pas, ISO3==c)
    # Adds attribute (area) column
    counpas@data$attribute <- counpas@data$area
    # Leaves attribute column only
    counpas <- counpas[,-(1:7)]
    
    # 3.1.2 Extracts transboundary PAs and adds 0 attribute
    # Selects buffer
    countrans <- subset(gadmbuffer, GID_0==c)
    # Selects PAs within the buffer, but excluding the within-country PAs
    pasoutside <- subset(pas, ISO3!=c)
    
    # Selects PAs included in countrans
    # Here we use the overlay function to detect the polygons that overlay with 500km boundary (gives pol index)
    paswithin <- over(pasoutside,geometry(countrans))
    # Subsets to the polygons within the boundary
    transbound <- pasoutside[which(!is.na(paswithin)),]
    
    # In case there are no countries around, we still need transbound (with 0 polygons)
    if(length(transbound@polygons)!=0){
      # Adds attribute column (equal to zero)
      transbound@data$attribute <- rep(0,nrow(transbound@data))
      # Leaves only attribute column
      transbound <- transbound[,-(1:7)]
    }
    
    # 3.1.3 Extract land portions and add 0 attribute
    counland <- subset(gadmmulti, GID_0==c)
    # Adds attribute column (equal to zero)
    counland@data$attribute <- rep(0,nrow(counland@data))
    # Leaves only attribute column
    counland <- counland[,-(1:3)]
    
    
    # 3.1.4 Merges all polygons of first network and adds polygon ID
    firstnet <- rbind(counpas,transbound,counland, makeUniqueIDs = TRUE)
    firstnet@data$ID <- seq(1,nrow(firstnet@data))
    
    # 3.1.5 Calculates distance between edges of polygons
    # 3.1.5.1 Reprojects to azimuthal equidistant proj to calculate distances
    # Calculate centroid of country
    centroid <- gCentroid(counland)
    # crs with country centroid as map center
    crs_coun <- paste(paste("+proj=laea +x_0=0 +y_0=0 +lon_0=",centroid@coords[1],sep=""),
                      " +lat_0=",centroid@coords[2],sep="")
    firstnet_laea <- spTransform(firstnet, CRS(crs_coun))
    
    
    # 3.1.6 Writes  shapefile to disk
    writeOGR(obj=firstnet_laea, dsn=OUTnetworkfolder, layer=paste(c,"firstnetwork",sep=""), driver="ESRI Shapefile",
           overwrite_layer = TRUE)
    # !!! Writting gives a warning, but is an unnecessary one (known issue): https://trac.osgeo.org/gdal/ticket/6803
    
   
    
    # 3.2 Creates "Second Network" dataset: (ProtUnconnDesign; Saura 2018, appendix)
    # The second network uses only the Pas within the country, with attribute equal to area, and the transboundary
    # PAs with attribute equal zero.
    
    # 3.2.1 Merges all polygons from country PAs and transboundary PAs with 0 attribute
    secondnet <- rbind(counpas,transbound)
    secondnet@data$ID <- seq(1,nrow(secondnet@data))
    
    secondnet_laea <- spTransform(secondnet, CRS(crs_coun))
    
    # 3.2.X Writes to shapefile to disk
    writeOGR(obj=secondnet_laea, dsn=OUTnetworkfolder, layer=paste(c,"secondnetwork",sep=""), driver="ESRI Shapefile",
           overwrite_layer = TRUE)
    
  }
}

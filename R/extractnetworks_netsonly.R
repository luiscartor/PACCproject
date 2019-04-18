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
#INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
#INpasfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA/'

INpasfile <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_till2010_final'

OUTnetworkfolder <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/networks_till2010_500km/'


# 2. READ DATA
# Read country boundaries data (each piece of land is a different feature)
gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')
gadmmulti <-shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart')
gadmbuffer <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_500kmbuffer')


pas <- shapefile(INpasfile)


# 3. MAIN ROUTINE

pas@data$area <- area(pas)
pas <- subset(pas,area>=1000000)

# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)


for (c in countrynames){
  
  #Run only for countries with PAs and avoid ANT (antartica)
  if(any(pas@data$ISO3==c & c != "ATA")){
  
    print(paste("Extracting networks for ",c,sep=""))
    
    # 3.1 Create First network dataset (ProtUnconnDesign; Saura 2018, appendix)
    # The first network uses all PAs within the country, with an attribute equal to area, all transboundary PAs,
    # with attribute equal zero, and all the land portions of the country.
    
    # 3.1.1 Extracts country pas and add area
    counpas <- subset(pas, ISO3==c)
    # Dissolve polygons avoiding overlap
    #counpas <- aggregate(counpas,by="ISO3",dissolve=TRUE)
    # Disaggregate so that we have multiple features
    #counpas <- disaggregate(counpas)
    # Adds attribute (area) column
    counpas@data$attribute <- counpas@data$area
    # Delete areas < 1km2, in case some small polygons were created after dissolving/cleaning
    #counpas <- subset(counpas,attribute>=1000000)
    # Skip country (leave loop) if no areas > 1km
    #if(length(counpas@polygons)==0){
    #  print(paste("No PAs > 1km2 for ",c,sep=""))
    #  next
    #}
    
    # Clip to country to avoid sea areas
    #counlimits <- subset(gadm, GID_0==c)
    #counpas2 <- gIntersection(counpas, counlimits, byid=TRUE)
    
    
    # Leaves only attribute column
    counpas <- counpas[,-(1:3)]
    
    # 3.1.2 Extract transboundary pas and add 0 attribute
    countrans <- subset(gadmbuffer, GID_0==c)


    # But excluding the country PAs
    pasoutside <- subset(pas, ISO3!=c)
    
    
    # Select PAs included in countrans
    # Here we use the overlay function to detect the polygons that overlay with 500km boundary (gives pol index)
    paswithin <- over(pasoutside,geometry(countrans))
    
    # Subsets to the polygons within the boundary
    transbound <- pasoutside[which(!is.na(paswithin)),]
    
    # Dissolve polygons avoiding overlap
    #transbound <- aggregate(transbound,by="ISO3",dissolve=TRUE)
    # Disaggregate so that we have multiple features
    #transbound <- disaggregate(transbound)
    
    
    # In case there are no countries around, we still need transbound (with 0 polygons)
    if(length(transbound@polygons)!=0){
      # We want to avoid areas < 1sqr km
      #transbound@data$area <- area(transbound)
      #transbound <- subset(transbound,area>=1000000)
      # Adds attribute column (equal to zero)
      transbound@data$attribute <- rep(0,nrow(transbound@data))
      # Leaves only attribute column
      transbound <- transbound[,-(1:3)]
    }
    
    # 3.1.3 Extract land portions and add 0 attribute
    counland <- subset(gadmmulti, GID_0==c)
    # Adds attribute column (equal to zero)
    counland@data$attribute <- rep(0,nrow(counland@data))
    # Leaves only attribute column
    counland <- counland[,-(1:2)]
    
    
    # 3.1.4 Merges all polygons of first network and adds polygon ID
    firstnet <- rbind(counpas,transbound,counland, makeUniqueIDs = TRUE)
    firstnet@data$ID <- seq(1,nrow(firstnet@data))
    
    # 3.1.5 Calculate distance between edges of polygons
    # 3.1.5.1 Reproject to azimuthal equidistant proj to calculate distances
    # Calculate centroid of country
    centroid <- gCentroid(counland)
    # crs with country centroid as map center
    crs_coun <- paste(paste("+proj=laea +x_0=0 +y_0=0 +lon_0=",centroid@coords[1],sep=""),
                      " +lat_0=",centroid@coords[2],sep="")
    firstnet_laea <- spTransform(firstnet, CRS(crs_coun))
    
    
    # 3.1.X Writes  shapefile to disk
    writeOGR(obj=firstnet_laea, dsn=OUTnetworkfolder, layer=paste(c,"firstnetwork",sep=""), driver="ESRI Shapefile",
           overwrite_layer = TRUE)
    # !!! Writting gives a warning, but is an unnecessary one (known issue): https://trac.osgeo.org/gdal/ticket/6803
    
   
    
    # 3.2 Create Second network dataset.
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





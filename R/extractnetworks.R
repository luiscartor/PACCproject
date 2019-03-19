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

OUTnetworkfolder <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/networks/'
OUTconeforRfolder <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_r/'

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
  
  #Run only for countries with PAs and avoid ANT (antartica)
  if(any(pas@data$ISO3==c & c != "ATA")){
  
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
    # But excluding the country PAs
    alldatespas <- subset(alldatespas, ISO3!=c)
    
    # Select PAs included in countrans
    # Here we use the overlay function to detect the polygons that overlay with 500km boundary (gives pol index)
    paswithin <- over(alldatespas,geometry(countrans))
    # Subsets to the polygons within the boundary
    transbound <- alldatespas[which(!is.na(paswithin)),]
    
    # In case there are no countries around, we still need transbound (with 0 polygons)
    if(length(transbound@polygons)!=0){
      # Adds attribute column (equal to zero)
      transbound@data$attribute <- rep(0,nrow(transbound@data))
      # Leaves only attribute column
      transbound <- transbound[,-(1:2)]
    }
    
    # 3.1.3 Extract land portions and add 0 attribute
    counland <- subset(gadmmulti, GID_0==c)
    # Adds attribute column (equal to zero)
    counland@data$attribute <- rep(0,nrow(counland@data))
    # Leaves only attribute column
    counland <- counland[,-(1:2)]
    
    
    # 3.1.4 Merges all polygons of first network and adds polygon ID
    firstnet <- rbind(counpas,transbound,counland, makeUniqueIDs = TRUE)
    #firstnet@data$ID <- seq(1,nrow(firstnet@data))
    
    
    
    # 3.1.X Writes  shapefile to disk
    #writeOGR(obj=firstnet_laea, dsn=OUTnetworkfolder, layer=paste(c,"firstnetwork",sep=""), driver="ESRI Shapefile",
    #       overwrite_layer = TRUE)
    # !!! Writting gives a warning, but is an unnecessary one (known issue): https://trac.osgeo.org/gdal/ticket/6803
    
    
    # 3.1.5 Calculate distance between edges of polygons
    # 3.1.5.1 Reproject to azimuthal equidistant proj to calculate distances
    # Calculate centroid of country
    centroid <- gCentroid(counland)
    # crs with country centroid as map center
    crs_coun <- paste(paste("+proj=laea +x_0=0 +y_0=0 +lon_0=",centroid@coords[1],sep=""),
                      " +lat_0=",centroid@coords[2],sep="")
    firstnet_laea <- spTransform(firstnet, CRS(crs_coun))
    
    
    # 3.1.5.2 Calculate distances
    # Assignes ID value to polygon rowname
    #rownames(firstnet_laea@data) <- firstnet_laea@data$ID #doesnt work
    
    #distances <- gDistance(firstnet_laea, byid=TRUE)
    distances <- gDistance(firstnet_laea, byid=TRUE)
    
    #3.1.6 Transform to obtain CONEFOR formatting tables
    dist_table <- as.data.frame(as.table(distances))
    # Clean table: 1) Delete self distance
    dist_table <- dist_table[dist_table$Var1!=dist_table$Var2,]
    # 2) Delete duplicated row with inverted order of polygons
    table.sort = t(apply(dist_table, 1, sort))
    dist_table <- dist_table[!duplicated(table.sort),]
    # 3) Convert ids to numeric 
    dist_table$Var1 <- as.numeric(as.character(dist_table$Var1))
    dist_table$Var2 <- as.numeric(as.character(dist_table$Var2))
    
    # 3.1.7 Writes attribute text file to disk
    namepath <- paste(OUTconeforRfolder,paste(paste("attributes/",c,sep=""),"firstnetworkatt.txt",sep=""),sep="")
    write.table(as.numeric(rownames(firstnet_laea@data)), namepath, col.names=FALSE, row.names=FALSE)
    
    # 3.1.7 Writes distance text file to disk
    namepath <- paste(OUTconeforRfolder,paste(paste("distances/",c,sep=""),"firstnetworkdis.txt",sep=""),sep="")
    write.table(dist_table, namepath, col.names=FALSE, row.names=FALSE)
    
    
    #ncol(combn(seq(1:length(firstnet[1:5,])),2))
    
    
    # 3.2 Create Second network dataset.
    # The second network uses only the Pas within the country, with attribute equal to area, and the transboundary
    # PAs with attribute equal zero.
    
    # 3.2.1 Merges all polygons from country PAs and transboundary PAs with 0 attribute
    secondnet <- rbind(counpas,transbound)
    secondnet@data$ID <- seq(1,nrow(secondnet@data))
    
    #secondnet_laea <- spTransform(secondnet, CRS(crs_coun))
    
    # 3.2.X Writes to shapefile to disk
    #writeOGR(obj=secondnet_laea, dsn=OUTfolder, layer=paste(c,"secondnetwork",sep=""), driver="ESRI Shapefile",
     #        overwrite_layer = TRUE)
  
    
    
    
    # 3.2.3 Writes attribute text file to disk
    namepath <- paste(OUTconeforRfolder,paste(paste("attributes/",c,sep=""),"secondnetworkatt.txt",sep=""),sep="")
    write.table(as.numeric(rownames(secondnet@data)), namepath, col.names=FALSE, row.names=FALSE)
    
    
    
    # 3.1.7 Writes distance text file to disk
    # We don't wont to repeat gdistance, so we just delete the distances involving counland polygons
    dist_table2nd <- dist_table[!(dist_table$Var1 %in% as.numeric(rownames(counland@data))) 
                                & !(dist_table$Var2 %in% as.numeric(rownames(counland@data))),]
    
    # Writing
    namepath <- paste(OUTconeforRfolder,paste(paste("distances/",c,sep=""),"secondnetworkdis.txt",sep=""),sep="")
    write.table(dist_table2nd, namepath, col.names=FALSE, row.names=FALSE)
    
  }
}





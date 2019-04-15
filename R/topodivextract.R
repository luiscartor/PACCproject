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

pas <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under5km/WDPA_2011on_final')


# 3. PREPARE DATA
topodiv[topodiv <= 0] <- NA
topodiv[topodiv == 32767] <- NA

# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)


for (c in countrynames){
  
  #Run only for countries with PAs and avoid ANT (antartica)
  if(any(pas@data$ISO3==c & c != "ATA")){
    
    print(paste("Extracting data for ",c,sep=""))
    
    # 3.1 Calculate topo diversity for areas not occupied by PAs
    # Read country established PAs
    counpastill2010 <- subset(pastill2010, ISO3==c)
    # Country boundaries
    counland <- subset(gadm, GID_0==c)
    
    # Obtained non protected area
    nonpa <- gDifference(counland,counpastill2010)
    
    
    
    # 3.2 Calculate topo diversity for new PAs
    # Subset country pas
    counpas <- subset(pas, ISO3==c)
    
    # Extract div values from PAs
    values <- extract(topodiv,counpas)
    
    # Calculate statistics. We need to sustract 1 to the values as the layer has a +1 (to avoid the na/zeroes)
    pa_mean <- mean(unlist(values),na.rm=TRUE) - 1
    pa_med <- median(unlist(values),na.rm=TRUE) - 1
    pa_var <- var(unlist(values),na.rm=TRUE)
    
    
  
    
    
    
    
  }
  
}
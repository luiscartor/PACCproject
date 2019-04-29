# ccvelextract.R
# on 22th April 2019
# by Luis
# extract climate change velocity for new pas at a country level
##################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)


# 1. INPUTS
OUTccvelfolder <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/'


# 2. READ DATA
# Read cc vel at 1km 
ccvel <- raster('/home/lcarrasco/Documents/research/protectedareas/ccvel/ccvel1km120binmasked.tif')


# Mask ccvel by ccvel

# Read GADM data
gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')
gadmmulti <-shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart')

gadm_rob <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify_robinson')
# Read PAs
pastill2010 <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_till2010_final')

pas <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_2011on_final')


# 3. PREPARE DATA
# Delete polygons smaller than 1km2
pastill2010@data$area <- area(pastill2010)
pastill2010 <- subset(pastill2010,area>=1000000)

pas@data$area <- area(pas)
pas <- subset(pas,area>=1000000)


# The -9999 values of the original raster are NAs
ccvel[ccvel == -9999] <- NA


# Creates a layer with old PAs masked out
#ccvel_maskedoldpa <- mask(ccvel,pastill2010,inverse=TRUE)
#writeRaster(ccvel_maskedoldpa,filename="/home/lcarrasco/Documents/research/protectedareas/ccvel/ccvel_maskPAtill2010.tif", 
#            format="GTiff",datatype='INT2S', overwrite=TRUE)
ccvel_maskedoldpa <- raster('/home/lcarrasco/Documents/research/protectedareas/ccvel/ccvel_maskPAtill2010.tif')


# 4. Main routine
# 4.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)

stats_mat <- data.frame()
for (c in countrynames){
  
  #Run only for countries with PAs and avoid ANT (antartica) and MSR (no PAs before 2010)
  if(any(pas@data$ISO3==c & c != "ATA" & c != "MSR")){
    
    print(paste("Extracting data for ",c,sep=""))
    
    # 4.1 Calculate topo diversity for areas not occupied by previous PAs
    # Read country established PAs
    #counpastill2010 <- subset(pastill2010, ISO3==c)
    # Country boundaries
    counland <- subset(gadm, GID_0==c)
    
    # Obtained non protected area
    #nonpa <- gDifference(counland,counpastill2010)
    
    # Extract div values from outside of the PAs (with old PAs masked, so doesn't add div inside those)
    values_outofpas <- extract(ccvel_maskedoldpa,counland)
    
    # Calculate statistics. We need to sustract 1 to the values as the layer has a +1 (to avoid the na/zeroes)
    outpa_mean <- mean(unlist(values_outofpas),na.rm=TRUE) - 1
    outpa_med <- median(unlist(values_outofpas),na.rm=TRUE) - 1
    outpa_var <- var(unlist(values_outofpas),na.rm=TRUE)
    outpa_tot <- length(unlist(values_outofpas)[!is.na(unlist(values_outofpas))])
    
    # 4.2 Calculate topo diversity for new PAs
    # Subset country pas
    counpas <- subset(pas, ISO3==c)
    
    # Extract div values from PAs
    values_pas <- extract(ccvel_maskedoldpa,counpas)
    
    # Calculate statistics. 
    pa_mean <- mean(unlist(values_pas),na.rm=TRUE) - 1
    pa_med <- median(unlist(values_pas),na.rm=TRUE) - 1
    pa_var <- var(unlist(values_pas),na.rm=TRUE)
    pa_tot <- length(unlist(values_pas)[!is.na(unlist(values_pas))])
    
    
    # 4.3 Calculates topo diversity for old PAs
    # Read country established PAs before 2011
    counpastill2010 <- subset(pastill2010, ISO3==c)
    
    # Extract div values from inside old PAs, uses the non masked ccvel layer
    values_oldpas <- extract(ccvel,counpastill2010)
    
    # Calculate statistics.
    oldpa_mean <- mean(unlist(values_oldpas),na.rm=TRUE) 
    oldpa_med <- median(unlist(values_oldpas),na.rm=TRUE)
    oldpa_var <- var(unlist(values_oldpas),na.rm=TRUE)
    oldpa_tot <- length(unlist(values_oldpas)[!is.na(unlist(values_oldpas))])
    
    # 4.4 Calculates topo diversity for the whole country
    values_coun <- extract(ccvel,counland)
    # Calculate statistics.
    coun_mean <- mean(unlist(values_coun),na.rm=TRUE) 
    coun_med <- median(unlist(values_coun),na.rm=TRUE) 
    coun_var <- var(unlist(values_coun),na.rm=TRUE)
    coun_tot <- length(unlist(values_coun)[!is.na(unlist(values_coun))])
    
    
    # 4.5 Puts statistics into vector
    coun_stats <- data.frame(c,pa_mean,pa_med,pa_var,pa_tot,outpa_mean,outpa_med,outpa_var,outpa_tot,
                             oldpa_mean,oldpa_med,oldpa_var,oldpa_tot,coun_mean,coun_med,coun_var,coun_tot)
    stats_mat <- rbind(stats_mat,coun_stats)
    
  }
  
}

colnames(stats_mat) <- c("country","pamean","pamed","pavar","patot","outpamean","outpamed","outpavar","outpatot",
                         "oldpamean","oldpamed","oldpavar","oldpatot","counmean","counmed","counvar","countot")

# 5 Write data
outtablename <- paste(OUTccvelfolder,"PAsccveltable_simp100.txt",sep="")
#write.csv2(stats_mat,file = outtablename,row.names=FALSE)
write.table(stats_mat,file = outtablename, row.names = FALSE)



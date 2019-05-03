# topodivextract.R
# on 13th April 2019
# by Luis
# extracts topography diversity for new pas at a country level
###########################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)

# 1. INPUTS
# Output folder
OUTtopodivfolder <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/'

# Protected areas file paths
INpas <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_2011on_final'
# Protected areas untill 2010
INpastill2010 <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_till2010_final'

# Topographic diversity file path
INtopodivfile <- '/home/lcarrasco/Documents/research/protectedareas/data/Topodiv_Alos/GlobalALOStopographicDiversity270mMasked1km.tif'

# Country boundaries files paths
INgadm <-'/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify' 
INgadmmulti <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart'


# 2. READ DATA
# Reads global topo diversity at 1km
topodiv <- raster(INtopodivfile)
# Reads GADM data
gadm <- shapefile(INgadm)
gadmmulti <-shapefile(INgadmmulti)
# Read PAs
pastill2010 <- shapefile(INpastill2010)
pas <- shapefile(INpas)


# 3. PREPARE DATA
# 3.1 Deletes PAs polygons smaller than 1km2
pastill2010@data$area <- area(pastill2010)
pastill2010 <- subset(pastill2010,area>=1000000)

pas@data$area <- area(pas)
pas <- subset(pas,area>=1000000)

# 3.2 Cleans topographic diversity raster: The zero values of the original raster are NAs, so we set 0 to NA (and 32767)
topodiv[topodiv <= 0] <- NA
topodiv[topodiv == 32767] <- NA

# 3.3. Creates a layer with old PAs masked out: We don't want the algorithm to consider topographic diversity of "available land"
# inside old PAs

#topodiv_maskedoldpa <- mask(topodiv,pastill2010,inverse=TRUE)
#writeRaster(topodiv_maskedoldpa,filename="/home/lcarrasco/Documents/research/protectedareas/data/Topodiv_Alos/topodiv_maskPAtill2010.tif", 
#            format="GTiff",datatype='INT2S', overwrite=TRUE)
topodiv_maskedoldpa <- raster('/home/lcarrasco/Documents/research/protectedareas/data/Topodiv_Alos/topodiv_maskPAtill2010.tif')


# 4. MAIN ROUTINE
# 4.0 Obtains countries names and creates loop
countrynames <- unique(gadm@data$GID_0)

stats_mat <- data.frame()
for (c in countrynames){
  
  # Run only for countries with PAs and avoid ANT (antartica) and MSR (no PAs before 2010)
  if(any(pas@data$ISO3==c & c != "ATA" & c != "MSR")){
    
    print(paste("Extracting data for ",c,sep=""))
    
    # 4.1 Calculates top. diversity for areas not occupied by previous PAs
    # Country boundaries
    counland <- subset(gadm, GID_0==c)
    
    # Extract top. diversity values from outside of the PAs (with old PAs masked, so doesn't add div inside those)
    values_outofpas <- extract(topodiv_maskedoldpa,counland)
    # Calculates statistics. We need to sustract 1 to the values as the layer has a +1 (to avoid the na/zeroes)
    outpa_mean <- mean(unlist(values_outofpas),na.rm=TRUE) - 1
    outpa_med <- median(unlist(values_outofpas),na.rm=TRUE) - 1
    outpa_var <- var(unlist(values_outofpas),na.rm=TRUE)
    outpa_tot <- length(unlist(values_outofpas)[!is.na(unlist(values_outofpas))])
    
    
    # 4.2 Calculates topo diversity for new PAs
    # Subset country PAs
    counpas <- subset(pas, ISO3==c)
    
    # Extract div values from PAs
    values_pas <- extract(topodiv_maskedoldpa,counpas)
    # Calculate statistics. 
    pa_mean <- mean(unlist(values_pas),na.rm=TRUE) - 1
    pa_med <- median(unlist(values_pas),na.rm=TRUE) - 1
    pa_var <- var(unlist(values_pas),na.rm=TRUE)
    pa_tot <- length(unlist(values_pas)[!is.na(unlist(values_pas))])
    
    
    # 4.3 Calculates topo diversity for old PAs
    # Reads country established PAs before 2011
    counpastill2010 <- subset(pastill2010, ISO3==c)
    
    # Extract top. div values from inside old PAs, uses the non-masked topodiv layer
    values_oldpas <- extract(topodiv,counpastill2010)
    # Calculates statistics
    oldpa_mean <- mean(unlist(values_oldpas),na.rm=TRUE) - 1
    oldpa_med <- median(unlist(values_oldpas),na.rm=TRUE) - 1
    oldpa_var <- var(unlist(values_oldpas),na.rm=TRUE)
    oldpa_tot <- length(unlist(values_oldpas)[!is.na(unlist(values_oldpas))])
    
    # 4.4 Calculates topo diversity for the whole country
    values_coun <- extract(topodiv,counland)
    # Calculates statistics
    coun_mean <- mean(unlist(values_coun),na.rm=TRUE) - 1
    coun_med <- median(unlist(values_coun),na.rm=TRUE) - 1
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

# 5. WRITE DATA
outtablename <- paste(OUTtopodivfolder,"PAstopodivtable_simp100.txt",sep="")
#write.csv2(stats_mat,file = outtablename,row.names=FALSE)
write.table(stats_mat,file = outtablename, row.names = FALSE)

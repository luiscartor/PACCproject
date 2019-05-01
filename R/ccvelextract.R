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
# Output folder
OUTccvelfolder <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/'

# CCvel file path
INccvel <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/ccvel1km120binmasked.tif'
  
# Protected areas file paths
INpas <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_2011on_final'
# Protected areas untill 2010
INpastill2010 <- '/home/lcarrasco/Documents/research/protectedareas/data/WDPA_under1km/WDPA_cleaned_till2010_final'

# Country boundaries files paths
INgadm <-'/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify' 
INgadmmulti <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_multipart'


# 2. READ DATA
# Reads cc vel at 1km 
ccvel <- raster(INccvel)
# Reads GADM data
gadm <- shapefile(INgadm)
gadmmulti <-shapefile(INgadmmulti)
# Reads PAs
pastill2010 <- shapefile(INpastill2010)
pas <- shapefile(INpas)


# 3. PREPARE DATA
# 3.1 Deletes PAs polygons smaller than 1km2
pastill2010@data$area <- area(pastill2010)
pastill2010 <- subset(pastill2010,area>=1000000)

pas@data$area <- area(pas)
pas <- subset(pas,area>=1000000)

# 3.2 Cleanes ccvel NAs
# The -9999 values of the original raster are NAs
ccvel[ccvel == -9999] <- NA


# 3.3 Creates a layer with old PAs masked out
#ccvel_maskedoldpa <- mask(ccvel,pastill2010,inverse=TRUE)
#writeRaster(ccvel_maskedoldpa,filename="/home/lcarrasco/Documents/research/protectedareas/ccvel/ccvel_maskPAtill2010.tif", 
#            format="GTiff",datatype='INT2S', overwrite=TRUE)
ccvel_maskedoldpa <- raster('/home/lcarrasco/Documents/research/protectedareas/ccvel/ccvel_maskPAtill2010.tif')


# 4. MAIN ROUTINE
# 4.0 Obtain countries codes and create loop
countrynames <- unique(gadm@data$GID_0)

stats_mat <- data.frame()
for (c in countrynames){
  
  #Run only for countries with PAs and avoid ANT (antartica) and MSR (no PAs before 2010)
  if(any(pas@data$ISO3==c & c != "ATA" & c != "MSR")){
    
    print(paste("Extracting data for ",c,sep=""))
    
    # 4.1 Calculates ccvel for areas not occupied by previous PAs
    # Country boundaries
    counland <- subset(gadm, GID_0==c)
    
    # Extract ccvel values from outside of the PAs (with old PAs masked, so doesn't add ccvel inside those)
    values_outofpas <- extract(ccvel_maskedoldpa,counland)
    
    # Calculate statistics. 
    outpa_mean <- mean(unlist(values_outofpas),na.rm=TRUE) 
    outpa_med <- median(unlist(values_outofpas),na.rm=TRUE) 
    outpa_var <- var(unlist(values_outofpas),na.rm=TRUE)
    outpa_tot <- length(unlist(values_outofpas)[!is.na(unlist(values_outofpas))])
    
    # 4.2 Calculate ccvel for new PAs
    # Subsets country pas
    counpas <- subset(pas, ISO3==c)
    
    # Extracts ccvel values from PAs
    values_pas <- extract(ccvel_maskedoldpa,counpas)
    
    # Calculate statistics. 
    pa_mean <- mean(unlist(values_pas),na.rm=TRUE) 
    pa_med <- median(unlist(values_pas),na.rm=TRUE) 
    pa_var <- var(unlist(values_pas),na.rm=TRUE)
    pa_tot <- length(unlist(values_pas)[!is.na(unlist(values_pas))])
    
    
    # 4.3 Calculates ccvel for old PAs
    # Reads country established PAs before 2011
    counpastill2010 <- subset(pastill2010, ISO3==c)
    
    # Extracts ccvel values from inside old PAs, uses the non masked ccvel layer
    values_oldpas <- extract(ccvel,counpastill2010)
    
    # Calculate statistics
    oldpa_mean <- mean(unlist(values_oldpas),na.rm=TRUE) 
    oldpa_med <- median(unlist(values_oldpas),na.rm=TRUE)
    oldpa_var <- var(unlist(values_oldpas),na.rm=TRUE)
    oldpa_tot <- length(unlist(values_oldpas)[!is.na(unlist(values_oldpas))])
    
    # 4.4 Calculates ccvel for the whole country
    values_coun <- extract(ccvel,counland)
    # Calculates statistics
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

# 5 WRITE DATA
outtablename <- paste(OUTccvelfolder,"PAsccveltable_simp100.txt",sep="")
#write.csv2(stats_mat,file = outtablename,row.names=FALSE)
write.table(stats_mat,file = outtablename, row.names = FALSE)



# bioclimextract.R
# 30 November 2018
# by Luis
# extracts the bioclim variables from world clim and creates PCA for future and present climate

library(raster)
library(sp)

# 1. INPUTS
# Sets working directory
setwd('C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\')

# Sets outputs folder
outFolder <- 'C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\'

# 2. GET DATA
# Get bioclim data for 30 seconds
# Current 1960-1990
# Lat long parameters
lonlist <- seq(-180,150,30)
latlist <- seq(-50,90,30)
currentbiolist <- list()
counter <- 0
for(j in 1:length(latlist)){
  for(i in 1:length(lonlist)){
    counter <- counter+1
    currentbiolist[[counter]] <- getData("worldclim",var="bio",res=0.5, lon = lonlist[i], lat = latlist[[j]])
  }
}

# DONT USE! Merge in one file
#currentbio <- do.call(merge,currentbiolist)
#writeRaster(currentbio, paste(outFolder,"currentbio.tif",sep=""), format="GTiff") 


# Future 2060-2080
# Future climate data is NOT divided in tiles
# Selected models: HadGEM2-ES; ACCESS1-0; bcc-csm1-1-m; CCSM4; CNRM-CM5
futurebiolist <- list()
models <- c("BC","AC","CC","CN","HE")
for (i in 1:length(models)){
  futurebiolist[[i]] <- getData('CMIP5', var='bio', res=0.5, rcp=85, model=models[i], year=70)
}

# 3. PREPARE DATA

# Averaged future projections
# Create copy of raster in order to replace bands
futureavg <- futurebiolist[[1]]
# Loop for number of bands
for (j in nlayers(futurebiolist[[1]])){
  # Loop for number of models
  bioclim <- list()
  for (m in 1:length(models)){
    bioclim[[m]] <- futurebiolist[[m]][[j]]
  }
  bioclim_brick <- brick(bioclim)
  futureavg[[j]] <- calc(bioclim_brick, mean)
}


# Change layer names for all tiles and future
bandnames <- c(substr(names(currentbiolist[[1]])[1:9], 0, 4),substr(names(currentbiolist[[1]])[10:19],0,5))
# Band names have to match with currentbio
for(i in 1:length(currentbiolist)){
  names(currentbiolist[[i]]) <- bandnames
}
names(futureavg) <- bandnames


# Removes objects
rm(futurebiolist,bioclim,bioclim_brick)


# 4. ANALISIS
# Run PCA for current data
set.seed(1)
# Run PCA on random sample of 1000 pix/tile (60000) and apply to raster
# Extract sample
samp <- c()
for(i in 1:length(currentbiolist)){
  samp <- rbind(samp,sampleRandom(currentbiolist[[i]],1000,na.rm=TRUE))
}
#samp <- sampleRandom(currentbio,10000)

# Run PCA
pca <- prcomp(samp, scale=TRUE, retx=FALSE) 
#write.csv(pca$rotation, paste(outFolder,"pca1km_rotation.csv",sep=""))
#write.csv(pca$sdev, paste(outFolder,"pca1km_sdev.csv",sep=""))
#write.csv(pca$center, paste(outFolder,"pca1km_center.csv",sep=""))
#write.csv(pca$scale, paste(outFolder,"pca1km_scale.csv",sep=""))

# Create PCA layers from current and future clim data
# Current climate layers are tiled so the prediction is run through tiles
currentPCAlist <- list()
for(i in 1:length(currentbiolist)){
  currentPCAlist[[i]] <- predict(currentbiolist[[i]],pca,index=1:2)
}
#rm(currentbiolist)

# Create one global layer
currentPCAlist$file <- paste(outFolder,"currentPCA-1km.tif",sep="")
currentPCAlist$overwrite <- TRUE
currentPCA <- do.call(merge,currentPCAlist)

rm(currentPCAlist)

# Obtain PCA for future clim. (no tiled)
futurePCA <- predict(futureavg,pca,index=1:2)



# 5. WRITE: rasters to asci
writeRaster(currentPCA[[1]], filename=paste(outFolder,"currentPCA1-1km.asc",sep=""), format="ascii",overwrite=TRUE)
writeRaster(currentPCA[[2]], filename=paste(outFolder,"currentPCA2-1km.asc",sep=""), format="ascii",overwrite=TRUE)
writeRaster(futurePCA[[1]], filename=paste(outFolder,"futurePCA1-1km.asc",sep=""), format="ascii",overwrite=TRUE)
writeRaster(futurePCA[[2]], filename=paste(outFolder,"futurePCA2-1km.asc",sep=""), format="ascii",overwrite=TRUE)

writeRaster(currentPCA, paste(outFolder,"currentPCA-1km.tif",sep=""), format="GTiff",overwrite=TRUE)
writeRaster(futurePCA, paste(outFolder,"futurePCA-1km.tif",sep=""), format="GTiff", overwrite=TRUE)

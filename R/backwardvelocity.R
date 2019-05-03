# backwardvelocity.R
# on 12 December 2014
# by Luis, edited from Hamann
# Backward climate change velocity
# Based on Hamann 2014; Appendix 3 https://adaptwest.databasin.org/pages/adaptwest-velocitywna

library(MASS)
library(SDMTools)
library(raster)
library(yaImpute)     # install package for k-nearest neighbour (kNN) search
library(dplyr)

# Sets working directory
setwd('/var/tmp/')
# Sets outputs folder
outFolder <- '/var/tmp/'

# Luis: Read PCA rasters and convert to ASCII 
print("Reading data")
#currentPCA <- stack(paste(outFolder,"currentPCA-1km.tif",sep=""))
#futurePCA <- stack(paste(outFolder,"futurePCA-1km.tif",sep=""))
#writeRaster(currentPCA[[1]], filename="C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\currentPCA1-5km.asc", format="ascii",overwrite=TRUE)
#writeRaster(currentPCA[[2]], filename="C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\currentPCA2-5km.asc", format="ascii",overwrite=TRUE)
#writeRaster(futurePCA[[1]], filename="C:\\Users\\lcarrasc\\Documents\\researlch\\protectedareas\\analysis\\futurePCA1-5km.asc", format="ascii",overwrite=TRUE)
#writeRaster(futurePCA[[2]], filename="C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\futurePCA2-5km.asc", format="ascii",overwrite=TRUE)

# Luis: If already created, read Ascii data (and uncomment previous paragraph)
# For backward velocity, we assign present to future and viceversa
future1 <- asc2dataframe(paste(outFolder,"currentPCA1-1km.asc",sep="")) # principal component grids
future2 <- asc2dataframe(paste(outFolder,"currentPCA2-1km.asc",sep=""))
present1  <- asc2dataframe(paste(outFolder,"futurePCA1-1km.asc",sep=""))
present2  <- asc2dataframe(paste(outFolder,"futurePCA2-1km.asc",sep=""))

idxy <- cbind(id=1:nrow(present1),present1[,1:2])   # data frame of IDs and XY coords
b <- (max(present1$var.1)-min(present1$var.1))/140  # bin size for 120 PC1 bins

p1 <- round(present1$var.1/b)              # convert PC1 to 120 bins via rounding
p2 <- round(present2$var.1/b)              # convert PC2 to <120 bins via rounding
f1 <- round(future1$var.1/b)               # same for future PC1
f2 <- round(future2$var.1/b)               # same for future PC2
p  <- paste(p1,p2)                         # PC1/PC2 combinations in present climate
f  <- paste(f1,f2)                         # PC1/PC2 combinations in future climate
u  <- unique(p)[order(unique(p))]          # list of unique PC1/PC2 combinations

sid <- c()                                 # empty vector for source IDs
tid <- c()                                 # empty vector for target IDs
d   <- c()                                 # empty vector for distances

print("Start loop to search PC1/PC2 combination")
for(i in u){                          # loop for each unique PC1/PC2 combination
  pxy <- idxy[which(p==i),]           # coordinates of i-th combination in present
  fxy <- idxy[which(f==i),]           # coordinates of i-th combination in future
  sid <- c(sid, pxy$id)               # append i-th PC1/PC2 combination to previous 
  
  if(nrow(fxy)>0){                    # kNN search unless no-analogue climate
    knn <- data.frame(ann(as.matrix(fxy[,-1]), as.matrix(pxy[,-1]), k=1)$knnIndexDist)      
    tid <- c(tid, fxy[knn[,1],"id"]) # the IDs of the closest matches  
    d <- c(d, sqrt(knn[,2]))         # their corresponding geographic distances
  }
  else {                              # else statement for no-analogue climates
    tid <- c(tid, rep(NA,nrow(pxy))) # flag destinations as missing for no analogues
    d <- c(d, rep(Inf,nrow(pxy)))    # flag distances as infinity for no analogues
  }
}

sxy <- merge(sid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # source coordinates
txy <- merge(tid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # target coordinates
names(txy)=c("target_y","target_x")


# write output table in CSV format with source and target coordinates and distances
outtab <- cbind(id=sid, sxy, txy, distance=d)   
write.csv(outtab, "outtab140bin.csv", row.names=F)
outtab <- read.csv2(file="outtab140bin.csv",sep=",")

# if out=merge is failing, remove some objetcs:
#rm(sxy,txy,p1,p2,f1,f2,sid,tid,idxy,p,f,u, d, present2, future1, future2)

# writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
# Luis edit: change outtab 3rd column name
names(outtab)[3] <- "x"

print("Writing velocities and distances")

# Luis: merge is running out of ram. We convert to data.table and merge
out=merge(present1[,1:2], outtab[,c(2,3,6)], by=c("y","x"), sort=F) # Original code

#rm(outtab,present1)
out$distance <- as.numeric(as.character(out$distance)) #Luis edit: as.numercic(as.character())
out$distance[out$distance==Inf] <- 10000  # sets no analogue to 10,000km (10000); for 5km grid, 2000
out$distance[out$distance==0] <- 0.5  # sets zero distance to 0.5km (1/2 cell size); 2.5 km for 5km grid

out$logDist=round(log10(out$distance)*100)
out$logSpeed=round(log10(out$distance/95)*100)
#dataframe2asc(out)
#rm(outtab)
# Convert to raster log speed
print("Convert to raster")
outras <- rasterFromXYZ(out[,c(2,1,5)])
projection(outras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
writeRaster(outras,filename = paste(outFolder,"ccvel1km_140bin",sep=""), format = "GTiff",overwrite=TRUE)

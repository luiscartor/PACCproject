# protconnbound.R
# on 26 February 2019
# by Luis
# calculates connectivity at the country level for PACC project,
# based on ProtConn-Bound from Saura et al. 2018.
# uses attribute and distance files calculated by extractnetworks.R
# calls conefor software (conefor executable must be present in working directory)
#################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)

# 0. FUNCTIONS
# PCfun calculates PC using conefor software
coneforpath <- paste(getwd(),"coveforLinux64.exe",sep="")

PCfun <- function(attfile, disfile){
  
  #system("/home/lcarrasco/Documents/research/protectedareas/PACCproject/coneforLinux64.exe")
  
  command <- paste(coneforpath,"-nodeFile", attfile,"-conFile",disfile, "-t dist all -confProb 200 0.5 -PC onlyoverall", collapse = ' ')
  system(command)
  
}

# 1. INPUTS
INattfolder <- "/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_r/attributes/"
INdisfolder <- "/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_r/distances/"



# 2. READ DATA
#gadm <- readOGR(INgadmfolder, 'gadm36_0_simplify')
gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')

pas <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_2011on_final')
pastilldate <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_till2010_final')




# 3. MAIN ROUTINE
countrynamesvec <- c()
protconnboundvec <- c()
# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)

for (c in countrynames){
  #Run only for countries with PAs and avoid ANT (antartica)
  if(file.exists(paste(INattfolder,c,"firstnetworkatt.txt",sep = ""))){
  
    # 3.1 Read network conefor files
    attfile1st <- paste(INattfolder,c,"firstnetworkatt.txt",sep = "")
    disfile1st <- paste(INdisfolder,c,"firstnetworkdis.txt",sep = "")
    attfile2nd <- paste(INattfolder,c,"secondnetworkatt.txt",sep = "")
    disfile2nd <- paste(INdisfolder,c,"secondnetworkdis.txt",sep = "")
    
    # 3.2 Calculates PC (probability of connectivity) index with conefor for both networks
    
    PC1st <- PCfun(attfile1st,disfile1st)
    PC2nd <- PCfun(attfile1st,disfile1st)
    
    # 3.3 Calculates ProtUnconn[Design] (Saura 2018 Appendix B.3)
    # Needs country area
    countryarea <- area(subset(gadm, GID_0==c))
    
    protunconndes <- (100*(PC1st/countryarea)) - (100*(PC2nd/countryarea))
    
    # 3.4 Calculates ProtConnBound; ProtConnBound=Prot−ProtUnconn[Design]
    # Needs the total protected area of considered dataset
    areatable <- read.table(attfile1st)
    protarea <- sum(areatable[,2])
      
    protconnbound <- (protarea/countryarea) - protunconndes
    
    countrynamesvec <- append(countrynamesvec, c) 
    protconnboundvec <- append(protconnboundvec, protconnbound)
  
  }
}

# 4. WRITE RESULTS
protconnboundtable <- data.frame(countrynamesvec,protconnboundvec)
colnames(protconnboundtable) <- c("country","protconnbound")

#write.table(protconnboundtable, "/home/lcarrasco/Documents/research/protectedareas/connectivity/results/protconnboundtable.txt",
 #           row.names = F, col.names = F)

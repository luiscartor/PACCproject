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
coneforpath <- paste(getwd(),"/coneforWin64",sep="")

#system("/home/lcarrasco/Documents/research/protectedareas/PACCproject/coneforLinux64")
system("C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\PACCproject\\coneforWin64.exe")

PCfun <- function(attfile, disfile, countrycode){
  
  command <- paste(coneforpath,"-nodeFile", attfile,"-conFile",disfile, "-t dist notall -confProb 10000 0.5 -PC onlyoverall -prefix", countrycode, collapse = ' ')
  shell(command)
  
}

# 1. INPUTS
#INattfolder <- "/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_r/attributes/"
#INdisfolder <- "/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_r/distances/"
#INattfolder <- "/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_qgis/attribute/"
#INdisfolder <- "/home/lcarrasco/Documents/research/protectedareas/connectivity/coneforfiles_qgis/distances/"

INfolder <- "C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\connectivity\\conefor_inputs_arcgis_fortest\\"



# 2. READ DATA
#gadm <- readOGR(INgadmfolder, 'gadm36_0_simplify')
#gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')
gadm <- shapefile('C:\\Users\\lcarrasc\\Documents\\research\\chinese_infrastructures\\datasets\\GADM\\gadm36_0_simplify')

#pas <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_2011on_final')
pas <- shapefile('C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\data\\WDPA_Global\\WDPA_2011on_final')

#pastilldate <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/WDPA/WDPA_till2010_final')
pastilldate <- shapefile('C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\data\\WDPA_Global\\WDPA_till2010_final')



# 3. MAIN ROUTINE

# 3.1 Calculate PC index for all networks
shell(paste(INfolder,"-nodeFile nodes_ -conFile distances_ -t dist notall -* -confProb 10000 0.5 -PC onlyoverall", collapse = ' '))

# Read PC index results from conefor created file
pctable <- read.table(paste(INfolder,"results_all_EC(PC).txt",sep = ""))


# 3.2 Calculate ProtConn bound: looping through all countries

countrynamesvec <- c()
protconnboundvec <- c()
# 3.0 Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)

for (c in countrynames){
  #Run only for countries with PAs and avoid ANT (antartica)
  if(paste(c,"firstnetwork.txt",sep = "") %in% pctable$V1){
  
    
    
    attfile1st <- paste(INfolder,"nodes_",c,"firstnetwork.txt",sep = "",collapse = "")
    disfile1st <- paste(INfolder,"distances_",c,"firstnetwork.txt",sep = "",collapse = "")
    attfile2nd <- paste(INfolder,"nodes_",c,"secondnetwork.txt",sep = "",collapse = "")
    disfile2nd <- paste(INfolder,"distances_",c,"secondnetwork.txt",sep = "",collapse = "")
    
    # 3.2 Calculates PC (probability of connectivity) index with conefor for both networks
    
    PC1st <- PCfun(attfile1st,disfile1st,c)
    PC2nd <- PCfun(attfile2nd,disfile2nd,c)
    
    # 3.3 Calculates ProtUnconn[Design] (Saura 2018 Appendix B.3)
    # Needs country area
    countryarea <- area(subset(gadm, GID_0==c))
    
    protunconndes <- (100*(PC1st/countryarea)) - (100*(PC2nd/countryarea))
    protunconndes <- (100*(82397800000/countryarea)) - (100*(31518700000/countryarea))

    # 3.4 Calculates ProtConnBound; ProtConnBound=Prot−ProtUnconn[Design]
    # Needs the total protected area of considered dataset
    areatable <- read.table(attfile1st)
    protarea <- sum(areatable[,2])
      
    protconnbound <- 100*(protarea/countryarea) - protunconndes
    
    countrynamesvec <- append(countrynamesvec, c) 
    protconnboundvec <- append(protconnboundvec, protconnbound)
  
  }
}

# 4. WRITE RESULTS
protconnboundtable <- data.frame(countrynamesvec,protconnboundvec)
colnames(protconnboundtable) <- c("country","protconnbound")

#write.table(protconnboundtable, "/home/lcarrasco/Documents/research/protectedareas/connectivity/results/protconnboundtable.txt",
 #           row.names = F, col.names = F)



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
PCfun <- function(attfile, disfile, countrycode){
  
  command <- paste(coneforpath,"-nodeFile", attfile,"-conFile",disfile, "-t dist notall -confProb 10000 0.5 -PC onlyoverall -prefix", countrycode, collapse = ' ')
  shell(command)
  
}

# 1. INPUTS
# Set working directory where node/distances files are; conefor.exe should be placed here also.
setwd("C:\\Users\\lcarrasc\\Documents\\research\\protectedareas\\analysis\\connectivity\\conefor_inputs_arcgis_fortest\\")

# Output file name
OUTfile <- "/protconnbound_till2010.txt"



# 2. READ DATA
#gadm <- shapefile('/home/lcarrasco/Documents/research/protectedareas/data/GADM/gadm36_0_simplify')
gadm <- shapefile('C:\\Users\\lcarrasc\\Documents\\research\\chinese_infrastructures\\datasets\\GADM\\gadm36_0_simplify')


# 3. MAIN ROUTINE

# 3.1 Calculate PC index for all networks
coneforpath <- paste(getwd(),"\\coneforWin64",sep="")
system(coneforpath)
shell(paste(coneforpath,"-nodeFile nodes_ -conFile distances_ -t dist notall -* -confProb 10000 0.5 -PC onlyoverall", collapse = ' '))

# Read PC index results from conefor created file
pctable <- read.table(paste(getwd(),"\\results_all_EC(PC).txt",sep = ""))


# 3.2 Calculate ProtConn bound: looping through all countries

countrynamesvec <- c()
protconnboundvec <- c()
protvec <- c()
countryareavec <- c()

# Obtain number of countries and create loop
countrynames <- unique(gadm@data$GID_0)

for (c in countrynames){
  # Run only for countries with PAs and avoid ANT (antartica)
  if(paste(c,"firstnetwork.txt",sep = "") %in% pctable$V1){
  
    # 3.2.1 Reads PC
    PC1st <- as.numeric(as.character(pctable[pctable$V1==paste(c,"firstnetwork.txt",sep = ""),4]))
    PC2nd <- as.numeric(as.character(pctable[pctable$V1==paste(c,"secondnetwork.txt",sep = ""),4]))
    
    
    
    # 3.2.2 Calculates ProtUnconn[Design] (Saura 2018 Appendix B.3)
    # Needs country area
    countryarea <- area(subset(gadm, GID_0==c))
    
    protunconndes <- (100*(PC1st[1]/countryarea)) - (100*(PC2nd[1]/countryarea))

    # 3.4 Calculates ProtConnBound; ProtConnBound=Prot−ProtUnconn[Design]
    # Needs the total protected area of considered dataset
    areatable <- read.table(paste("nodes_",c,"firstnetwork.txt",sep = ""))
    protarea <- sum(areatable[,2])
      
    protconnbound <- 100*(protarea/countryarea) - protunconndes
    
    countrynamesvec <- append(countrynamesvec, c) 
    protconnboundvec <- append(protconnboundvec, protconnbound)
    protvec <- append(protvec,100*(protarea/countryarea))
    countryareavec <- append(countryareavec,countryarea)
  
  }
}

# 4. WRITE RESULTS
protconnboundtable <- data.frame(countrynamesvec,protconnboundvec,protvec,countryareavec)
colnames(protconnboundtable) <- c("country","protconnbound","prot","countryarea")

write.table(protconnboundtable, paste(getwd(),OUTfile,sep=""),
            row.names = F, col.names = T)



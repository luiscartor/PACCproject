# globalneeds.R
# on May 6 2019
# by Luis
# maps the needs for increasing refugia, diversity or protconn by country
##################################################################################################################

library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(plyr)
library(scales)
library(broom)
library(mapproj)


# 1. INPUTS
OUTglobalneedsfolder <- '/home/lcarrasco/Documents/research/protectedareas/globalneeds/'


INccveltable <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/PAsccveltable.txt'
INtopodivtable <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/PAstopodivtable.txt'
INconnectall <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/results/protconnbound_allyears_dis10.txt'


INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INgadmfile <- 'gadm36_0_simplify'
#INgadmfile <- 'gadm36_0_simplify_robinson_buff0'

INgdpfile <- '/home/lcarrasco/Documents/research/protectedareas/data/GDP/IMF_GDPPPP_Data.csv'

# 2. READ DATA
ccveltable <- read.table(INccveltable,header = TRUE)
tdivtable <- read.table(INtopodivtable,header = TRUE)
connectall <- read.table(INconnectall,header = TRUE)

gadm <- readOGR(INgadmfolder, INgadmfile)

#gdp <- read.csv2(INgdpfile, header = TRUE, sep=",", stringsAsFactors=FALSE)


# 3. ANALYSIS
# 3.1 ccvel needs
# Difference between ccvel inside PAs and country mean
ccveltable$pacoundiff <- (((ccveltable$oldpamean*ccveltable$oldpatot)+(ccveltable$pamean*ccveltable$patot))
                         /(ccveltable$oldpatot+ccveltable$patot))-ccveltable$counmean

# Differencpae between ccvel inside PAs and and available land (outside all pas)
ccveltable$paavaildiff <- (((ccveltable$oldpamean*ccveltable$oldpatot)+(ccveltable$pamean*ccveltable$patot))
                           /(ccveltable$oldpatot+ccveltable$patot))-
                          (((ccveltable$outpamean*ccveltable$outpatot)-(ccveltable$pamean*ccveltable$patot))
                           /(ccveltable$outpatot+ccveltable$patot))
  

ccveltable$ccvel_class <- ccveltable$paavaildiff
ccveltable$ccvel_class[ccveltable$ccvel_class>=0] <- 100	# 100 means country needs to improve
ccveltable$ccvel_class[ccveltable$ccvel_class<0] <- 200		# 200 means country doing well


# 3.2 topodiv needs
# Differencpae between topodiv inside PAs and and available land (outside all pas)
tdivtable$paavaildiff <- (((tdivtable$oldpamean*tdivtable$oldpatot)+(tdivtable$pamean*tdivtable$patot))
                           /(tdivtable$oldpatot+tdivtable$patot))-
                          (((tdivtable$outpamean*tdivtable$outpatot)-(tdivtable$pamean*tdivtable$patot))
                           /(tdivtable$outpatot+tdivtable$patot))
  

tdivtable$tdiv_class <- tdivtable$paavaildiff
tdivtable$tdiv_class[tdivtable$tdiv_class>=0] <- 200	# 200 means country doing well
tdivtable$tdiv_class[tdivtable$tdiv_class<0] <- 100	# 100 means country needs to improve


# 3.3 connected protected needs
# Proportion of PA that is well connected
connectall$propconn <- 100*(connectall$protconnbound/connectall$prot)
  

connectall$propconn_class <- connectall$propconn
connectall$propconn_class[connectall$propconn_class>=80] <- 200	# 200 means country doing well
connectall$propconn_class[connectall$propconn_class<80] <- 100	# 100 means country needs to improve


# 3.4 Add needs columns to gadm
colnames(ccveltable)[1] <- "GID_0"
colnames(tdivtable)[1] <- "GID_0"
colnames(connectall)[1] <- "GID_0"

gadm@data <- merge(gadm@data,ccveltable[,c("GID_0","ccvel_class")],all.x=TRUE)
gadm@data <- merge(gadm@data,tdivtable[,c("GID_0","tdiv_class")],all.x=TRUE)
gadm@data <- merge(gadm@data,connectall[,c("GID_0","propconn_class")],all.x=TRUE)

gadm@data$needs <- ifelse(gadm@data$ccvel_class == 200 & gadm@data$tdiv_class == 200 & gadm@data$propconn_class == 200, "allgood",
		ifelse(gadm@data$ccvel_class == 200 & gadm@data$tdiv_class == 200 & gadm@data$propconn_class == 100, "conn",
		ifelse(gadm@data$ccvel_class == 200 & gadm@data$tdiv_class == 100 & gadm@data$propconn_class == 100, "div&conn",
		ifelse(gadm@data$ccvel_class == 100 & gadm@data$tdiv_class == 200 & gadm@data$propconn_class == 200, "ccvel",
		ifelse(gadm@data$ccvel_class == 100 & gadm@data$tdiv_class == 100 & gadm@data$propconn_class == 200, "ccvel&div",
		ifelse(gadm@data$ccvel_class == 200 & gadm@data$tdiv_class == 100 & gadm@data$propconn_class == 200, "div",
		ifelse(gadm@data$ccvel_class == 100 & gadm@data$tdiv_class == 200 & gadm@data$propconn_class == 100, "ccvel&con","needall"
		)))))))



# 4. PLOTS
# 4.1 MAPS: Prepare plots
# Delete Antartica
gadm <- subset(gadm, GID_0 != "ATA")

# Create df for ggplot maps
gadm@data$id <- rownames(gadm@data)

gadm_df <- fortify(gadm, region="id")
gadm_df <- join(gadm_df, gadm@data, by="id")


# Plot topodiv against available lands
# Colours
pal <- c(
  "allgood" = "red",
  "conn" = "orange", 
  "div&conn" = "yellow", 
  "ccvel" = "forestgreen",
  "ccvel&div" = "blue",
  "div" = "darkblue", 
  "ccvel&conn" = "green", 
  "needall" = "magenta" 
)


ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = factor(needs)), color="white",size=0.15) +
  #scale_fill_manual(values = pal, limits = names(pal), na.value = "darkgray")+ 
  scale_fill_viridis(option = "viridis", discrete = TRUE, na.value = "darkgray")+   
labs(fill = "Country \npriority")+
  ggtitle("Protected area priorization priorities against climate change", 
          subtitle = "Connectivity needed if below 80% of PA is connected; 
			\nTopographic diversity increase needed if PAs mean below country available land mean; 
			\nClimate refugia needed if PAs mean ccvel is higher than available land mean")+
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(file=paste(OUTglobalneedsfolder,"map_needs.eps",sep=""))



# 4.2 PIE CHARTS 
# % of countries with certain level of ccvel increase

needs_df <- data.frame(table(gadm@data$needs))
# Reverse order
#outclass_df <- outclass_df[nrow(outclass_df):1, ]
needs_df 

#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(needs_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  #geom_text(aes(y = c(8,15,20,53,75,82,88,100), label=Freq), color = "black", size=10)+
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.5), color = "black", size=10)+
  scale_fill_viridis(option = 'viridis',discrete = TRUE,direction=1)+ 
  coord_polar("y", start = 0)+
  #scale_fill_manual(values = mycols) +
  ggtitle("Number of countries with specific needs")+
  theme_void()+
  theme(legend.title = element_blank(),legend.text = element_text(size=11))+
  guides(fill = guide_legend())

#ggsave(file=paste(OUTglobalneedsfolder,"pie_globalneeds.eps",sep=""))

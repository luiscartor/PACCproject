#topodiv_analysis.R
# on 18th April 2019
# by Luis
# analyze topodiv data extracted from topodivextract.R and plot results
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
OUTtopodivfolder <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/'
INtopodivtable <- '/home/lcarrasco/Documents/research/protectedareas/topodiv/PAstopodivtable.txt'

INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INgadmfile <- 'gadm36_0_simplify'
#INgadmfile <- 'gadm36_0_simplify_robinson_buff0'
  
# 2. READ DATA
tdivtable <- read.table(INtopodivtable,header = TRUE)
gadm <- readOGR(INgadmfolder, INgadmfile)

# 3. ANALYSIS
# Creates column with difference between PAs tdiv mean and outside
tdivtable$difpas_out <- (tdivtable$pamean - tdivtable$outpamean)/1000
tdivtable$difpas_coun <- (tdivtable$pamean - tdivtable$counmean)/1000
tdivtable$difpas_old <- (tdivtable$pamean - tdivtable$oldpamean)/1000

# Add columns to gadm
colnames(tdivtable)[1] <- "GID_0"
gadm@data <- merge(gadm@data,tdivtable[,c("GID_0","difpas_out","difpas_coun","difpas_old")],all.x=TRUE)



# 4. PLOTS
# 4.1 MAPS: Prepare plots
# Delete Antartica
gadm <- subset(gadm, GID_0 != "ATA")

# Create df for ggplot maps
gadm@data$id <- rownames(gadm@data)

gadm_df <- fortify(gadm, region="id")
gadm_df <- join(gadm_df, gadm@data, by="id")

# Plot topodiv against available lands
ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = difpas_out), color="white",size=0.15) +
  scale_fill_viridis(option = 'viridis')+ 
  labs(fill = "Difference in\ntopographic\ndiversity")+
  ggtitle("Topographic diversity differences between new PAs and available land", 
          subtitle = "Difference in mean topographic diversity between PAs established after Aichi Targets and available land, excluding already protected land")+
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(file=paste(OUTtopodivfolder,"map_topodivout.eps",sep=""))

# Plot topodiv against previous pas
ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = difpas_old), color="white",size=0.15) +
  scale_fill_viridis(option = 'viridis')+ 
  labs(fill = "Difference in\ntopographic\ndiversity")+
  ggtitle("Topographic diversity increase in PAs", 
          subtitle = "Difference in mean topographic diversity between PAs established after Aichi Targets and pre-Aichi PAs")+ 
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
#ggsave(file=paste(OUTtopodivfolder,"map_topodivold.eps",sep=""))


# 4.2 PIE CHARTS 
# 4.2.1 New VS out
# % of countries with certain level of top div increase
tdivtable$difpasout_class <- tdivtable$difpas_out
tdivtable$difpasout_class[tdivtable$difpasout_class > 0.25] <- 4
tdivtable$difpasout_class[tdivtable$difpasout_class < 0.25 & tdivtable$difpasout_class >= 0] <- 3
tdivtable$difpasout_class[tdivtable$difpasout_class < 0 & tdivtable$difpasout_class >= -0.25] <- 2
tdivtable$difpasout_class[tdivtable$difpasout_class < -0.25] <- 1
tdivtable$difpasout_class <- as.factor(tdivtable$difpasout_class)

tdivtable$difpasout_class <- revalue(tdivtable$difpasout_class, c("4"="Big increase (> 0.3)", "3"="Small increase (< 0.3)",
                                                                  "2"="Small decrease (< 0.3)", "1"="Big decrease (> 0.3)"))
outclass_df <- data.frame(table(tdivtable$difpasout_class))
# Reverse order
outclass_df <- outclass_df[nrow(outclass_df):1, ]
outclass_df

#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(outclass_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = c(8,40,87,107), label=Freq), color = "white", size=10)+
  scale_fill_viridis(option = 'viridis',discrete = TRUE)+ 
  coord_polar("y", start = 0)+
  #scale_fill_manual(values = mycols) +
  ggtitle("Number of countries which PAs present certain of\ntopographic diversity differences respect to the available land")+
  theme_void()+
  theme(legend.title = element_blank(),legend.text = element_text(size=11))+
  guides(fill = guide_legend(reverse=TRUE))

#ggsave(file=paste(OUTtopodivfolder,"pie_topodivout.eps",sep=""))
                                
# 4.2.1 New VS outew VS OLD
# % of countries with certain level of top div increase
tdivtable$difpasout_class <- tdivtable$difpas_old
tdivtable$difpasout_class[tdivtable$difpasout_class > 0.25] <- 4
tdivtable$difpasout_class[tdivtable$difpasout_class < 0.25 & tdivtable$difpasout_class >= 0] <- 3
tdivtable$difpasout_class[tdivtable$difpasout_class < 0 & tdivtable$difpasout_class >= -0.25] <- 2
tdivtable$difpasout_class[tdivtable$difpasout_class < -0.25] <- 1
tdivtable$difpasout_class <- as.factor(tdivtable$difpasout_class)

tdivtable$difpasout_class <- revalue(tdivtable$difpasout_class, c("4"="Big increase (> 0.3)", "3"="Small increase (< 0.3)",
                                                                  "2"="Small decrease (< 0.3)", "1"="Big decrease (> 0.3)"))
outclass_df <- data.frame(table(tdivtable$difpasout_class))
# Reverse order
outclass_df <- outclass_df[nrow(outclass_df):1, ]
outclass_df

#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(outclass_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = c(7,25,72,106), label=Freq), color = "white", size=10)+
  scale_fill_viridis(option = 'viridis',discrete = TRUE)+ 
  coord_polar("y", start = 0)+
  #scale_fill_manual(values = mycols) +
  ggtitle("Number of countries which PAs present certain of\ntopographic diversity differences respect to old PAs")+
  theme_void()+
  theme(legend.title = element_blank(),legend.text = element_text(size=11))+
  guides(fill = guide_legend(reverse=TRUE))

#ggsave(file=paste(OUTtopodivfolder,"pie_topodivold.eps",sep=""))
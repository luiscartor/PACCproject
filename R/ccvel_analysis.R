# ccvel_analysis.R
# on 25th April 2019
# by Luis
# analyze climate change velocity data extracted from ccvelextract.R and plot results
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
library(shades)


# 1. INPUTS
OUTccvelfolder <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/'
INccveltable <- '/home/lcarrasco/Documents/research/protectedareas/ccvel/PAsccveltable.txt'

INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INgadmfile <- 'gadm36_0_simplify'
#INgadmfile <- 'gadm36_0_simplify_robinson_buff0'

INgdpfile <- '/home/lcarrasco/Documents/research/protectedareas/data/GDP/IMF_GDPPPP_Data.csv'

# 2. READ DATA
ccveltable <- read.table(INccveltable,header = TRUE)
gadm <- readOGR(INgadmfolder, INgadmfile)

gdp <- read.csv2(INgdpfile, header = TRUE, sep=",", stringsAsFactors=FALSE)


# 3. ANALYSIS
# Creates column with difference between PAs ccvel mean and outside
ccveltable$difpas_out <- (ccveltable$pamean - ccveltable$outpamean)
ccveltable$difpas_coun <- (ccveltable$pamean - ccveltable$counmean)
ccveltable$difpas_old <- (ccveltable$pamean - ccveltable$oldpamean)

ccveltable$reldif <- ccveltable$difpas_out/(ccveltable$outpamean)
ccveltable$relcoundif <- ccveltable$difpas_out/(ccveltable$counmean)

# Add columns to gadm
colnames(ccveltable)[1] <- "GID_0"
gadm@data <- merge(gadm@data,ccveltable[,c("GID_0","difpas_out","difpas_coun","difpas_old","reldif","relcoundif")],all.x=TRUE)



# 4. PLOTS
# 4.1 MAPS: Prepare plots
# Delete Antartica
gadm <- subset(gadm, GID_0 != "ATA")

# Create df for ggplot maps
gadm@data$id <- rownames(gadm@data)

gadm_df <- fortify(gadm, region="id")
gadm_df <- join(gadm_df, gadm@data, by="id")

# Plot ccvel against available lands
ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = reldif), color="white",size=0.15) +
  scale_fill_viridis(option = 'magma',trans='reverse')+ 
  labs(fill = "Adjusted \ndifference in\nclimate change\nvelocity (log(km/yr))")+
  ggtitle("Climate change velocity adjusted mean decrease between new PAs and available land", 
          subtitle = "Difference in mean CCvel between PAs established after Aichi Targets and available land, excluding already protected land")+
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(file=paste(OUTccvelfolder,"map_ccvelout_adjusted.eps",sep=""))

# Plot ccvel against previous pas
ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = difpas_old), color="white",size=0.15) +
  scale_fill_viridis(option = 'magma',trans = 'reverse')+ 
  labs(fill = "Difference in\nclimate change\nvelocity (log(km/yr))")+
  ggtitle("Climate change velocity mean decrease in PAs", 
          subtitle = "Difference in mean CCvel between PAs established after Aichi Targets and pre-Aichi PAs")+ 
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(file=paste(OUTccvelfolder,"map_ccvelold.eps",sep=""))


# 4.2 PIE CHARTS 
# 4.2.1 New VS out
# % of countries with certain level of ccvel increase
ccveltable$difpasout_class <- ccveltable$difpas_out
ccveltable$difpasout_class[ccveltable$difpasout_class > 50] <- 400
ccveltable$difpasout_class[ccveltable$difpasout_class <= 50 & ccveltable$difpasout_class >= 0] <- 300
ccveltable$difpasout_class[ccveltable$difpasout_class < 0 & ccveltable$difpasout_class >= -50] <- 200
ccveltable$difpasout_class[ccveltable$difpasout_class < -50] <- 100
ccveltable$difpasout_class <- as.factor(ccveltable$difpasout_class)

ccveltable$difpasout_class <- revalue(ccveltable$difpasout_class, c("400"="Big increase (> 50)", "300"="Small increase (< 50)",
                                                                  "200"="Small decrease (< 50)", "100"="Big decrease (> 50)"))
outclass_df <- data.frame(table(ccveltable$difpasout_class))
# Reverse order
#outclass_df <- outclass_df[nrow(outclass_df):1, ]
outclass_df 

#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(outclass_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = c(6,27,73,102), label=Freq), color = "grey", size=10)+
  scale_fill_viridis(option = 'magma',discrete = TRUE,direction=-1)+ 
  coord_polar("y", start = 0)+
  #scale_fill_manual(values = mycols) +
  ggtitle("Number of countries which PAs present certain of\nCCvel differences respect to the available land")+
  theme_void()+
  theme(legend.title = element_blank(),legend.text = element_text(size=11))+
  guides(fill = guide_legend())

#ggsave(file=paste(OUTccvelfolder,"pie_ccvelout.eps",sep=""))

# 4.2.1 New VS outew VS OLD
# % of countries with certain level of ccvel increase
ccveltable$difpasout_class <- ccveltable$difpas_old
ccveltable$difpasout_class[ccveltable$difpasout_class > 50] <- 400
ccveltable$difpasout_class[ccveltable$difpasout_class < 50 & ccveltable$difpasout_class >= 0] <- 300
ccveltable$difpasout_class[ccveltable$difpasout_class < 0 & ccveltable$difpasout_class >= -50] <- 200
ccveltable$difpasout_class[ccveltable$difpasout_class < -50] <- 100
ccveltable$difpasout_class <- as.factor(ccveltable$difpasout_class)

ccveltable$difpasout_class <- revalue(ccveltable$difpasout_class, c("400"="Big increase (> 50)", "300"="Small increase (< 50)",
                                                                  "200"="Small decrease (< 50)", "100"="Big decrease (> 50)"))
outclass_df <- data.frame(table(ccveltable$difpasout_class))
# Reverse order
#outclass_df <- outclass_df[nrow(outclass_df):1, ]
outclass_df

#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(outclass_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = c(8,37,78,103), label=Freq), color = "grey", size=10)+
  scale_fill_viridis(option = 'magma',discrete = TRUE)+ 
  coord_polar("y", start = 0)+
  #scale_fill_manual(values = mycols) +
  ggtitle("Number of countries which PAs present certain of\nCCvel differences respect to old PAs")+
  theme_void()+
  theme(legend.title = element_blank(),legend.text = element_text(size=11))+
  guides(fill = guide_legend(reverse=TRUE))

#ggsave(file=paste(OUTccvelfolder,"pie_ccvelold.eps",sep=""))


# 4.3 New PAs increase VS total increased area
#ccveltable$outweighted <- ccveltable$difpas_out/ccveltable$patot
# Plot PAs ccvel difference with available land, against total new PA
ccvel_df <- data.frame(ccveltable)
plot(log10(ccveltable$patot),ccveltable$difpas_out)
l
ggplot(ccvel_df, aes(y=difpas_out, x=log10(patot)))+
  geom_point(aes(size = countot, colour=counmean)) + 
  geom_text(data = subset(ccvel_df, difpas_out > 50 | difpas_out < -50), aes(label=GID_0), size=4, hjust = -0.27)+
  scale_size_continuous(range=c(1,20),
                        name= expression("Country's total\nwild area (km"^2*")"), breaks=c(1e+3,1e+5,1e+7))+
  scale_colour_viridis(name="Country's mean\nClimate Change\nvelocity (km/year)\n(at wild areas)")+
  ylab("Difference in CCvel between new PAs and available land (km/year)")+ 
  xlab(expression("Area of new PAs (log(km"^2*"))"))

#ggsave(file=paste(OUTccvelfolder,"scatter_outVSpaarea.eps",sep=""))


# 4.4 GDP analysis
for(i in 2:ncol(gdp)){gdp[,i]<-as.numeric(gdp[,i])}

# Obtain gpd average from 2011 to 2017
gdp$gdpave <- rowMeans(gdp[,c(12:18)],na.rm=TRUE)

# Merge to ccvel data
ccvel_df <- merge(ccvel_df, gdp[, c("GID_0", "gdpave")], by="GID_0")

# Plot
ggplot(ccvel_df, aes(y=difpas_out, x=gdpave))+
  geom_point(aes(size = countot, colour=counmean)) + 
  geom_text(data = subset(ccvel_df, difpas_out > 50 | difpas_out < -50), aes(label=GID_0), size=4, hjust = -0.27)+
  scale_size_continuous(range=c(1,20),
                        name= expression("Country's total\nwild area (km"^2*")"), breaks=c(1e+3,1e+5,1e+7))+
  scale_colour_viridis(name="Country's mean\nClimate Change\nvelocity (km/year)\n(at wild areas)")+
  ylab("Difference in CCvel between new PAs and available land (km/year)")+ 
  xlab(expression("Average (2011-2017) nominal GDP (billion dollars)"))

#ggsave(file=paste(OUTccvelfolder,"scatter_outVSpaarea.eps",sep=""))

lm <- lm(ccvel_df$difpas_out ~ ccvel_df$gdpave +  ccvel_df$patot + ccvel_df$counmean + ccvel_df$countot)
summary(lm)





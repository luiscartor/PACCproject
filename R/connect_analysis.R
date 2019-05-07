# connect_analysis.R
# on 3 May 2019
# by Luis
# compares connectivity (protconnbound) between 2011 and present

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
OUTconnectfolder <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/results/'
INtable2010 <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/results/protconnbound_till2010_dis10.txt'
INtableall <- '/home/lcarrasco/Documents/research/protectedareas/connectivity/results/protconnbound_allyears_dis10.txt'


INgadmfolder <- '/home/lcarrasco/Documents/research/protectedareas/data/GADM/'
INgadmfile <- 'gadm36_0_simplify'
#INgadmfile <- 'gadm36_0_simplify_robinson_buff0'

#INgdpfile <- '/home/lcarrasco/Documents/research/protectedareas/data/GDP/IMF_GDPPPP_Data.csv'

# 2. READ DATA
table_2010 <- read.table(INtable2010,header = TRUE)
table_all <- read.table(INtableall,header = TRUE)

gadm <- readOGR(INgadmfolder, INgadmfile)

#gdp <- read.csv2(INgdpfile, header = TRUE, sep=",", stringsAsFactors=FALSE)


# 3. ANALYSIS
# Creates column with proportion of connected/protected
table_2010$connprop <- 100*(table_2010$protconnbound/table_2010$prot)
table_all$connprop <- 100*(table_all$protconnbound/table_all$prot)

# Select common countries (can be the case of countries without PA in 2010 but present in allyears)
commoncoun <- intersect(table_2010$country, table_all$country)
table_2010 <- table_2010[table_2010$country %in% commoncoun,]
table_all <- table_all[table_all$country %in% commoncoun,]

table_all$propdiff <- (table_all$connprop - table_2010$connprop)
table_all$diff <- (table_all$protconnbound - table_2010$protconnbound)
table_all$oldprotconnbound <- table_2010$protconnbound


# Add columns to gadm
colnames(table_all)[1] <- "GID_0"
gadm@data <- merge(gadm@data,table_all[,c("GID_0","propdiff","diff")],all.x=TRUE,all=TRUE)


# 4. PLOTS
# 4.1 MAPS: Prepare plots
# Delete Antartica
gadm <- subset(gadm, GID_0 != "ATA")

# Create df for ggplot maps
gadm@data$id <- rownames(gadm@data)

gadm_df <- fortify(gadm, region="id")
gadm_df <- join(gadm_df, gadm@data, by="id")

# We want log(diff)
# First we put negative values and zeroes to very small values
gadm_df$logdiff <- gadm_df$diff
gadm_df$logdiff[gadm_df$logdiff <= 0] <- 0.00000001
# Then we transform to log
gadm_df$logdiff <- log(gadm_df$logdiff)

# Ticks for log-transformed legend
logbreaks <- c(log(20),log(1),log(0.01),log(0.00000001))
loglabels <- c(20,1,0.01,0)

# Plot protconn diff 
ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = logdiff), color="white",size=0.15) +
  scale_fill_viridis(option = 'plasma', breaks=logbreaks, labels=loglabels)+ 
  labs(fill = "Difference in\nconnected\nand protected (%)")+
  ggtitle("Changes in protected and connected areas from 2011 to present", 
          subtitle = "Difference in % of protected and connected area using the protconnbound index")+
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(file=paste(OUTconnectfolder,"map_protconndif.eps",sep=""))



# PROPDIFF PLOT 
# Transform propdiff values
gadm_df$logpropdiff <- asinh(gadm_df$propdiff)

# Then we transform to log
gadm_df$logpropdiff <- log(abs(gadm_df$propdiff+0.0000001))



gadm_df$logpropdiff <- sign(gadm_df$propdiff+0.0000001)*(gadm_df$logpropdiff+17)

# Ticks for log-transformed legend
logbreaks <- c(asinh(20),asinh(2),asinh(0),asinh(-2),asinh(-15))
loglabels <- c(20,2,0,-2,-15)


# Plot percentage of protconn diff between years
ggplot(gadm_df) +
  geom_polygon(aes(long, lat, group=group, fill = logpropdiff), color="white",size=0.15) +
  scale_fill_viridis(option = 'plasma', breaks=logbreaks, labels=loglabels)+ 
  labs(fill = "Difference in proportion \nof connected PA in \nrespect of total \nprotected land (%)")+
  ggtitle("Changes in proportion of connected areas in respect of total protected areas from 2011 to present", 
          subtitle = "Difference in % proportion of connected areas using the protconnbound index")+
  theme_bw()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(file=paste(OUTconnectfolder,"map_connproportiondif.eps",sep=""))




# 4.2 Scatter plot New PAs increase VS total increased area
#tdivtable$outweighted <- tdivtable$difpas_out/tdivtable$patot
# Plot PAs topdiv difference with available land, against total new PA
table_all_df <- data.frame(table_all)



ggplot(table_all_df, aes(y=diff, x=prot))+
  geom_point(aes(size = countryarea, colour=connprop)) + 
  ylim(-3, 30)+
  xlim(-5,100)+
  geom_text(data = subset(table_all_df, diff > 8 | prot > 66), aes(label=country), size=4, hjust = -0.27)+
  scale_size_continuous(range=c(1,30),
                        name= expression("Country's total\n area (km"^2*")"), breaks=c(1e+11,1e+12,1e+13))+
  scale_colour_viridis(name="Proportion of\nconnected\nand total\n(areas)")+
  ylab("Difference in connected and protected between 2010 and present")+ 
  xlab(expression("Country protected area (%)"))

#ggsave(file=paste(OUTconnectfolder,"scatter_diffVSpaarea.eps",sep=""))



# 4.3 PIE charts
# 4.2.1 Diff in protconn
# % of countries with certain level of ccvel increase
table_all_df$diff_class <- table_all_df$diff
table_all_df$diff_class[table_all_df$diff_class > 10] <- 400
table_all_df$diff_class[table_all_df$diff_class <= 10 & table_all_df$diff_class >= 1] <- 300
table_all_df$diff_class[table_all_df$diff_class < 1 & table_all_df$diff_class >= 0.1] <- 200
table_all_df$diff_class[table_all_df$diff_class < 0.1] <- 100
table_all_df$diff_class <- as.factor(table_all_df$diff_class)

table_all_df$diff_class <- revalue(table_all_df$diff_class, c("400"="Increase > 10", "300"="Increase > 1",
                                                                    "200"="Increase > 0.1", "100"="Increase < 0.1"))
diffclass_df <- data.frame(table(table_all_df$diff_class))
# Reverse order
#outclass_df <- outclass_df[nrow(outclass_df):1, ]
diffclass_df 

#mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(diffclass_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = c(0,19,43,132), label=Freq), color = "darkgray", size=10)+
  scale_fill_viridis(option = 'plasma',discrete = TRUE,direction=-1)+ 
  coord_polar("y", start = 0)+
  #scale_fill_manual(values = mycols) +
  ggtitle("Number of countries certain increase in connected and protected area")+
  theme_void()+
  theme(legend.title = element_blank(),legend.text = element_text(size=11))+
  guides(fill = guide_legend())

#ggsave(file=paste(OUTconnectfolder,"pie_conndiff.eps",sep=""))

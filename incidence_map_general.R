###### Script for incidence map ##########

# load libraries
library(classInt)
library(maptools)
library(RColorBrewer)
library(rgeos)
library(ggplot2)
library(sp)


# set your working directory
setwd("set/your/working/directory")

# load cases in SpatialPoints- format (shapefile)
cases_ppp<- readShapeSpatial("cases_ppp.shp")

# read regional shapefiles with number of population (SpatialPolygons)
gem<- readShapeSpatial("gem.shp") #does include population numbers
gem_f<- fortify(gem, region="gemnr")

# just for plotting the provinces
prov<- readShapeSpatial("Provincie.shp")
prov_f<- fortify(prov, region="geb_naam")

# check which cases are in which region
gem_cases<- over(cases_ppp, gem)
# count cases per region
gem_cases<- count(gem_cases$gemnr)
colnames(gem_cases)<- c("gemnr", "Freq")
# merge with original shapefile of region
gem_merge<- merge(gem, gem_cases, by="gemnr")
# only select regions with population
gem_merge<- subset(gem_merge, inw>0)
# set region with no cases to 0 cases instead of NA
gem_merge@data$Freq[is.na(gem_merge@data$Freq)]<- 0
# calculate the incidence per region per 100,000 population
gem_merge@data$inc<- round(gem_merge@data$Freq/gem_merge@data$inw*100000, 2)
# set NA incidence to 0 
gem_merge@data$inc<- ifelse(is.na(gem_merge@data$inc), 0, gem_merge@data$inc)

# categorize incidence by jenks' breaks
inc.df<- data.frame(id= gem_merge@data$gemnr, inc= gem_merge@data$inc)
jenks<- classIntervals(inc.df$inc[inc.df$inc!=0], n=5, style="jenks")
jenks
# copy the class labels below in the labels statement
inc.df$jenks<- cut(inc.df$inc, breaks=c(-1, jenks$brks),
                          labels=c("0", "[0.32,2.42]", "(2.42,4.56]", "(4.56,8.25]",  "(8.25,12.94]", "(12.94,16.82]"))
# add incidence classes to region file
gem_f<- merge(gem_f, inc.df, by="id", all.x=T)


# pick color scheme
cols<- c("white", brewer.pal(n=6, "Reds"))

# for plotting the north arrow
northarrowdf<- data.frame(x= c(min(prov_f$long)), y=c(min(prov_f$lat)+10000), arrow.length=c(20000))

# do the plotting
incidencemap<- ggplot(prov_f, aes(x = long, y = lat)) +
  # create layers for shading of the map
  geom_polygon(data=prov_f, aes(x=long+3000, y= lat-3000, group=group), fill="gray60", colour=NA, alpha=0.1)+
  geom_polygon(data=prov_f, aes(x=long+2500, y= lat-2500, group=group), fill="gray60", colour=NA, alpha=0.2)+
  geom_polygon(data=prov_f, aes(x=long+2000, y= lat-2000, group=group), fill="gray60", colour=NA, alpha=0.5)+
  geom_polygon(data=prov_f, aes(x=long+1500, y= lat-1500, group=group), fill="gray60", colour=NA, alpha=0.7)+
  geom_polygon(data=prov_f, aes(x=long+1000, y= lat-1000, group=group), fill="gray60", colour=NA, alpha=0.9)+
  # add regional polygons with incidence
  geom_polygon(data=gem_f, aes(x=long, y=lat, group=group, fill=jenks, colour=jenks), size=0.1)+
  scale_fill_manual(values=cols, name="Incidence/\n100,000 population", drop=F)+
  scale_colour_manual(values=cols, name="Incidence/\n100,000 population", drop=F)+
  # add the province boundaries
  geom_polygon(data=prov_f, aes(x= long, y=lat, group = group), colour="gray30", fill=NA,  size=0.3)+
  # equal distance lat and long
  coord_equal()+
  # layout
  theme_bw()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y= element_blank(),
        axis.ticks.x= element_blank(),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        panel.border= element_blank(),
        legend.text= element_text(size=6),
        legend.title= element_text(size=6),
        legend.key.size = unit(0.5, "cm"),
        legend.margin=unit(0, "lines"))+
  theme(legend.position=c(0,1), legend.justification=c(0.1,1))+
  # add north arrow
  geom_segment(data = northarrowdf, aes(x = x, y = y, xend = x, yend = y+arrow.length),
               arrow = arrow(length = unit(0.1,"cm")), size=0.1)+
  annotate("text", label = "N", x = northarrowdf$x , y = (northarrowdf$y+ (0.5*northarrowdf$arrow.length)), size = 4, colour = "black")+
  # add scale bar
  geom_rect(data= prov_f, aes(xmin= min(long), xmax=min(long)+50000, ymin=min(lat), ymax=min(lat)+5000), col="black", fill="white", size=0.1)+
  geom_rect(data= prov_f, aes(xmin= min(long)+50000, xmax=min(long)+100000, ymin=min(lat), ymax=min(lat)+5000), col="black", fill="black", size=0.1)+
  annotate("text", label = c("0km","50km", "100km"), x = c(min(prov_f$long), min(prov_f$long)+50000, min(prov_f$long)+100000) , 
           y = c(min(prov_f$lat)-5000, min(prov_f$lat)-5000, min(prov_f$lat)-5000), size = 2, colour = "black")


# write map to pdf
pdf("incidence_map.pdf")
incidencemap
dev.off()
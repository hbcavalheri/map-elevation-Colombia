library(ggplot2)
library(raster)
library(rgdal)

library(grid)
library(ggmap)
library(dplyr)
library(rasterVis)
library(scales)
library(rgeos)

#load map data
mapa1 <- map_data("world")
mapa<-subset(mapa1, region %in% c("Brazil", "Uruguay", "Argentina", "French Guiana",
                                  "Suriname", "Colombia", "Venezuela","Bolivia",
                                  "Ecuador", "Chile", "Paraguay", "Peru", "Guyana"))
mapa_pais<-subset(mapa1, region %in% c("Colombia"))

#### just to see if adding points works
test.data<-data.frame("lake"=c("tota","bogota"),"lat"=c(5.5446,4.711),
                      "long"=c(-72.9283,-74.0721))


# Extent rectangle for inset map
pol<-data.frame(xmin=121.7,xmax=122.2 ,ymin=13 ,ymax=13.7)

## main map
p1<-ggplot()
p1<-p1 + geom_polygon(data=mapa_pais,aes(x=long, y=lat, group = group),
                      colour="black", fill="white") +
    theme(axis.text=element_text(size=10,face="bold",color="black"),
          axis.title=element_text(size=10,face="bold",color="black")) +
    labs(x="Longitude",y="Latitude") +
    geom_point(data=test.data, aes(x=long,y=lat),col="red",size=3) +
    theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour="black"),
          panel.background = element_rect(fill='transparent',colour=NA))
p1

## south america inset map
p2 <- ggplot()
p2 <- p2 + geom_polygon(data=mapa,aes(x=long, y=lat, group = group),
                                     colour="black", fill="white") +
    coord_map(xlim = c(-100, -30),ylim = c(-60,15)) +
    geom_polygon(data=mapa_pais, aes(x=long, y=lat, group = group),
                  colour="black", fill="black" ) +
    theme(panel.background = element_rect(fill='transparent',colour=NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
    )

p2

## creating figure
png(file="map_colombia.png",w=1800,h=1800,res=300)
grid.newpage()
v1<-viewport(width=1,height=1,x=0.5,y=0.5)
v2<-viewport(width=0.3,height=0.3,x=0.88,y=0.85) 
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()

################################################################################
################################################################################

# topographic map

r1<-raster('srtm_21_11.tif')
r2<-raster('srtm_21_12.tif')
r3<-raster('srtm_21_13.tif')
r4<-raster('srtm_22_11.tif')
r5<-raster('srtm_22_12.tif')
r6<-raster('srtm_22_13.tif')
r7<-raster('srtm_23_11.tif')
r8<-raster('srtm_23_12.tif')
r9<-raster('srtm_23_13.tif')
r10<-raster('srtm_22_10.tif')

col<-getData('GADM',country='COL',level=0)
dem<-merge(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)

## the next two steps are gonna take time...
dem_lower_res<-aggregate(dem,fact=10) 
elevation.sub<-mask(dem_lower_res,col)

dem.p<-rasterToPoints(elevation.sub) 
df<-data.frame(dem.p)
colnames(df)<-c("lon","lat","alt")


p3<-ggplot(df,aes(lon,lat)) +
    geom_raster(aes(fill=alt)) +
    scale_fill_gradientn(colours=terrain.colors(6)) +
    theme(axis.text=element_text(size=10,face="bold",color="black"),
          axis.title=element_text(size=10,face="bold",color="black")) +
    theme(panel.grid.minor = element_blank(),panel.grid.major=element_blank(),
          plot.background = element_rect(fill="transparent",colour=NA),
          axis.line = element_line(colour="black"),
          panel.background = element_rect(fill='transparent',colour=NA)) +
    geom_point(data=test.data, aes(x=long,y=lat),col="red",size=3) +
    labs(x="Longitude",y="Latitude") +
    guides(fill=guide_legend(title="Elevation (m)"))

p3 # it takes awhile to plot


png(file="map_colombia_topo.png",w=1800,h=1800,res=300)
grid.newpage()
v3<-viewport(width=1,height=1,x=0.5,y=0.5)
v2<-viewport(width=0.3,height=0.3,x=0.88,y=0.85) 
print(p3,vp=v3) 
print(p2,vp=v2)
dev.off()


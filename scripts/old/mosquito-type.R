library(raster)
library(rgdal)
library(RColorBrewer)
library(scales)


aegypti_map<-raster("Aedes_maps_public/aegypti.tif")
albopictus_map<-raster("Aedes_maps_public/albopictus.tif")
diff_map<-aegypti_map-albopictus_map

png(file="mosqplot.png", height=12, width=8, unit="in", res=300)
par(mfrow=c(3,2))
plot(aegypti_map, main="A. aegypti", axes=0, box=FALSE, 
     col=brewer.pal(9, "Greens"))
plot(albopictus_map, main="B. albopictus", axes=0, box=FALSE, 
     col=brewer.pal(9, "Purples"))


plot(diff_map, main="C. aegypti - albopictus", axes=0, box=FALSE, 
     col=brewer.pal(9, "PRGn"))
plot(aegypti_map+albopictus_map, main="C. aegypti + albopictus", axes=0, box=FALSE, 
     col=brewer.pal(9, "Blues"))

#4 - high both
#3 - high aegypti, low albopictus
#2 - low aegpyti, high albopictus
#1 - low both
discrete_map<-ifelse(as.matrix(aegypti_map)>.25, ifelse(as.matrix(albopictus_map)>.25, 4, 3), ifelse(as.matrix(albopictus_map)>.25, 2, 1))
plot(raster(discrete_map), main="E. Discrete Palette", axes=0, box=FALSE, 
     col=c("gray", "purple", "green", "blue"))
dev.off()

more_aedes<-raster(ifelse(as.matrix(aegypti_map-albopictus_map)>.1, 1, 0))
more_albopictus<-raster(ifelse(as.matrix(albopictus_map-aegypti_map)>.1, 1, 0))
has_mosq<-raster(ifelse(as.matrix(albopictus_map+aegypti_map)>0, 1, 0))

more_aedes<-crs(aedes_map)
country_sf<-readOGR("World_Countries__Generalized_", dsn=".")
#mean_aedes<-extract(aedes_map, country_sf, fun=mean, na.rm=TRUE)
#mean_albopictus<-extract(albopictus_map, country_sf, fun=mean, na.rm=TRUE)

jpeg(file="aedvalb.jpeg")
plot(as.vector(aedes_map), as.vector(albopictus_map), col=alpha(rgb(0,0,0), 0.01))
dev.off()
#do we want to treat no-either countries same as high-both
#what if instead: term for aedes, term for albopictus, and interaction
#should this be population-scaled

pct_aegypti<-aegypti_map/(aegypti_map+albopictus_map)
crs(pct_aegypti)<-CRS("+proj=longlat +datum=WGS84")


png(file="aed_pct.png", height=12, width=8, unit="in", res=300)
plot(pct_aegypti, main="% Aedes", axes=0, box=FALSE, 
     col=brewer.pal(9, "PRGn"))
dev.off()

#plot w diff cutoffs
png(file="aed_cutoffs.png", height=12, width=8, unit="in", res=300)
par(mfrow=c(2,2))

discrete_map_five<-ifelse(as.matrix(aegypti_map)>.05, ifelse(as.matrix(albopictus_map)>.05, 4, 3), ifelse(as.matrix(albopictus_map)>.05, 2, 1))
plot(raster(discrete_map_five), main=paste("Cutoff: 0.05"), axes=0, box=FALSE, 
     col=c("gray", "purple", "green", "blue"))

discrete_map_one<-ifelse(as.matrix(aegypti_map)>.1, ifelse(as.matrix(albopictus_map)>.1, 4, 3), ifelse(as.matrix(albopictus_map)>.1, 2, 1))
plot(raster(discrete_map_one), main=paste("Cutoff: 0.1"), axes=0, box=FALSE, 
     col=c("gray", "purple", "green", "blue"))

discrete_map_quarter<-ifelse(as.matrix(aegypti_map)>.25, ifelse(as.matrix(albopictus_map)>.25, 4, 3), ifelse(as.matrix(albopictus_map)>.25, 2, 1))
plot(raster(discrete_map_quarter), main=paste("Cutoff: 0.25"), axes=0, box=FALSE, 
     col=c("gray", "purple", "green", "blue"))

discrete_map_half<-ifelse(as.matrix(aegypti_map)>.5, ifelse(as.matrix(albopictus_map)>.5, 4, 3), ifelse(as.matrix(albopictus_map)>.5, 2, 1))
plot(raster(discrete_map_one), main=paste("Cutoff: 0.5"), axes=0, box=FALSE, 
     col=c("gray", "purple", "green", "blue"))

dev.off()


rclmat_aeg<-matrix(c(0, .1, 0, .1, Inf, 1), ncol=3, byrow=TRUE)
discrete_aegypti<-reclassify(aegypti_map, rclmat_aeg)

rclmat_alb<-matrix(c(0, .1, 0, .1, Inf, 2), ncol=3, byrow=TRUE)
discrete_albopictus<-reclassify(albopictus_map, rclmat_alb)

discrete_map<-discrete_albopictus+discrete_aegypti

writeRaster(discrete_map, filename=file.path("discrete_map.tif"),
            format="GTiff", overwrite=TRUE)

writeRaster(pct_aegypti, filename=file.path("pct_aegypti.tif"),
            format="GTiff", overwrite=TRUE)

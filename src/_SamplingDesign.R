## 
# Title:      FLAB Sampling Scheme
# Objective:  Download remote sensing products from NOAA
# Created by: Paul Julian; pjulian@sccf.org
# Created on: 08/08/2022
## 
## 

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)

# GIS libraries 
# library(sp)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

library(tmap)
tmap_mode("view")

#Paths
wd="C:/Julian_LaCie/_GitHub/FLAB_Algae"

paths=paste0(wd,c("/Plots/","/Export/","/Data","/GIS","/src/","/_documents/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"

# Helper variables
# epsg.io
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")
wgs84=CRS("+init=epsg:4326")
# -------------------------------------------------------------------------

shore=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),utm17)
FLAB=spTransform(readOGR(paste0(GIS.path,"/FATHOM_segs"),"FATHOM_segs"),utm17)

focusArea=spTransform(readOGR(paste0(GIS.path,"/OtherAreas"),"Downstream_Focus"),utm17)
# plot(FLAB)

FLAB.basins=c("Rankin Lake","Snake Bight","Conchie Channel","Pelican Keys",
              "Catfish Key","Johnson Key","Dildo Key Bank","Sid Key","First National Bank")
study_basin=subset(FLAB,NAME%in%FLAB.basins)

plot(FLAB)
plot(study_basin,add=T,col="green")
plot(focusArea,add=T)


study_basin$area.m2=gArea(study_basin,byid=T)

study_basin$area.m2/20


# Stratified random sampling ----------------------------------------------
## Make hex grid
# set.seed(12)
# grd=spsample(study_basin,type="hexagonal",cellsize=2000)
# hex.grd=HexPoints2SpatialPolygons(grd)
# proj4string(hex.grd)=wkt(utm17)
# 
# grd=spsample(focusArea,type="hexagonal",cellsize=100)
# hex.grd.focus=HexPoints2SpatialPolygons(grd)
# proj4string(hex.grd.focus)=wkt(utm17)
# 
# plot(hex.grd)
# plot(FLAB,add=T)
# plot(hex.grd.focus,add=T,col="red")
# 
# pid <- sapply(slot(hex.grd, "polygons"), function(x) slot(x, "ID"))
# hex.grd=SpatialPolygonsDataFrame(hex.grd,data.frame(ID=1:length(hex.grd),row.names=pid))
# hex.grd=raster::intersect(hex.grd,study_basin)
# 
# # https://rdrr.io/cran/spatialEco/man/stratified.random.html
# basin_sample=spatialEco::stratified.random(hex.grd,strata="NAME",n=8,reps=3,replace=T)
# basin_sample.pt=gCentroid(basin_sample,byid = T)
# # basin_sample2=spatialEco::stratified.random(hex.grd,strata="area.m2",n=10,reps=3,replace=F)
# plot(basin_sample,add=T,col="grey")
# # plot(basin_sample2,add=T,col="blue")
# # plot(spsample(study_basin,n=270,"random"),add=T,pch=21,bg="red")
# 
# tapply(basin_sample@data$NAME, basin_sample@data$NAME, length)
# 
# pid <- sapply(slot(hex.grd.focus, "polygons"), function(x) slot(x, "ID"))
# hex.grd.focus=SpatialPolygonsDataFrame(hex.grd.focus,data.frame(ID=1:length(hex.grd.focus),row.names=pid))
# hex.grd.focus=raster::intersect(hex.grd.focus,focusArea)
# 
# focus_sample=spatialEco::stratified.random(hex.grd.focus,strata="Region",n=5,reps=3,replace=T)
# focus_sample.pt=gCentroid(focus_sample,byid = T)
# plot(focusArea)
# plot(focus_sample,add=T,col="red")
# 
# plot(study_basin)
# plot(basin_sample.pt,pch=21,bg="red",add=T)
# plot(focus_sample.pt,pch=21,bg="blue",add=T)
# 
# length(basin_sample.pt)
# length(focus_sample.pt)
# 
# length(basin_sample.pt)+length(focus_sample.pt)


# Uniform sampling --------------------------------------------------------
set.seed(1)
basin_sample_reg=spsample(study_basin,n=100,"regular")
diff(coordinates(basin_sample_reg)[,1])
focus_sample_reg=spsample(focusArea,n=105,"regular")

basin_sample_reg=SpatialPointsDataFrame(basin_sample_reg,data.frame(ID=1:length(basin_sample_reg)))
basin_sample_reg=raster::intersect(basin_sample_reg,study_basin)

focus_sample_reg=SpatialPointsDataFrame(focus_sample_reg,data.frame(ID=1:length(focus_sample_reg)))
focus_sample_reg=raster::intersect(focus_sample_reg,focusArea)
focus_sample_reg=focus_sample_reg[,c('ID',"Region")]
plot(study_basin)
plot(basin_sample_reg,add=T,pch=21,bg="red")
plot(focus_sample_reg,add=T,pch=21,bg="blue")

length(basin_sample_reg)
length(focus_sample_reg)

length(basin_sample_reg)+length(focus_sample_reg)




tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
  tm_shape(FLAB)+tm_polygons(border.col="grey",alpha=0)+
  tm_shape(study_basin)+tm_polygons(border.col="yellow",alpha=0)+
  tm_shape(basin_sample_reg)+tm_dots("red",size=0.02)+
  tm_shape(focusArea)+tm_polygons(border.col="blue",alpha=0)+
  tm_shape(focus_sample_reg)+tm_dots("blue",size=0.02)

## export as shapefiles 
# writeOGR(basin_sample_reg,paste0(export.path,"GIS"),"basin_sample",driver="ESRI Shapefile")
# writeOGR(focus_sample_reg,paste0(export.path,"GIS"),"focus_sample",driver="ESRI Shapefile")



library(ceramic)
# ceramic public token for API
public.token="pk.eyJ1IjoicGp1bGlhbiIsImEiOiJjanllbmJ0eXkxMzV0M2dzNXh5NGRlYXdqIn0.g4weKGOt1WdNZLg2hxBz1w"
Sys.setenv(MAPBOX_API_KEY=public.token)

aoi=bind(study_basin,focusArea)

roi=raster::extent(spTransform(gBuffer(aoi,width=4000),wgs84))
im <- cc_location(roi,zoom=13)
plotRGB(im)

im=projectRaster(im,crs=utm17)
im=setValues(im,scales::rescale(values(im), c(0,255)))

# writeRaster(im, filename=paste(GIS.path,"AOI_image.tif",sep="/"), format="GTiff", overwrite=TRUE)



library(USAboundaries)
states.shp=us_boundaries(resolution ="low")
states.shp=as(states.shp,"Spatial")
states.shp=spTransform(states.shp,utm17)
attributes(states.shp)
SW.US=c("Florida","Georgia","Alabama","South Carolina")
FL.shp=us_boundaries(resolution ="low",states=SW.US)
FL.shp=as(FL.shp,"Spatial")
FL.shp=spTransform(FL.shp,utm17)
attributes(FL.shp)

# png(filename=paste0(plot.path,"samplingmap.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")

par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(c(rep(1,3),2:6),2,4,byrow=T),heights = c(1,0.8))

AOI=raster::extent(gBuffer(aoi,width=4000))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17

aoi.focus1=subset(focusArea,Region%in%c("Lake Ingraham Downstream",
                                       "Raulerson Brothers"))
aoi.focus2=subset(focusArea,Region%in%c("Slagle Ditch Downstream",
                                       "Flamingo Canal Downstream",
                                       "West Snake bight"))
aoi.focus3=subset(focusArea,Region%in%c("Garfield Bight",
                                       "Terrapin Bay"))
AOI.focus1=raster::extent(gBuffer(aoi.focus1,width=1000))
AOI.focus1.poly=as(AOI.focus1,"SpatialPolygons")
proj4string(AOI.focus1.poly)=utm17
AOI.focus2=raster::extent(gBuffer(aoi.focus2,width=1000))
AOI.focus2.poly=as(AOI.focus2,"SpatialPolygons")
proj4string(AOI.focus2.poly)=utm17
AOI.focus3=raster::extent(gBuffer(aoi.focus3,width=1000))
AOI.focus3.poly=as(AOI.focus3,"SpatialPolygons")
proj4string(AOI.focus3.poly)=utm17

bbox.lims=bbox(aoi)
plot(FLAB,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,border=NA)
plotRGB(im,add=T,alpha=200)
plot(crop(FLAB,AOI.poly),add=T,border="indianred1",lwd=1.5)
plot(study_basin,add=T,border="yellow",lwd=1.5)
plot(focusArea,add=T,border="white",col=adjustcolor("white",0.5))
plot(basin_sample_reg,add=T,bg="red",pch=21,cex=1,lwd=0.1)
plot(focus_sample_reg,add=T,bg="blue",pch=21,cex=1,lwd=0.1)
plot(AOI.poly,add=T,lwd=0.75)
plot(AOI.focus1.poly,add=T,border="white")
plot(AOI.focus2.poly,add=T,border="white")
plot(AOI.focus3.poly,add=T,border="white")

mapmisc::scaleBar(utm17,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")


bbox.lims=bbox(shore)
plot(FL.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(AOI.poly,add=T,col="red")

bbox.lims=bbox(gBuffer(aoi.focus1,width=1000))
plot(FLAB,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,border=NA)
plotRGB(im,add=T)
plot(crop(FLAB,AOI.poly),add=T,border="indianred1",lwd=1.5)
plot(study_basin,add=T,border="yellow",lwd=1.5)
plot(focusArea,add=T,border="white",col=adjustcolor("white",0.5))
plot(basin_sample_reg,add=T,bg="red",pch=21,cex=0.75,lwd=0.1)
plot(focus_sample_reg,add=T,bg="blue",pch=21,cex=0.75,lwd=0.1)
mapmisc::scaleBar(utm17,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")


bbox.lims=bbox(gBuffer(aoi.focus2,width=1000))
plot(FLAB,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,border=NA)
plotRGB(im,add=T)
plot(crop(FLAB,AOI.poly),add=T,border="indianred1",lwd=1.5)
plot(study_basin,add=T,border="yellow",lwd=1.5)
plot(focusArea,add=T,border="white",col=adjustcolor("white",0.5))
plot(basin_sample_reg,add=T,bg="red",pch=21,cex=0.75,lwd=0.1)
plot(focus_sample_reg,add=T,bg="blue",pch=21,cex=0.75,lwd=0.1)
mapmisc::scaleBar(utm17,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")

bbox.lims=bbox(gBuffer(aoi.focus3,width=1000))
plot(FLAB,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,border=NA)
plotRGB(im,add=T)
plot(crop(FLAB,AOI.poly),add=T,border="indianred1",lwd=1.5)
plot(study_basin,add=T,border="yellow",lwd=1.5)
plot(focusArea,add=T,border="white",col=adjustcolor("white",0.5))
plot(basin_sample_reg,add=T,bg="red",pch=21,cex=0.75,lwd=0.1)
plot(focus_sample_reg,add=T,bg="blue",pch=21,cex=0.75,lwd=0.1)
mapmisc::scaleBar(utm17,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("Florida Bay Basins", "Basins of Interest","Focus Areas", "Basin Sampling","Focus Area Sampling"),
       pch=c(22,22,22,21,21),lty=c(NA),lwd=c(1),
       col=c(NA,"yellow","grey",NA,NA),
       pt.bg=c("indianred1","white","grey80","Red","blue"),
       pt.cex=c(1.5),ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()
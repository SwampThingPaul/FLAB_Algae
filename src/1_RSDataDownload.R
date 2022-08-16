## 
# Title:      NOAA RS Data download
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

# Functions
ci.reverse.scaling.fun=function(DN){
  13.46374/((275.0/DN)-1)
}
ci.scaling.fun=function(ci){
  round(275.0 / 1+1346374/ci[ci>0])
}
# -------------------------------------------------------------------------

shore=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),utm17)
FLAB=spTransform(readOGR(paste0(GIS.path,"/FATHOM_segs"),"FATHOM_segs"),utm17)
# plot(FLAB)

# -------------------------------------------------------------------------


## Get list of files on NOAA site
noaa.HAB=readLines("https://products.coastalscience.noaa.gov/habs_explorer/index.php?path=N3JvcFZBb2ZoWThvTTNIZnJpTjQxTnZNeS9MVjNHUkRGUWtmNFZnK2JtOD0=")

vals=grep("<section class='onecol habonecol'><a href='https://products.coastalscience.noaa.gov/habs_explorer/index.ph",noaa.HAB)

noaa.image.inventory=data.frame()
# for(i in 1:6){
for(i in 1:length(vals)){
  tmp=noaa.HAB[vals[i]]
  tmp2=strsplit(tmp,"<a|</a>")[[1]]
  tmp3=strsplit(tmp2[2],"href=|title=|>")
  tmp4=strsplit(tmp2[3],"</section>")
  
  dat=data.frame(fileadd=sapply(tmp3,"[",2),filename=sapply(tmp4,"[",1))
  
  fname.vals=strsplit(dat$filename,"\\.")
  if(trimws(substr(sapply(fname.vals,"[",1),1,9))!="sentinel"){next}else{
    dat$data.product=sapply(fname.vals,"[",length(fname.vals[[1]])-1)
    
    
    yr.val=as.numeric(substr(sapply(fname.vals,"[",2),1,4))
    month.val=as.numeric(substr(sapply(fname.vals,"[",3),1,2))
    day.val=as.numeric(substr(sapply(fname.vals,"[",3),3,4))
    dat$date=as.Date(paste(yr.val,month.val,day.val,sep="-"))
    
    noaa.image.inventory=rbind(dat,noaa.image.inventory)
  }
}
unique(noaa.image.inventory$data.product)

noaa.image.inventory=subset(noaa.image.inventory,data.product=="chl_gil_rhos")
range(noaa.image.inventory$date)

noaa.image.inventory$fnames=with(noaa.image.inventory,paste0(format(date,"%Y%m%d"),"chl_gil_rhos.tif"))

noaa.image.inventory=subset(noaa.image.inventory,date>as.Date("2022-05-01"))

# fnames=noaa.image.inventory$fnames

# paste0(data.path,"sentinel_2022/", fnames)
# new.dat=fnames[fnames%in%list.files(paste0(data.path,"/RemoteSensing"))==F]

# adjust timeout time
options(timeout = max(800, getOption("timeout")))

# noaa.HAB.image2=subset(noaa.image.inventory,fnames%in%new.dat)
# if(nrow(noaa.HAB.image2)!=0){
pb=txtProgressBar(min=0,max=nrow(noaa.image.inventory),style=3)
for(i in 1:nrow(noaa.image.inventory)){
    download.file(noquote(gsub("'", '', noaa.image.inventory$fileadd[i], fixed=TRUE)),paste(data.path,"RemoteSensing", noaa.image.inventory$fnames[i],sep="/"),mode="wb",method="wininet")
  setTxtProgressBar(pb, i)  
  # print(i)
  }
# }
## 

fnames.tmp=subset(noaa.image.inventory,date==max(noaa.image.inventory$date))

fnames=subset(noaa.image.inventory,date>as.Date(as.Date("2022-08-08")-lubridate::duration(2,"months")))

for(i in 1:nrow(fnames)){
  tmp.raster=raster(paste(data.path,"RemoteSensing", fnames$fnames[i],sep="/"))

# tmp.raster=raster(paste(data.path,"RemoteSensing", fnames.tmp$fnames,sep="/"))
tmp.raster=mask(tmp.raster,gBuffer(FLAB,width=20000))

cloud.area=tmp.raster==253
cloud.area.raster=cloud.area

vals=c(0,250,251,252,253,254,255)
tmp.raster[tmp.raster%in%vals]=NA
tmp.raster=calc(tmp.raster,fun=function(x) x*1)

rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)# *100000000

area=rev.scale>0
val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]

if(cellStats(calc(rev.scale,fun=function(x) is.na(x)),min)==1){next}
tmp.date=fnames$date[i]
png(filename=paste0(plot.path,"RS_animation/",format(tmp.date,"%Y%m%d"),"_chl_gil_rhos.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")

par(family="serif",mar=c(0.5,0.5,0.5,0.5),oma=c(0.1,0.1,0.1,0.1));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

bbox.lims=bbox(gBuffer(FLAB,width=10000))
b=c(0,50,100,150,200,250)
cols=viridisLite::turbo(249,direction=1)

plot(FLAB,lwd=0.05,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
image(rev.scale,add=T,col = cols)
image(cloud.area.raster,add=T,col=c(NA,"grey"))
# plot(rasterToContour(rev.scale,levels=b,nlevels=length(b)),col="black",lwd=2,add=T)
# plot(FLAB,add=T,border="white")
plot(shore,add=T,col="cornsilk",border="grey",lwd=0.5)
mtext(side=3,line=-2.5,adj=0,paste("Date:",format(tmp.date,"%m-%d-%Y"),
                                   "\nData Source: NOAA NCCOS"))
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,outer=F)

plot(0:1,0:1,ann=F,axes=F,type="n")
b2=b
l.b=length(b2)
labs=b2
n.bks=length(b2) -1
top.val=0.8
bot.val=0.2
mid.v.val=bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
lab.pos=seq(bot.val,top.val,length.out=l.b)
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
text(x=x.max, y = lab.pos, labels = format(b2),cex=0.75,adj=0,pos=4,offset=0.5)
segments(rep(x.min,l.b),lab.pos,rep(x.max,l.b),lab.pos,lwd=2)
#add cloud
legend_image=as.raster(matrix("grey",ncol=1))
rasterImage(legend_image,x.min,bot.val-0.05,x.max,bot.val)
text(x=x.max, y = bot.val-0.025, labels = "Clouds",cex=0.5,adj=0,pos=4,offset=0.5)
text(x=mid.val,y=top.val,"Chlorophyll\n(\u03BCg L\u207B\u00B9)",adj=0,cex=0.8,pos=3,xpd=NA)
dev.off()
}


file.names=paste0(plot.path,"RS_animation/",list.files(paste0(plot.path,"RS_animation/")))

gifski::gifski(file.names, 
               delay = 50 / 100, 
               gif_file  = paste0(plot.path,"FLAB_Chl_gifski.gif"),
               loop = T)

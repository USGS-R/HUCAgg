library(maptools)
library(snow)

# Set this to where the files are.
workingPath<-'~/temp'

setwd(workingPath)

# Load directly from shapefile
hucPoly<-readShapePoly("WBDHU12.shp",proj4string= CRS('+init=epsg:4269'))

# Or load from an old data export. (faster)
# load('hucPoly.rda')

# Build the fromHUC list
# fromHUC<-sapply(hucPoly@data$HUC,fromHUC_finder,hucs=hucPoly@data$HUC,tohucs=hucPoly@data$TOHUC)

# Or load it from a previous run
load('fromHUC.rda')

# Generate the aggregate HUC list for everything.
# hucList<-hucPoly@data$HUC
# aggrHUCs<-sapply(as.character(unlist(hucList)), HUC_aggregator, fromHUC=fromHUC)

# Or load it from a previous run.
load('aggrHUCs.rda')

## TESTING One Watershed ##

huc<-"070700051802" #Wisconsin River
# huc<-"020402040000" #Delaware River
# huc<-"150301070105" #Colorado River

outHuc<-unionHUC(huc,aggrHUCs,hucPoly)
outShp<-subset(hucPoly,hucPoly@data$HUC %in% huc)
outShp@polygons[which(outShp@data$HUC %in% huc)][[1]]<-outHuc@polygons[[1]]
writePolyShape(outShp,file.path(huc))

## TESTING One Watershed ##

## TESTING HUC04 ##

hucList<-c()
for(huc in hucPoly@data$HUC) {
  if(grepl('^0707.*',huc)) {
    hucList<-c(hucList,huc)
  }
}
outHucs<-sapply(unlist(hucList), unionHUC, upstreamHUCs=aggrHUCs, hucPoly=hucPoly)
outShp<-subset(hucPoly,hucPoly@data$HUC %in% hucList)
for(huc in outShp@data$HUC){
  if(!is.null(outHucs[[huc]])){
    print(huc)
    outShp@polygons[which(outShp@data$HUC %in% huc)][[1]]<-outHucs[[huc]]@polygons[[1]]
  }
}
writePolyShape(outShp,file.path('0707'))

## TESTING HUC08 ##

## Run whole country ##

cl <- makeCluster(rep('localhost',24), type = "SOCK")

range<-seq(from = 1, to = length(hucPoly@data$HUC), by=1000)
ranges<-array(dim=c(length(range),2))
ranges[,1]<-seq(from = 1, to = length(hucPoly@data$HUC), by=1000)
ranges[,2]=seq(from = 1, to = length(hucPoly@data$HUC), by=1000)+1000
ranges[nrow(ranges),2]=length(hucPoly@data$HUC)

out<-parApply(cl, ranges, 1, natRunner, aggrHUCs=aggrHUCs, hucPoly=hucPoly, unionHUC=unionHUC, outPath=workingPath)

stopCluster(cl)

## Run whole country ##

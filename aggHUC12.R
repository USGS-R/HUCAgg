library(maptools)
library(snow)
library(HUCAgg)
library(rgeos)

# Set this to where the files are.
workingPath<-'~/temp'

setwd(workingPath)

# Load directly from shapefile
# hucPoly<-readShapePoly("WBDHU12.shp",proj4string= CRS('+init=epsg:4269'))

# Or load from an old data export. (faster)
load('hucPoly.rda')

# Build the fromHUC list
# fromHUC<-sapply(hucPoly@data$HUC,fromHUC_finder,hucs=hucPoly@data$HUC,tohucs=hucPoly@data$TOHUC)

# Or load it from a previous run
load('fromHUC.rda')

# Generate the aggregate HUC list for everything.
hucList<-hucPoly@data$HUC
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

cl <- makeCluster(rep('localhost',2), type = "SOCK")

range<-seq(from = 1, to = length(hucPoly@data$HUC), by=1000)
ranges<-array(dim=c(length(range),2))
ranges[,1]<-seq(from = 1, to = length(hucPoly@data$HUC), by=1000)
ranges[,2]=seq(from = 1, to = length(hucPoly@data$HUC), by=1000)+1000
ranges[nrow(ranges),2]=length(hucPoly@data$HUC)

out<-parApply(cl, ranges, 1, natRunner, aggrHUCs=aggrHUCs, hucPoly=hucPoly, unionHUC=unionHUC, outPath=workingPath)

stopCluster(cl)

## Run whole country ##

## Walk Down the Network ##

upstream_size<-sapply(aggrHUCs, length) # The length of the list of upstream hucs.

for ( setSize in 1:max(upstream_size) ) {
  hucs<-names(upstream_size[which(upstream_size==setSize)])
  print(setSize)
  print(paste('length of set is',length(hucs)))
  for ( huc in hucs ) {
    fromHUCs<-unlist(fromHUC[huc][[1]])
    hucListSub<-c(fromHUCs,huc)
    hucPolySub<-subset(hucPoly,hucPoly@data$HUC %in% hucListSub)
    hucPolySub@data$group<-1
    tryCatch(
    hucPoly@polygons[which(hucPoly@data$HUC %in% huc)][[1]]<-unionSpatialPolygons(hucPolySub,hucPolySub@data$group)@polygons[[1]],
    warning = function(w) {print(paste("Warning handling", huc, "warning was", w))},
    error = function(e) {print(paste("Error handling", huc, "error was", e))})
    hucPoly@polygons[which(hucPoly@data$HUC %in% huc)][[1]]@ID<-huc 
  }
}

save(hucPoly,file='hucPoly_agg.rda')

## Walk Down the Network

range<-seq(from = 1, to = length(hucPoly@data$HUC), by=1000)
ranges<-array(dim=c(length(range),2))
ranges[,1]<-seq(from = 1, to = length(hucPoly@data$HUC), by=1000)
ranges[,2]=seq(from = 1, to = length(hucPoly@data$HUC), by=1000)+1000
ranges[nrow(ranges),2]=length(hucPoly@data$HUC)
  
write_shape<-function(range,hucPoly) {
  subPoly<-subset(hucPoly,hucPoly@data$HUC %in% as.character(hucPoly@data$HUC[range[1]:range[2]]))
  for (p in 1:length(subPoly@polygons)) {
    numCoords<-0
    for (p2 in 1:length(subPoly@polygons[[p]]@Polygons)) {
      numCoords<-numCoords+length(subPoly@polygons[[p]]@Polygons[[p2]]@coords)
    }
    if (numCoords>900000) {
      print(subPoly@data$HUC12[p])
      print(numCoords)
      subPoly2<-subset(subPoly,subPoly@data$HUC %in% as.character(subPoly@data$HUC[p]))
      subPoly@polygons[[p]]<-gSimplify(subPoly2,0.00005)@polygons[[1]]
      numCoords<-0
      for (p2 in 1:length(subPoly@polygons[[p]]@Polygons)) {
        numCoords<-numCoords+length(subPoly@polygons[[p]]@Polygons[[p2]]@coords)
      }
      print(numCoords)
    }
  }
  writePolyShape(subPoly,file.path('./temp',toString(range[1])))
}

apply(ranges,1,write_shape,hucPoly)

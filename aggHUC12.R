library(maptools)
library(snow)

fromHUC_finder<-function(huc,hucs,tohucs,fromHUC){
  fromHUC<-as.list(hucs[tohucs %in% huc])
  return(fromHUC)
}

HUC_aggregator<-function(huc,fromHUC){
  fromHUCs<-fromHUC[[huc]] # Get fromHUCs list for given huc
  if(any(huc %in% fromHUCs)) { # found some HUCs that have themselves as a toHUC
    print(paste('found circular reference in',huc))
    return(huc)
  }
  if(length(fromHUCs)==0) { # If no fromHUCs for given HUC, return aggHUCs.
    return(fromHUCs)
  }
  else { # Otherwise, add current list to aggHUCs and call HUC_aggregator for list of upstream HUCs.
    aggHUCs<-c(fromHUCs,(unlist(lapply(fromHUCs,HUC_aggregator,fromHUC=fromHUC))))
  }
}

unionHUC<-function(huc,upstreamHUCs,hucPoly) {
  library(maptools)
  print(huc)
  # takes a huc as a string, the lookup table for upstream HUCs, and the upstream polygons.
  hucListSub<-unlist(upstreamHUCs[huc])
  if(is.null(hucListSub)) {
    print(paste('top',huc))
    return(NULL)
  }
  hucListSub<-c(hucListSub,huc)
  hucPolySub<-subset(hucPoly,hucPoly@data$HUC %in% hucListSub)
  hucPolySub@data$group<-1
  aggPoly<-unionSpatialPolygons(hucPolySub,hucPolySub@data$group)
  return(aggPoly)
}

# Used for running unionHUC in parallel
natRunner<-function(range, aggrHUCs, hucPoly, unionHUC, outPath) {
  library(maptools)
  tryCatch ({
    hucList<-as.character(hucPoly@data$HUC[range[1]:range[2]])
    outHucList<-sapply(unlist(hucList), unionHUC, upstreamHUCs=aggrHUCs, hucPoly=hucPoly)
    outShp<-subset(hucPoly,hucPoly@data$HUC %in% hucList)
    for(huc in outShp@data$HUC){
      if(!is.null(outHucList[[huc]])){
        outShp@polygons[which(outShp@data$HUC %in% huc)][[1]]<-outHucList[[huc]]@polygons[[1]]
      }
    }
    writePolyShape(outShp,file.path(outPath,toString(range[1])))
    rm(outHucList)
    rm(outShp)
    return(1)
  },
  error = function(cond) {
    return(paste(range[1], cond))
  })
}

workingPath<-'~/Desktop/'

setwd(workingPath)

# at<-read.csv('huc12_atts.csv', colClasses='character')
# hucPoly<-readShapePoly("WBDHU12.shp",proj4string= CRS('+init=epsg:4269'))

load('hucPoly.rda')

# fromHUC<-list()
# fromHUC<-sapply(at$HUC,fromHUC_finder,hucs=at$HUC,tohucs=at$TOHUC,fromHUC=fromHUC)

load('fromHUC.rda')

# hucList<-hucPoly@data$HUC
# aggrHUCs<-sapply(as.character(unlist(hucList)), HUC_aggregator, fromHUC=fromHUC)

load('aggrHUCs.rda')

## TESTING One Watershed ##
# huc<-"070700051802" #Wisconsin River
# # huc<-"020402040000" #Delaware River
# # huc<-"150301070105" #Colorado River
# 
# outHuc<-unionHUC(huc,aggrHUCs,hucPoly)
# outShp<-subset(hucPoly,hucPoly@data$HUC %in% huc)
# outShp@polygons[which(outShp@data$HUC %in% huc)][[1]]<-outHuc@polygons[[1]]
# writePolyShape(outShp,file.path(huc))
## TESTING One Watershed ##

## TESTING HUC04 ##
# hucList<-c()
# for(huc in hucPoly@data$HUC) {
#   if(grepl('^0707.*',huc)) {
#     hucList<-c(hucList,huc)
#   }
# }
# outHucs<-sapply(unlist(hucList), unionHUC, upstreamHUCs=aggrHUCs, hucPoly=hucPoly)
# outShp<-subset(hucPoly,hucPoly@data$HUC %in% hucList)
# for(huc in outShp@data$HUC){
#   if(!is.null(outHucs[[huc]])){
#     print(huc)
#     outShp@polygons[which(outShp@data$HUC %in% huc)][[1]]<-outHucs[[huc]]@polygons[[1]]
#   }
# }
# writePolyShape(outShp,file.path('0707'))

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

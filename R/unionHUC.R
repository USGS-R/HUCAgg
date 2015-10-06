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
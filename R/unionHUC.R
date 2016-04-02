#' Union HUC
#' 
#' Aggregates geometry for a set of HUCs.
#' 
#' @param huc The huc in question
#' @param upstreamHUCs A list of HUCs to be aggregated into one geometry
#' @param hucPoly An imported shapefile with all the HUC geometry
#' @return An aggregated polygon
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @importFrom maptools unionSpatialPolygons
#' @export
#' @examples
#' load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
#' plot(testhucPoly)
#' hucList<-as.character(unlist(getHUCList("07",testhucPoly)))
#' fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
#' aggrHUCs<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
#' huc<-"070900020904"
#' outhucPoly<-unionHUC(huc, aggrHUCs, testhucPoly)
#' plot(outhucPoly, add=TRUE, col=rgb(1,0,0,.3))
#' 
unionHUC<-function(huc,upstreamHUCs,hucPoly) {
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

load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
plot(testhucPoly)
hucList<-as.character(unlist(getHUCList("07",testhucPoly)))
fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
aggrHUCs<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
huc<-"070900020904"
outhucPoly<-unionHUC(huc, aggrHUCs, testhucPoly)
plot(outhucPoly, add=TRUE, col=rgb(1,0,0,.3))
 
#' Union HUC
#' 
#' Aggregates geometry for a set of HUCs
#' 
#' @param huc The huc in question
#' @param upstreamHUCs A list of HUCs to be aggregated into one geometry
#' @param hucPoly An imported shapefile with all the HUC geometry
#' @return An aggregated polygon
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @importFrom maptools unionSpatialPolygons
#' @export
#' @examples
#' TBD
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
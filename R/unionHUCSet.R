#' Union HUC Set
#' 
#' Aggregates geometry for a large collection of HUCs. Note that subhucPoly must be all potential HUCs required.
#' 
#' @param aggrHUCs The list of aggregate HUCs to be processed per HUC_aggregator
#' @param fromHUCs The list of 'fromHUCs' to be processed per fromHUC_finder
#' @param subhucPoly A subset of HUCs for the region in questioned.
#' @return The subhucPoly data frame with requested aggregated HUCs unioned together. 
#' Note that Polygon 'ID's are set to the HUC id and areas are added up appropriately. 
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @importFrom maptools unionSpatialPolygons
#' @export
#' @examples
#' \dontrun{
#' load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
#' plot(testhucPoly)
#' hucList<-testhucPoly@data$HUC12
#' fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
#' aggrHUCs<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
#' testhucPoly<-unionHUCSet(aggrHUCs, fromHUC, testhucPoly)
#' plot(subset(testhucPoly,testhucPoly@data$HUC12 %in% "070900020904"), add=TRUE, col=rgb(1,0,0,.3))
#' plot(subset(testhucPoly,testhucPoly@data$HUC12 %in% "070900020702"), add=TRUE, col=rgb(1,1,0,.3))
#' }
unionHUCSet<-function(aggrHUCs,fromHUCs,subhucPoly) {
  upstream_size<-sapply(aggrHUCs, length)
  for ( setSize in 1:max(upstream_size)) {
    hucs<-names(upstream_size[which(upstream_size==setSize)])
    for ( huc in hucs ) {
      fromHUC_local<-c(unlist(fromHUCs[huc][[1]]))
      if(length(fromHUC_local)>50) {print(paste(huc,'has',length(fromHUC_local),'contributing hucs'))}
      for (ihuc in 1:length(fromHUC_local)) { # I found that it is much faster to combine two iteratively rather than a ton in one block.
        hucListSub<-c(unlist(fromHUC_local[ihuc][[1]]),huc)
        subhucPolySub<-subset(subhucPoly,subhucPoly@data$HUC12 %in% hucListSub)
        subhucPolySub@data$group<-1
        ind<-which(subhucPoly@data$HUC12 %in% huc)
        tryCatch(
          subhucPoly@polygons[ind][[1]]<-unionSpatialPolygons(subhucPolySub,subhucPolySub@data$group)@polygons[[1]],
          warning = function(w) {print(paste("Warning handling", huc, "warning was", w))},
          error = function(e) {print(paste("Error handling", huc, "error was", e))})
        subhucPoly@data$AREAACRES[ind]<-sum(subhucPolySub@data$AREAACRES)
        subhucPoly@data$AREASQKM[ind]<-sum(subhucPolySub@data$AREASQKM)
        subhucPoly@polygons[[ind]]@ID=huc
      }
    }
  }
  return(subhucPoly)
}
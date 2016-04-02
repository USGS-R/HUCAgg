#' Simplify HUCs
#' 
#' A convenience function to help simplify very large aggregate HUCs.
#' 
#' @param subhucPoly A subset of HUCs for the region in questioned.
#' @param coordThresh A threshold number of nodes above which polygons will be simplified.
#' @param simpTol A tolerance to pass into the Douglas-Peuker algorith.
#' @return The subhucPoly data frame with large polygons simplified.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @importFrom rgeos gSimplify
#' @export
#' @examples
#' load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
#' hucList<-testhucPoly@data$HUC12
#' fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
#' aggrHUC<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
#' testhucPoly<-unionHUCSet(aggrHUC, fromHUC, testhucPoly)
#' plot(subset(testhucPoly,testhucPoly@data$HUC12 %in% "070900020504"))
#' testhucPoly<-simplifyHucs(testhucPoly,coordThresh=0,simpTol = 1e-2)
#' plot(subset(testhucPoly,testhucPoly@data$HUC12 %in% "070900020504"), add=TRUE)
#' 
simplifyHucs<-function(subhucPoly,coordThresh=50000,simpTol=0.00005) {
  for (p in 1:length(subhucPoly@polygons)) {
    numCoords<-0
    for (p2 in 1:length(subhucPoly@polygons[[p]]@Polygons)) {
      numCoords<-numCoords+length(subhucPoly@polygons[[p]]@Polygons[[p2]]@coords)
    }
    if (numCoords>coordThresh) {
      subhucPolySub<-subset(subhucPoly,subhucPoly@data$HUC12 %in% as.character(subhucPoly@data$HUC12[p]))
      tryCatch(
        subhucPoly@polygons[[p]]<-gSimplify(subhucPolySub,simpTol,topologyPreserve=TRUE)@polygons[[1]],
        warning = function(w) {print(paste("Warning simplifying", subhucPoly@data$HUC12[p], "warning was", w))},
        error = function(e) {print(paste("Error simplifying", subhucPoly@data$HUC12[p], "error was", e))})
    }
  }
  return(subhucPoly)
}
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
#' TBD
#' 
simplifyHucs<-function(subhucPoly,coordThresh=50000,simpTol=0.00005) {
  for (p in 1:length(subhucPoly@polygons)) {
    numCoords<-0
    for (p2 in 1:length(subhucPoly@polygons[[p]]@Polygons)) {
      numCoords<-numCoords+length(subhucPoly@polygons[[p]]@Polygons[[p2]]@coords)
    }
    if (numCoords>coordThresh) {
      subhucPolySub<-subset(subhucPoly,subhucPoly@data$HUC %in% as.character(subhucPoly@data$HUC[p]))
      tryCatch(
        subhucPoly@polygons[[p]]<-gSimplify(subhucPolySub,simpTol,topologyPreserve=TRUE)@polygons[[1]],
        warning = function(w) {print(paste("Warning simplifying", huc, "warning was", w))},
        error = function(e) {print(paste("Error simplifying", huc, "error was", e))})
    }
  }
  return(subhucPoly)
}
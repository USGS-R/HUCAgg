#' Get list of HUCs
#' 
#' A convenience function to retreive lists of HUCs according to a regex.
#' 
#' @param subRegion A HUC 02 or 04 for which to retreive a list of all HUC12s.
#' @param subhucPoly A subset of HUCs for the region in question.
#' @return A list of hucs found.
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @export
#' @examples
#' \dontrun{
#' load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
#' print(testhucPoly@data$HUC12)
#' hucList<-as.character(unlist(getHUCList("0709000205",testhucPoly)))
#' print(hucList)
#' }
#' 
getHUCList<-function(subRegion,subhucPoly) {
  hucList<-c()
  for(huc in subhucPoly@data$HUC12) {
    if(grepl(paste0('^',subRegion,'.*'),huc)) {
      hucList<-c(hucList,huc)
    }
  }
  return(hucList)
}
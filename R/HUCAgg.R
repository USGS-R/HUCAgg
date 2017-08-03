pkg.env <- new.env()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    'USGS Support Package: 
    https://owi.usgs.gov/R/packages.html#support'),
    collapse='\n'))
}


#' HUCAgg
#'
#' \tabular{ll}{
#' Package: \tab HUCAgg\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' This package is used to aggregate HUC12 geospatial and time series 
#' data to total upstream watersheds.
#'
#' @name HUCAg-package
#' @docType package
NULL

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
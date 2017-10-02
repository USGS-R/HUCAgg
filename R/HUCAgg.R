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
#' @return A list of unique hucs found.
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
  return(unique(hucList))
}

#'Combine HUC polygons
#' 
#' A convenience function to combine HUCs stored as single polygons into multipolygons
#' 
#' @param subhucPoly A subset of HUCs with an AREASQKM and HUC12 field.
#' @return A deduplicated subhucPoly SpatialPolygonsDataFrame
#' @author David Blodgett \email{dblodgett@usgs.gov}
#' @export
#' 
combine_multis <- function(subhucPoly) {
  if(any(duplicated(subhucPoly@data$HUC12))) {
    
    subhucPoly_combined <- dplyr::group_by(subhucPoly@data, HUC12) %>%
      dplyr::summarise(AREASQKM_sum = sum(AREASQKM), AREASQKM_max = max(AREASQKM)) %>% 
      dplyr::filter(AREASQKM_max != AREASQKM_sum) %>% 
      dplyr::select(-AREASQKM_max) %>% data.frame(stringsAsFactors = F)
    
    row.names(subhucPoly_combined) <- subhucPoly_combined$HUC12
    
    for(huc in subhucPoly_combined$HUC12) {
      subset_hucs <- subhucPoly[which(subhucPoly@data$HUC12 == huc),]
      
      subhucPoly <- subhucPoly[which(subhucPoly@data$HUC12 != huc),]
      
      single_huc <- rgeos::gUnaryUnion(subset_hucs, id = subset_hucs@polygons[[1]]@ID)
      
      single_huc_df <- subset_hucs@data[1,]
      single_huc_df$AREASQKM <- subhucPoly_combined[huc,]$AREASQKM_sum
      
      single_huc <- SpatialPolygonsDataFrame(single_huc, single_huc_df, match.ID = F)
      
      subhucPoly <- rbind(subhucPoly, single_huc)
    }
  }
  return(subhucPoly)
}
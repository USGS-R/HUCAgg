#' Parallel Union HUCs
#' 
#' This is a convenience function to be called by parApply
#' 
#' @param range A vector of length two specifying which hucs to process
#' @param aggrHUCs The output of running fromHUC_finder for the entire dataset
#' @param hucPoly An imported shapefile with all the HUC geometry
#' @param unionHUC The unionHUC function for use in a newly spawned environment
#' @param outPath The path for the newly spawned environment to write to
#' @return 1 for success the range and error condition for failure.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @examples
#' TBD
#' 
parUnionHUCRunner<-function(range, aggrHUCs, hucPoly, unionHUC, outPath) {
  library(maptools) # This may not be needed now that this is part of a package.
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
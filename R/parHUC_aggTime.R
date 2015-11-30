#' Parallel Weighted Average for HUCs
#' 
#' This is a convenience function to be called by parApply
#' 
#' @param range A vector of length two specifying which hucs to process
#' @param aggrHUCs The output of running fromHUC_finder for the entire dataset
#' @param areas The areas for all the huc12s
#' @param dataF A data frame containing time series for each local huc
#' @param aggregate_hucdata The function for use in a newly spawned environment
#' @return 1 for success the range and error condition for failure.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' TBD
#' 
parHUC_aggTime<-function(range, aggrHUCs, areas, aggregate_hucdata) {
  tryCatch ({
    hucList<-as.character(areas$huc12[range[1]:range[2]])
    outData<-sapply(unlist(hucList), aggregate_hucdata, aggrHUCs=aggrHUCs, areas=areas, dataF=dataF)
    return(outData)
  },
  error = function(cond) {
    return(paste(range[1], cond))
  })
}
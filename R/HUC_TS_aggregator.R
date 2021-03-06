#' Aggregate HUC time series data
#' 
#' Aggregate time series data for a collection of HUCs
#' 
#' @param upstream_size Named integer list of the number of upstream HUCs for each huc.
#' @param fromHUC Named list of lists containing the HUCs that flow into each HUC.
#' @param huc12_areaDF \code{data.frame} with a single observation of area for each local HUC.
#' @param huc12agg_areaDF \code{data.frame} with a single observation of area for each aggregated HUC.
#' @param dataF \code{data.frame} with a set of observations for each local HUC.
#' @return dataF \code{data.frame} with the set of observations aggregated upstream using a weighted average.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' \dontrun{
#' load(system.file("extdata","HUC_TS_aggregator_test.rda",package="HUCAgg"))
#' outData<-HUC_TS_aggregator(upstream_size,fromHUC,huc12_areaDF,huc12agg_areaDF,dataF)
#' }
HUC_TS_aggregator<-function(upstream_size,fromHUC,huc12_areaDF,huc12agg_areaDF,dataF) {
    for ( setSize in 1:max(upstream_size) ) {
      hucs<-names(upstream_size[which(upstream_size==setSize)])
      if ( length(hucs) > 100 ) {
        print(setSize)
        print(paste('length of set is',length(hucs)))
      }
      for ( huc in hucs ) {
        if(huc %in% names(dataF)) {
          fromHUCs<-fromHUC[huc]
          # calculate the sum of areas
          area<-huc12_areaDF[huc][[1]]
          # Set wa to the local times its area initially.
          wa<-dataF[huc][[1]]*huc12_areaDF[huc][[1]]
          for(h in unlist(fromHUCs[[1]])) {
            try({ # Using try here to handle rare case of missing data. 
              # Loop over the fromHUCs adding them to wa with their area.
              wa<-wa+dataF[h][[1]]*huc12agg_areaDF[h][[1]]
              area<-area+huc12agg_areaDF[h][[1]]}, silent = FALSE
            )
          }
          wa<-wa/area
        }
        dataF[huc]<-wa
      }
    }
    return(dataF)
  }



#' FromHUC Finder
#' 
#' This function finds the hucs that flow into a given huc. 
#' 
#' @param huc The huc in question
#' @param hucs The list of all HUCs
#' @param tohucs The list of TUHUCs
#' @return The list of fromHUCs
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' TBD
#' 
fromHUC_finder<-function(huc,hucs,tohucs){
  fromHUC<-as.list(hucs[tohucs %in% huc])
  return(fromHUC)
}
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
#' \dontrun{
#' load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
#' print(testhucPoly@data$HUC12[which(testhucPoly@data$TOHUC %in% "070900020504")])
#' print(testhucPoly@data$HUC12[which(testhucPoly@data$TOHUC %in% "070900020604")])
#' hucList<-testhucPoly@data$HUC12
#' fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
#' print(unlist(fromHUC["070900020504"][[1]]))
#' print(unlist(fromHUC["070900020604"][[1]]))
#' }
#' 
fromHUC_finder<-function(huc,hucs,tohucs){
  fromHUC<-as.list(hucs[tohucs %in% huc])
  return(fromHUC)
}
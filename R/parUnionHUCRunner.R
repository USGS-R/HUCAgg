parUnionHUCRunner<-function(range, aggrHUCs, hucPoly, unionHUC, outPath) {
  library(maptools)
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
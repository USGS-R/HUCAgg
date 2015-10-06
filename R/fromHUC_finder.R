fromHUC_finder<-function(huc,hucs,tohucs,fromHUC){
  fromHUC<-as.list(hucs[tohucs %in% huc])
  return(fromHUC)
}
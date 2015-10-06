HUC_aggregator<-function(huc,fromHUC){
  fromHUCs<-fromHUC[[huc]] # Get fromHUCs list for given huc
  if(any(huc %in% fromHUCs)) { # found some HUCs that have themselves as a toHUC
    print(paste('found circular reference in',huc))
    return(huc)
  }
  if(length(fromHUCs)==0) { # If no fromHUCs for given HUC, return aggHUCs.
    return(fromHUCs)
  }
  else { # Otherwise, add current list to aggHUCs and call HUC_aggregator for list of upstream HUCs.
    aggHUCs<-c(fromHUCs,(unlist(lapply(fromHUCs,HUC_aggregator,fromHUC=fromHUC))))
  }
}
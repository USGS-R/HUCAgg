## ----opts, echo=F, eval=T------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=5, fig.align = "center") 

## ---- init_regions, echo=T, eval=T, error=F, message=F-------------------
library(HUCAgg)
WBDPath<-"WBD.gdb"
regionsPath<-"regions"

regions<-init_regions(WBDPath, regionsPath)
str(regions)

## ----colorado_1, echo=T, eval=T, error=F, message=F----------------------
region <- "colorado"
load(file.path('regions',paste0(region,'.rda')))
subhucPoly@data$UPHUCS<-""
subRegion <- regions[region][[1]][1]
local_incremental <- subhucPoly[which(subhucPoly$HUC12 == "140100011508"), ]
plot(local_incremental)

## ----colorado_2, echo=T, eval=T, error=F, message=F----------------------
hucList<-getHUCList(subRegion,subhucPoly)

## ----colorado_3, echo=T, eval=T, error=F, message=F----------------------
fromHUC<-sapply(as.character(unlist(hucList)), fromHUC_finder,
                hucs=subhucPoly@data$HUC12,
                tohucs=subhucPoly@data$TOHUC)
aggrHUCs<-sapply(as.character(unlist(hucList)), HUC_aggregator, 
                 fromHUC=fromHUC)

## ----colorado_4, echo=T, eval=T, error=F, message=F----------------------
subhucPoly<-unionHUCSet(aggrHUCs, fromHUC, subhucPoly)
subhucPoly<-simplifyHucs(subhucPoly, simpTol = 1e-04)

## ----colorado_5, echo=T, eval=T, error=F, message=F----------------------
total_upstream <- subhucPoly[which(subhucPoly$HUC12 == "140100011508"), ]
plot(total_upstream)
plot(local_incremental, add=TRUE, col=rgb(1,0,0,.3))


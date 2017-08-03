## ----opts, echo=F, eval=T------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=5, fig.align = "center") 

## ----loadData, warning=F, message=F, echo=T, eval=T----------------------
library(rgdal)
ogrListLayers("WBD_06_GDB/WBD_06_GDB.gdb/")
WBD06 <- readOGR("WBD_06_GDB/WBD_06_GDB.gdb/", layer = "WBDHU12", stringsAsFactors = F)

## ----fromhuc1, warning=F, echo=T, message=F, eval=T----------------------
library(HUCAgg)
fromHUCs <- fromHUC_finder(huc = "060102010204", 
                           hucs = WBD06@data$HUC12, 
                           tohucs = WBD06@data$ToHUC)
fromHUCs

## ----fromhuc2, warning=F, echo=T, eval=T---------------------------------
fromHUCs<-sapply(WBD06@data$HUC12,
                fromHUC_finder,
                hucs=WBD06@data$HUC12,
                tohucs=WBD06@data$ToHUC)
number_of_fromHUCs <- as.numeric(lapply(fromHUCs, function(x) length(x)))
hist(number_of_fromHUCs)

## ----aggrhuc1, warning=F, echo=T, eval=T---------------------------------
upstream_hucs <- HUC_aggregator("060102010204", fromHUCs)
length(upstream_hucs)

## ----aggrhuc2, warning=F, echo=T, eval=T---------------------------------
aggregate_HUCs<-sapply(WBD06@data$HUC12, HUC_aggregator, fromHUC=fromHUCs)
length(aggregate_HUCs["060102010204"][[1]])
number_of_upstream_hucs <- as.data.frame(as.numeric(lapply(aggregate_HUCs, function(x) length(x))))
library(ggplot2)
ggplot(number_of_upstream_hucs, aes(x = number_of_upstream_hucs)) + geom_histogram(binwidth = .2) + scale_x_log10()

## ----unionHUC, warning=F, echo=T, eval=T---------------------------------
upstream_huc_polygons <- WBD06[which(WBD06$HUC12 %in% upstream_hucs), ]
plot(upstream_huc_polygons)

## ----unionHUC2, warning=F, echo=T, eval=T--------------------------------
unioned_HUC <- unionHUC("060102010204", 
                        upstreamHUCs = aggregate_HUCs,
                        hucPoly = WBD06)
plot(upstream_huc_polygons)
plot(unioned_HUC, add=TRUE, col=rgb(1,0,0,.3))


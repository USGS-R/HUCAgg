context("Test HUC Simplification Function")

test_that("Returned unioned polygons are correct.", {
  load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
  hucList<-as.character(unlist(getHUCList("07",testhucPoly)))
  fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
  aggrHUC<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
  expected_area<-sum(testhucPoly@data$AREASQKM[which(testhucPoly@data$HUC12 %in% c('070900020904',aggrHUC[['070900020904']]))])
  testhucPoly<-unionHUCSet(aggrHUC, fromHUC, testhucPoly)
  testhucPoly<-simplifyHucs(testhucPoly,coordThresh=0,simpTol = 1e-4)
  for (p in 1:length(testhucPoly@polygons)) {
    numCoords<-0
    for (p2 in 1:length(testhucPoly@polygons[[p]]@Polygons)) {
      numCoords<-numCoords+length(testhucPoly@polygons[[p]]@Polygons[[p2]]@coords)
    }
  }
  expect_equal(numCoords,4234) # From Previous Visual Inspection Test
})

context("Test HUC combining functions")

test_that("Returned hucs are as expected", {
  subhucPoly <- readRDS("data/duplicate_subhucPoly.rds")
  subhucPoly_dedup <- combine_multis(subhucPoly)  
  expect_equal(nrow(subhucPoly_dedup), length(unique(subhucPoly@data$HUC12)))
  expect_equal(sum(subhucPoly@data[which(subhucPoly@data$HUC12 == subhucPoly@data$HUC12[1]), ]$AREAACRES),
               subhucPoly_dedup@data$AREAACRES[which(subhucPoly_dedup@data$HUC12 == subhucPoly@data$HUC12[1])])
  expect_equal(sum(subhucPoly@data[which(subhucPoly@data$HUC12 == subhucPoly@data$HUC12[1]), ]$AREASQKM),
               subhucPoly_dedup@data$AREASQKM[which(subhucPoly_dedup@data$HUC12 == subhucPoly@data$HUC12[1])])
})
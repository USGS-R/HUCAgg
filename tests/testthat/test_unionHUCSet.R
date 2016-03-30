context("Test Union HUC Set Function")

test_that("Returned unioned polygons are correct.", {
  load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
  hucList<-as.character(unlist(getHUCList("07",testhucPoly)))
  fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
  aggrHUC<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
  expected_area<-sum(testhucPoly@data$AREASQKM[which(testhucPoly@data$HUC12 %in% c('070900020904',aggrHUC[['070900020904']]))])
  testhucPoly<-unionHUCSet(aggrHUC, fromHUC, testhucPoly)
  for (p in 1:length(testhucPoly@polygons)) {
    numCoords<-0
    for (p2 in 1:length(testhucPoly@polygons[[p]]@Polygons)) {
      numCoords<-numCoords+length(testhucPoly@polygons[[p]]@Polygons[[p2]]@coords)
    }
  }
  expect_equal(numCoords,10826) # From Previous Visual Inspection Test
  expect_equal(testhucPoly@data$AREASQKM[which(testhucPoly@data$HUC12 %in% '070900020904')],expected_area)
})

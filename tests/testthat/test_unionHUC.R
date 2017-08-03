context("Test Union HUC Function")

test_that("Returned unioned polygons are correct.", {
  load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
  hucList<-as.character(unlist(getHUCList("07",testhucPoly)))
  fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
  aggrHUCs<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
  huc<-"070900020904"
  outhucPoly<-unionHUC(huc, aggrHUCs, testhucPoly)
  for (p in 1:length(outhucPoly@polygons)) {
    numCoords<-0
    for (p2 in 1:length(testhucPoly@polygons[[p]]@Polygons)) {
      numCoords<-numCoords+length(testhucPoly@polygons[[p]]@Polygons[[p2]]@coords)
    }
  }
  expect_equal(numCoords,2400) # From Previous Visual Inspection Test
  
  expect_equal(outhucPoly@data$AREAACRES, 275170)
  expect_equal(outhucPoly@data$AREASQKM, 1113.574466)
})

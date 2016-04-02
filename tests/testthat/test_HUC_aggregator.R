context("Test HUC aggregator Function")

test_that("Returned unioned polygons are correct.", {
  load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
  hucList<-testhucPoly@data$HUC12
  fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
  expected<-c(unlist(fromHUC["070900020604"][[1]]),
              unlist(fromHUC["070900020504"][[1]]), 
              unlist(fromHUC["070900020602"][[1]]), 
              unlist(fromHUC["070900020603"][[1]]))
  aggrHUCs<-sapply(hucList, HUC_aggregator, fromHUC=fromHUC)
  expect_equal(expected,unlist(aggrHUCs["070900020604"][[1]]))
})

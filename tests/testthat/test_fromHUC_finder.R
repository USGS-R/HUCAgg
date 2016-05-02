context("Test fromHUC_finder Function")

test_that("Returned unioned polygons are correct.", {
  load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
  hucList<-testhucPoly@data$HUC12
  fromHUC<-sapply(hucList,fromHUC_finder,hucs=testhucPoly@data$HUC12,tohucs=testhucPoly@data$TOHUC)
  expect_equal(unlist(fromHUC["070900020504"][[1]]),testhucPoly@data$HUC12[which(testhucPoly@data$TOHUC %in% "070900020504")])
  expect_equal(unlist(fromHUC["070900020604"][[1]]),testhucPoly@data$HUC12[which(testhucPoly@data$TOHUC %in% "070900020604")])
})

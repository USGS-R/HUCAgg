context("Test getHUCList Function")

test_that("Returned unioned polygons are correct.", {
  load(system.file("extdata","testhucpoly.rda",package="HUCAgg"))
  expected<-c("070900020501", "070900020502", "070900020503", "070900020504")
  hucList<-as.character(unlist(getHUCList("0709000205",testhucPoly)))
  expect_equal(expected,hucList)
})

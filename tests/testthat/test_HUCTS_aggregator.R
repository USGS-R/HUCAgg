context("Test HUC Time Series Aggregator")

test_that("test package functions", {
  load(system.file("extdata","HUCTS_aggregator_test.rda",package="HUCAgg"))
  outData2<-HUCTS_aggregator(upstream_size,fromHUC,huc12_areaDF,huc12agg_areaDF,dataF)
  expect_equal(outData,outData2)
})
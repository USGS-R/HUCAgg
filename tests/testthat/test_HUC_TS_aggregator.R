context("Test HUC Time Series Aggregator")

test_that("test package functions", {
  load(system.file("extdata","HUC_TS_aggregator_test.rda",package="HUCAgg"))
  outData2<-HUC_TS_aggregator(upstream_size,fromHUC,huc12_areaDF,huc12agg_areaDF,dataF)
  expect_equal(outData,outData2)
})
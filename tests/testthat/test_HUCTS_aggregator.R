context("Test HUC Aggregator")

test_that("test package functions", {
  load('data/HUCTS_aggregator_test.rda')
  outData2<-HUCTS_aggregator(upstream_size,fromHUC,huc12_areaDF,huc12agg_areaDF,dataF)
  expect_equal(outData,outData2)
})
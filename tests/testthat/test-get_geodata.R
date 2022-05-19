testthat::test_that("geodata retrieval at the municipal level works", {
  testthat::expect_s3_class(swissdd::get_geodata(),"sf")
})

testthat::test_that("geodata retrieval at the district level works", {
  testthat::expect_s3_class(swissdd::get_geodata(geolevel="district"),"sf")
})

testthat::test_that("geodata retrieval at the cantonal level works", {
  testthat::expect_s3_class(swissdd::get_geodata(geolevel="canton"),"sf")
})

testthat::test_that("plot_nationalvotes works", {
  testthat::expect_s3_class(swissdd::plot_nationalvotes("2020-11-29"),"ggplot")
})

testthat::test_that("plot_cantonalvotes works", {
  testthat::expect_s3_class(swissdd::plot_cantonalvotes(),"ggplot")
})


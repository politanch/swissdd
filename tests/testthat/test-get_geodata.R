testthat::test_that("geodata retrieval works", {
  testthat::expect_s3_class(swissdd::get_geodata(),"sf")
})

testthat::test_that("plot_nationalvotes works", {
  testthat::expect_s3_class(swissdd::plot_nationalvotes("2020-11-29"),"ggplot")
})

testthat::test_that("plot_cantonalvotes works", {
  testthat::expect_s3_class(swissdd::plot_cantonalvotes(),"ggplot")
})


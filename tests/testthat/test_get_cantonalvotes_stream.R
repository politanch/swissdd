context("get_cantonalvotes_stream")

test_that("get_cantonalvotes_stream works", {
  
  expect_error(get_cantonalvotes_stream(votedate="1999-01-01"))

  cantvotes <- get_cantonalvotes_stream()
  
  expect_is(cantvotes, "data.frame")

})


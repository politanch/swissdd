context("get_cantonalvotes")

test_that("get_cantonalvotes works", {
  
  expect_error(get_cantonalvotes(votedates="1999-01-01"))
  
  cantvotes <- get_cantonalvotes()
  
  expect_is(cantvotes, "data.frame")

})


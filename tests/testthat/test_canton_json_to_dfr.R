context("canton_json_to_dfr")

test_that("canton_json_to_dfr works", {
  
  expect_error(canton_json_to_dfr(votedates="1999-01-01vv"))
  
  cantvotes <- canton_json_to_dfr()
  
  expect_is(cantvotes, "data.frame")
  
})

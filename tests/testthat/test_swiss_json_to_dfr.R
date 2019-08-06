context("swiss_json_to_dfr")

test_that("swiss_json_to_dfr works", {
  
  expect_error(swiss_json_to_dfr(votedates="1999-01-01vv"))
  
  cantvotes <- swiss_json_to_dfr()
  
  expect_is(cantvotes, "data.frame")
  
})

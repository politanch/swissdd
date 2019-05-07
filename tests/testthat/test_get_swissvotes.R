context("get_swissvotes")

test_that("get_swissvotes works", {
  
  expect_error(get_swissvotes(votedates="1999-01-01"))

  swissvotes <- get_swissvotes()
  
  expect_is(swissvotes, "data.frame")

})

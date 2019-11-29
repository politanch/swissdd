context("get_swissvotes")

test_that("get_swissvotes works", {
  
  swissvotes <- get_swissvotes()
  
  expect_is(swissvotes, "data.frame")
  
})


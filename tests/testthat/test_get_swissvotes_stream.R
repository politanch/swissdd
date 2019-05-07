context("get_swissvotes_stream")

test_that("get_swissvotes works", {
  
  expect_error(get_swissvotes_stream(votedate="1999-01-01"))

  swissvotes <- get_swissvotes_stream()
  
  expect_is(swissvotes, "data.frame")

})


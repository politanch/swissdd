context("get_swissvotes")

test_that("get_swissvotes works", {
  
  expect_error(get_swissvotes(votedates="1999-01-01"))
  
  swissvotes <- get_swissvotes()
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_swissvotes(geolevel="district")
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_swissvotes(geolevel="canton")
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_swissvotes(geolevel="national")
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- swissdd::get_swissvotes(geolevel = "zh_counting_districts",from_date = "2017-12-11")
  
  expect_is(swissvotes, "data.frame")
  
})
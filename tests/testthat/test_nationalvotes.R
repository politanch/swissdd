context("get_nationalvotes")

test_that("get_nationalvotes works", {
  
  expect_error(get_nationalvotes(votedates="1999-01-01"))
  
  swissvotes <- get_nationalvotes()
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_nationalvotes(geolevel="district")
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_nationalvotes(geolevel="canton")
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_nationalvotes(geolevel="national")
  
  expect_is(swissvotes, "data.frame")
  
  swissvotes <- get_nationalvotes(geolevel = "zh_counting_districts",from_date = "2017-12-11")
  
  expect_is(swissvotes, "data.frame")
  
})
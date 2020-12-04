context("get_nationalvotes")

test_that("get_nationalvotes results in an error with invalid votedates", {
  
  expect_error(get_nationalvotes(votedates="1999-01-01"))
  
})
  
test_that("get_nationalvotes works with default parameters (current votedate)", {
  
  swissvotes <- get_nationalvotes()
  
  expect_is(swissvotes, "data.frame")
  
})
  
test_that("get_nationalvotes works for district level results", {
  
  swissvotes <- get_nationalvotes(geolevel="district")
  
  expect_is(swissvotes, "data.frame")
  
})
 

test_that("get_nationalvotes works for canton level results", {
  
  swissvotes <- get_nationalvotes(geolevel="canton")
  
  expect_is(swissvotes, "data.frame")
  
})

test_that("get_nationalvotes works for national level results", {
  swissvotes <- get_nationalvotes(geolevel="national")
  
  expect_is(swissvotes, "data.frame")
  
})
  

test_that("get_nationalvotes works for counting district level results", {
  swissvotes <- get_nationalvotes(geolevel = "zh_counting_districts",from_date = "2017-12-11")
  
  expect_is(swissvotes, "data.frame")
  
})
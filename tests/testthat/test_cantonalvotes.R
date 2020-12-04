context("get_cantonalvotes")

test_that("get_cantonalvotes yields error when invalid votedate is provided", {
  
  expect_error(get_cantonalvotes(votedates="1999-01-01"))
  
})

test_that("get_cantonalvotes works with default parameters (last/current votedate)", {
  
  cantvotes <- get_cantonalvotes()
  
  expect_is(cantvotes, "data.frame")
  
})
  
test_that("get_cantonalvotes works for district level results", {
  
  
  cantvotes <- get_cantonalvotes(geolevel = "district")
  
  expect_is(cantvotes, "data.frame")
  
})
  

test_that("get_cantonalvotes works for counting districts results", {
  
  cantvotes <- get_cantonalvotes(votedates="2019-09-01",geolevel = "zh_counting_districts")
  
  expect_is(cantvotes, "data.frame")
  
})
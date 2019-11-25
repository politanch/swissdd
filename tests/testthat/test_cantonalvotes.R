context("get_cantonalvotes")

test_that("get_cantonalvotes works", {
  
  expect_error(get_cantonalvotes(votedates="1999-01-01"))
  
  cantvotes <- get_cantonalvotes()
  
  expect_is(cantvotes, "data.frame")
  
  cantvotes <- get_cantonalvotes(geolevel = "district")
  
  expect_is(cantvotes, "data.frame")
  
  cantvotes <- get_cantonalvotes(geolevel = "municipality")
  
  expect_is(cantvotes, "data.frame")
  
  cantvotes <- get_cantonalvotes(geolevel = "zh_counting_districts")
  
  expect_is(cantvotes, "data.frame")
  
})

context("available votedates")

test_that("available votedates works", {
  
  swissdates <- available_votedates()
  
  expect_is(swissdates, "Date")
  
  cantdates <- available_votedates(geolevel = "canton")
  
  expect_is(cantdates, "Date")
  
})



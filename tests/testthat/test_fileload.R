context("Get File")

test_that("Filename is correct loaded", {
  fname <- make_filename(2013)
  currdir <- getwd()
  setwd(paste0(currdir,"/data"))
  data <- fars_read(fname)
  
  expect_equal(nrow(data), 30202)
  expect_equal(length(data), 50)
})
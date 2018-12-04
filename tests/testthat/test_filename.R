context("Filename")

test_that("Filename is correct", {
  name <- make_filename(2013)
  expect_equal(name, 'accident_2013.csv.bz2')
})
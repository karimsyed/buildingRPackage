context("Test the map function of the buildingRPackage package")

test_that("make_filename generates the file name", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

testthat::test_that("make_filename function generates valid filename",{
      testthat::expect_equal(FARS::make_filename(2015),"accident_2015.csv.bz2")
})

context("POSIX metric")

test_that("POSIX metric are as expected", {
  time="2014-10-02 09:30:01"
  expect_equal(util_POSIXTimeToDate(goog.data[1,1]), "2014-10-02 09:30:01")
  expect_equal(as.numeric(util_dateToPOSIXTime(time)),as.numeric(goog.data[1,1]))
})


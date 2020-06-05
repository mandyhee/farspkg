# test number of columns are 50
test_that("test number of columes are 50",{
  expect_equal(ncol(fars_read(make_filename(2013))), 50)
})

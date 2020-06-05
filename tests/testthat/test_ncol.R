# test number of columns are 50
test_that("test number of columes are 50",{
  file = paste0(system.file("extdata", package = "farspkg"), "/",make_filename(2013))
  expect_equal(ncol(fars_read(file)), 50)
})

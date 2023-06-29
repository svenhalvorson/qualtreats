test_that("You have a base url enrivornment variable configured", {
  expect_true(valid_base_url('ashurbanipal.nineveh.qualtrics.com'))
  expect_false(valid_base_url('www.qualtables.com'))
  expect_error(valid_base_url())
  expect_false(valid_base_url(runif(1)))
  expect_false(valid_base_url(FALSE))
})

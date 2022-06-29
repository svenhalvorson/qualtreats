test_that("You have specified a valid out_dir", {
  # This is stupid because when we actually use the package
  # We probably won't have the repo open
  #expect_true(valid_out_dir(here::here()))
  expect_error(valid_out_dir(letters))
  expect_error(valid_out_dir(1))
  expect_true(valid_out_dir(NULL))
})

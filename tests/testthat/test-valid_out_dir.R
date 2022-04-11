test_that("multiplication works", {
  expect_true(valid_out_dir(here::here()))
  expect_error(valid_out_dir(letters))
  expect_error(valid_out_dir(1))
  expect_true(valid_out_dir(NULL))
})

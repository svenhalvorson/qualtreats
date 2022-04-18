test_that("we can subset safely", {
  expect_equal(
    subset_safely(iris, 'Species') ,
    iris$Species
  )
  expect_equal(
    subset_safely(iris, 'c'),
    NA
  )
})

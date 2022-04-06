test_that("valid_survey_id accepts proper arguments", {
  # This is too extensive but its my first time writing tests
  expect_error(valid_survey_id())
  expect_false(valid_survey_id('Lugal Banda'))
  expect_true(valid_survey_id('SV_887tASY7oNJ0Uxo'))
})

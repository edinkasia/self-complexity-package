
data("Showers_onesort", package = "selfcomplexity")

test_that("Calculate H gives correct answer", {
  unique_att <- extract_unique_attributes(simple_example$Attributes)
  res <- calculate_H(simple_example, Attributes, ResponseId, unique_att)
  # ensure that there is only one result as expected
  expect_length(res$H_index, 1)
  # ensure the calculated result matches published value
  expect_equal(res$H_index, 3.99, tolerance = 0.005)
})

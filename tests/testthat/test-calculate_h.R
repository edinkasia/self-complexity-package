
data("showers_onesort", package = "selfcomplexity")
data("attributes_40", package = "selfcomplexity")

test_that("Calculate H gives correct answer", {
  res <- calculate_H(showers_onesort, "Attributes", "ResponseId", attributes_40)
  # ensure that there is only one result as expected
  expect_length(res$H_index, 1)
  # ensure the calculated result matches published value
  expect_equal(res$H_index, 3.99, tolerance = 0.005)
})

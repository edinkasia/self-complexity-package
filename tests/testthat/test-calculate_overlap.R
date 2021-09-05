
data("Showers_onesort", package = "selfcomplexity")

test_that("Calculate overlap gives correct answer", {
  res <- calculate_overlap(Showers_onesort, "Attributes", "ResponseId", "Subtype_name",
                           na_name_rm = TRUE)
  # ensure that there is only one result as expected
  expect_length(res$overlap_norm, 1)
  # ensure the calculated result matches published value
  expect_equal(res$overlap_norm, 0.124, tolerance = 0.005)
})

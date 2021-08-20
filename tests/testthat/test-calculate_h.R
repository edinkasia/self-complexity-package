
data("Showers_onesort", package = "selfcomplexity")
all_attributes <- c(
  "Capable", "Comfortable", "Communicative", "Confident", "Disagreeing", "Disorganised",
  "Energetic", "Friendly", "Fun and Entertaining", "Giving", "Happy", "Hardworking",
  "Hopeless", "Immature", "Incompetent", "Indecisive", "Independent", "Inferior",
  "Insecure", "Intelligent", "Interested", "Irresponsible", "Irritable", "Isolated",
  "Lazy", "Like a failure", "Lovable", "Mature", "Needed", "Optimistic", "Organised",
  "Outgoing", "Sad and Blue", "Self-centered", "Successful", "Tense", "Uncomfortable",
  "Unloved", "Weary", "Worthless"
)

test_that("Calculate H gives correct answer", {
  res <- calculate_H(simple_example, Attributes, ResponseId, all_attributes)
  # ensure that there is only one result as expected
  expect_length(res$H_index, 1)
  # ensure the calculated result matches published value
  expect_equal(res$H_index, 3.99, tolerance = 0.005)
})

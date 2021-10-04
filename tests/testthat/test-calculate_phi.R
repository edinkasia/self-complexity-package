
data("complexity_data", package = "selfcomplexity")
data("Attributes_df", package = "selfcomplexity")

test_that("Calculate Phi gives correct answer", {
  data_phi <- complexity_data %>%
    dplyr::filter(ResponseId == "R_2wmF4JXKNx6McGd")
  neg <- Attributes_df %>%
    dplyr::filter(Negative == 1) %>%
    dplyr::pull(Attribute)
  pos <- Attributes_df %>%
    dplyr::filter(Positive == 1) %>%
    dplyr::pull(Attribute)
  res <- calculate_phi(data = data_phi, att_column = "Attributes",
                       id_column = "ResponseId", pos_att_vector = pos, neg_att_vector = neg)
  # ensure that there is only one result as expected
  expect_length(res$phi, 1)
  # ensure the calculated result matches published value
  expect_equal(res$phi, 0.0457, tolerance = 0.005)
})

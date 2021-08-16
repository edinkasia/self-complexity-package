#' Calculate the H index of self-complexity (dimensionality measure)
#'
#' @param x factor
#'
#' @return A tibble
#' @export
#' @examples
#' fcount(iris$Species)


calculate_H <- #nolint
  function(data, att_column, id_column, vector) {

  # creates a symbol from the string input
  # (needed to use this column name in further operations)
  id_col <- rlang::ensyms(id_column)

  hashed_data <- vector %>%
    purrr::map(~stringr::str_detect(data[[att_column]], .x)) %>%
    purrr::set_names(nm = vector) %>%
    dplyr::bind_cols(data, .) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::mutate(power = 2^ (0:(dplyr::n() - 1))) %>%
    dplyr::mutate(across(all_of(vector), ~. * power)) %>%
    dplyr::summarise(across(all_of(vector), sum)) %>%
    tidyr::pivot_longer(cols = all_of(vector)) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::count(value) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::summarise(
      H_index = log2(length(vector)) -
        (sum(n * log2(n)) / length(vector))
    )

  return(hashed_data)
}

# Currently, this function needs clean data in a long format -
# each non-empty self-aspect is a row, with its Attributes in a column,
# and a column of unique participant IDs.
# In addition, the function need a vector of Attributes.

# One option to add would be to have an argument that specifies whether
# the Attributes are already spread wide (which they might be, from Qualtrics).
# This would make life easier, as we could skip the first 3 lines of the function.

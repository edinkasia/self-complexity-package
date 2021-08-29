### The key function is calculate_overlap (at the bottom)
### The other functions prepare the data and set up the necessary operations

calculate_ol <- function(a1, a2) {
  overlap <- sum(a1 %in% a2) / length(a1)
  return(overlap)
}

create_overlap_df <- function(data, att_column, id_column, subtype_column,
                              na_name_rm = TRUE) {
  . <- NULL

  # creates a symbol from the string input
  # (needed to use this column name in further operations)
  id_col <- rlang::ensyms(id_column)
  att_quo <- rlang::ensym(att_column)

  # sanity checks
  check_input_data(data)
  check_columns_exist(data, !!!id_col, !!att_quo)

  split_data <- data %>% {
    if (na_name_rm) dplyr::filter(data, !is.na(data[[subtype_column]])) else .
  } %>%
    dplyr::mutate(attr = stringr::str_split(!!att_quo, ",")) %>%
    dplyr::select(id_column, subtype_column, attr)
### function updated with general arguments up to this point

  overlap_data <- split_data %>%
    dplyr::full_join(split_data, by = c(id_column)) %>%
    dplyr::mutate(overlap = purrr::map2_dbl(attr.x, attr.y, calculate_ol)) %>%
    dplyr::filter(Subtype.x != Subtype.y) %>%
    # overlap between two empty lists equals 1, so we filter them out here
    dplyr::filter(
      !rlang::is_empty(attr.x) | !rlang::is_empty(attr.y)
    ) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::mutate(
      overlap_norm = sum(overlap) /
        ((dplyr::n_distinct(Subtype.x)) *
           (dplyr::n_distinct(Subtype.x) - 1))
    )

  return(overlap_data)
}

#' Calculate overlap index
#'
#' @param data A data.frame like object, see details for more information
#' @param na_name_rm Boolean variable to determine whether empty aspect names
#' should be removed
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(selfcomplexity)
#' data(complexity_data, package = "selfcomplexity")
#' calculate_overlap(complexity_data, na_name_rm = TRUE)


calculate_overlap <- function(data, na_name_rm = TRUE) {
  overlap_df <- create_overlap_df(data, na_name_rm = na_name_rm)
  overlap_res <- overlap_df %>%
    dplyr::select(ResponseId, overlap_norm) %>%
    unique()
  return(overlap_res)
}

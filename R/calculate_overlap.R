### The key function is calculate_overlap (at the bottom)
### The other functions prepare the data and set up the necessary operations

calculate_ol <- function(a1, a2) {
  overlap <- sum(a1 %in% a2) / length(a1)
  return(overlap)
}

create_overlap_df <- function(subject_df, na_name_rm = TRUE) {
  . <- NULL
  split_data <- subject_df %>% {
    if (na_name_rm) dplyr::filter(., !is.na(.$Name)) else .
  } %>%
    dplyr::mutate(attr = stringr::str_split(Attributes, ",")) %>%
    dplyr::select(ResponseId, Subtype, Name, attr)

  overlap_data <- split_data %>%
    dplyr::full_join(split_data, by = c("ResponseId")) %>%
    dplyr::mutate(overlap = purrr::map2_dbl(attr.x, attr.y, calculate_ol)) %>%
    dplyr::filter(Subtype.x != Subtype.y) %>%
    # overlap between two empty lists equals 1, so we filter them out here
    dplyr::filter(
      !rlang::is_empty(attr.x) | !rlang::is_empty(attr.y)
    ) %>%
    dplyr::group_by(ResponseId) %>%
    dplyr::mutate(
      overlap_norm = sum(overlap) /
        ((dplyr::n_distinct(Subtype.x)) *
           (dplyr::n_distinct(Subtype.x) - 1))
    )

  return(overlap_data)
}

#' Calculate overlap index
#'
#' @param subject_df
#' @param na_name_rm
#'
#' @return dataframe
#' @export
#'
#' @examples
#' calculate_overlap(complexity_data, na_name_rm = TRUE)


calculate_overlap <- function(subject_df, na_name_rm = TRUE) {
  overlap_df <- create_overlap_df(subject_df, na_name_rm = na_name_rm)
  overlap_res <- overlap_df %>%
    dplyr::select(ResponseId, overlap_norm) %>%
    unique()
  return(overlap_res)
}

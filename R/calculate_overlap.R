### The key function is calculate_overlap (at the bottom)
### The other functions prepare the data and set up the necessary operations

calculate_ol <- function(a1, a2) {
  overlap <- sum(a1 %in% a2) / length(a1)
  return(overlap)
}

#' @importFrom rlang .data
create_overlap_df <- function(data,
                              att_column,
                              id_column,
                              subtype_column,
                              na_name_rm = TRUE) {
  . <- NULL

# creates a symbol from the string input
# (needed to use this column name in further operations)

id_column_as_symbol <-  into_symbol(id_column)
att_column_as_symbol <-  into_symbol(att_column)

id_column_as_string <- rlang::as_label(id_column_as_symbol)

  # labels for the subtype filter
  sx <- paste0(subtype_column, ".x")
  sy <- paste0(subtype_column, ".y")

  split_data <- data %>% {
    if (na_name_rm)
      dplyr::filter(data, !is.na(data[[subtype_column]]))
    else
      .
  } %>%
    dplyr::mutate(attr = stringr::str_split(!!att_column_as_symbol, ",")) %>%
    dplyr::select(id_column_as_symbol, subtype_column, attr)

  overlap_data <- split_data %>%
    dplyr::full_join(split_data, by = id_column_as_string) %>%
    dplyr::mutate(overlap = purrr::map2_dbl(.data$attr.x, .data$attr.y, calculate_ol)) %>%
    dplyr::filter(.data[[sx]] != .data[[sy]]) %>%
    # overlap between two empty lists equals 1, so we filter them out here
    dplyr::filter(!rlang::is_empty(.data$attr.x) |
                    !rlang::is_empty(.data$attr.y)) %>%
    dplyr::group_by(!!id_column_as_symbol) %>%
    dplyr::mutate(overlap_norm = sum(.data$overlap) /
                    ((dplyr::n_distinct(.data[[sx]])) *
                       (dplyr::n_distinct(.data[[sx]]) - 1)))

  return(overlap_data)
}

#' Calculate overlap index
#'
#' @param data A data.frame like object, see details for more information
#' @param att_column unquoted variable name of a single column. See details for
#' structure of this variable.
#' @param id_column unquoted variable name of a single column. The participant's unique
#' identifier.
#' @param subtype_column unquoted variable name of a single column. Names of self-aspects
#' listed by the participants. Rows where this variable is empty are filtered out by default.
#' @param na_name_rm Boolean variable to determine whether empty aspect names
#' should be removed.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(selfcomplexity)
#' data(complexity_data, package = "selfcomplexity")
# calculate_overlap(data = complexity_data, att_column = "Attributes", id_column = "ResponseId",
# subtype_column = "Name", na_name_rm = TRUE)



calculate_overlap <-
  function(data,
           att_column,
           id_column,
           subtype_column,
           na_name_rm = TRUE) {

    att_quo_as_symbol <- rlang::ensym(att_column)
    id_column_as_symbol <- rlang::ensym(id_column)

    # sanity checks
    check_input_data(data)
    check_columns_exist(data, !!id_column_as_symbol, !!att_quo_as_symbol)

    overlap_df <-
      create_overlap_df(
        data = data,
        att_column = att_column,
        id_column = id_column,
        subtype_column = subtype_column,
        na_name_rm = na_name_rm
      )

    overlap_res <- overlap_df %>%
      dplyr::select(!!id_column_as_symbol, .data$overlap_norm) %>%
      unique()
    return(overlap_res)
  }

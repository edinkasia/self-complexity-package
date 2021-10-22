#' Calculate phi index
#'
#' @param data A data.frame like object, see details for more information
#' @param att_column unquoted variable name of a single column. See details for
#' structure of this variable.
#' @param id_column unquoted variable name of a single column. The participant's unique
#' identifier.
#' @param pos_att_vector A vector of positive attributes available to the participant.
#' @param neg_att_vector A vector of negative attributes available to the participant.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(selfcomplexity)
#' data(Showers_onesort, package = "selfcomplexity")
#' data(Attributes_df, package = "selfcomplexity")
#' pos <- Attributes_df %>%
#' dplyr::filter(Positive == 1) %>%
#' dplyr::pull(Attribute)
#' neg <- Attributes_df %>%
#' dplyr::filter(Negative == 1) %>%
#' dplyr::pull(Attribute)
# calculate_phi(data = complexity_data, att_column = "Attributes",
# id_column = "ResponseId", pos_att_vector = pos, neg_att_vector = neg)

calculate_phi <- function(data, att_column, id_column, pos_att_vector, neg_att_vector) {

  id_column_as_symbol <- into_symbol(id_column)
  att_column_as_symbol <- into_symbol(att_column)

  result <- data %>%
    dplyr::mutate(attr = stringr::str_split(!!att_column_as_symbol, ",")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Pos_Attributes = sum(attr %in% pos_att_vector),
           Neg_Attributes = sum(attr %in% neg_att_vector)) %>%
    dplyr::select(id_column_as_symbol, .data$Pos_Attributes, .data$Neg_Attributes) %>%
    dplyr::group_by(!!id_column_as_symbol) %>%
    tidyr::nest() %>%
    dplyr::mutate(
           # transpose the table into a 2-row format
           trans = purrr::map(data, ~t(.x)),
           # add up all attributes used (with repeats)
           n_att = purrr::map_dbl(.data$trans, ~sum(colSums(.x))),
           n_by_val = purrr::map(data, ~colSums(.x)),
           # flag up rows with <3 neg or pos attributes
           warning_needed = purrr::map(.data$n_by_val, ~check_for_low_values(.x)),
           # calculate the chi squared
           # warning suppressed as p values are not of interest, just chi squared
           model = purrr::map(.data$trans, ~suppressWarnings(stats::chisq.test(x = .x))),
           # pluck the chi squared statistic
           chi_sq = purrr::map_dbl(.data$model, ~purrr::pluck(.x, 1)),
           # calculate phi as per formula (NA if conditions not met)
           phi = ifelse(.data$warning_needed == 0, sqrt(.data$chi_sq / .data$n_att), NA)) %>%
    dplyr::select(id_column_as_symbol, .data$phi) %>%
    dplyr::ungroup()

  return(result)
}

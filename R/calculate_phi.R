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

  into_symbol <- function(value) {
    result <- if (typeof(value) != "symbol") rlang::ensym(value)  else value
    return(result)
  }

  id_column_as_symbol <- into_symbol(id_column)
  att_column_as_symbol <- into_symbol(att_column)

  result <- data %>%
    dplyr::rowwise %>%
    dplyr::mutate(Pos_Attributes = sum(!!att_column_as_symbol %in% pos_att_vector),
           Neg_Attributes = sum(!!att_column_as_symbol %in% neg_att_vector)) %>%
    dplyr::select(id_column_as_symbol, Pos_Attributes, Neg_Attributes) %>%
    dplyr::group_by(!!id_column_as_symbol) %>%
    tidyr::nest() %>%
    dplyr::mutate(trans = purrr::map(data, ~t(.x)), # transpose the table into a 2-row format
           # add up all attributes used (with repeats)
           n_att = purrr::map_dbl(trans, ~sum(colSums(.x))),
           # calculate the chi squared
           model = purrr::map(trans, ~stats::chisq.test(x = .x)),
           # pluck the chi squared statistic
           chi_sq = purrr::map_dbl(model, ~purrr::pluck(.x, 1)),
           # calculate phi as per formula
           phi = sqrt(chi_sq/n_att)) %>%
    dplyr::select(id_column_as_symbol) %>%
    dplyr::ungroup()

  return(result)
}

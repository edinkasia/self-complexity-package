#' Calculate the H index of self-complexity (dimensionality measure)
#'
#' Longer description
#'
#' @param data A data.frame like object, see details for more information
#' @param att_column unquoted variable name of a single column. See details for
#' structure of this variable.
#' @param id_column unquoted variable name of a single column. The partipants unique
#' identifier
#' @param vector character vector of all attributes.
#' @return A tibble
#' @details
#' Further details on some of the arguments is given below.
#' \itemize{
#'  \item{"data"}{
#'    The expectation is that we have tidy, long format data. Each
#'    participant has a unique identifier, described by the id_column
#'    argument. There may be several observations for single participants
#'    in which case they represent different subtypes. It may be convenient
#'    to keep the subtype identification column in the data but it is not required
#'    for the purpose of this calculation.
#'  }
#'  \item{"att_column"}{
#'    At present we expect a column where each observation is a single,
#'    comma separated, character value of multiple adjectives, e.g
#'    "Comfortable,Disorganised,Interested,Irresponsible"
#'  }
#' }
#' @export
#' @examples
#' library(selfcomplexity)
#' data(complexity_data, package = "selfcomplexity")
#' data(Attributes_40, package = "selfcomplexity")
#' calculate_H(complexity_data, Attributes, ResponseId, Attributes_40)
calculate_H <- #nolint
  function(data, att_column, id_column, vector) {

  . <- NULL
  # creates a symbol from the string input
  # (needed to use this column name in further operations)
  id_col <- rlang::ensym(id_column)
  att_quo <- rlang::ensym(att_column)

  # sanity checks
  check_input_data(data)
  check_columns_exist(data, !!id_col, !!att_quo)

  # extract the string label for att_column
  att_column <- rlang::as_label(att_quo)
  hashed_data <- vector %>%
    purrr::map(~stringr::str_detect(data[[att_column]], .x)) %>%
    purrr::set_names(nm = vector) %>%
    dplyr::bind_cols(data, .) %>%
    dplyr::group_by(!!id_col) %>%
    dplyr::mutate(power = 2^ (0:(dplyr::n() - 1))) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(vector),
         ~. * power
        )
    ) %>%
    dplyr::summarise(
      dplyr::across(
        tidyselect::all_of(vector),
        sum
      )
    ) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(vector)) %>%
    dplyr::group_by(!!id_col) %>%
    dplyr::count(.data$value) %>%
    dplyr::summarise(
      H_index = log2(length(vector)) -
        (sum(.data$n * log2(.data$n)) / length(vector))
    )
    # # creates a symbol from the string input
    # # (needed to use this column name in further operations)
    # id_col <- rlang::ensyms(id_column)
    #
    # hashed_data <- vector %>%
    #   purrr::map(~stringr::str_detect(data[[att_column]], .x)) %>%
    #   purrr::set_names(nm = vector) %>%
    #   dplyr::bind_cols(data, .) %>%
    #   dplyr::group_by(!!!id_col) %>%
    #   dplyr::mutate(power = 2^ (0:(dplyr::n() - 1))) %>%
    #   dplyr::mutate(across(all_of(vector), ~. * power)) %>%
    #   dplyr::summarise(across(all_of(vector), sum)) %>%
    #   tidyr::pivot_longer(cols = all_of(vector)) %>%
    #   dplyr::group_by(!!!id_col) %>%
    #   dplyr::count(value) %>%
    #   dplyr::group_by(!!!id_col) %>%
    #   dplyr::summarise(
    #     H_index = log2(length(vector)) -
    #       (sum(n * log2(n)) / length(vector))
    #   )

    return(hashed_data)
  }

# Currently, this function needs clean data in a long format -
# each non-empty self-aspect is a row, with its Attributes in a column,
# and a column of unique participant IDs.
# In addition, the function need a vector of Attributes.

# One option to add would be to have an argument that specifies whether
# the Attributes are already spread wide (which they might be, from Qualtrics).
# This would make life easier, as we could skip the first 3 lines of the function.

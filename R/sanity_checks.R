#' Check input data
#'
#' A utility function to ensure that the input data.frame like
#' object for the various functions is appropriate
#'
#' @param data a data.frame like object
check_input_data <- function(data) {
  assertthat::assert_that(
    inherits(data, "data.frame"),
    msg = "Input data should be either a data.frame or tibble"
  )
}

#' Check columns exist
#'
#' Ensure that specified columns are present in the data.frame
#' like object
#'
#' @param data a data.frame like object
#' @param ... unquoted variable names
#' @example
#' check_columns_exist(cars, speed)
check_columns_exist <- function(data, ...) {
  dots <- rlang::enquos(..., .named = TRUE)
  nam <- names(dots)
  assertthat::assert_that(
    all(nam %in% colnames(data)),
    msg = glue::glue(
      "Column(s) {setdiff(nam, colnames(data))} are missing from input data"
    )
  )
}

#' Check for low values
#'
#' Check that the list-like object does not contain
#' numbers 0-2 (as per Phi calculation conditions).
#' Returns a 1 if low values present, and 0 otherwise.
#'
#' @param my_list a list or vector
#' @example
#' check_for_low_values(c(2, 3, 4))
check_for_low_values <- function(my_list) {
  low_nums <- c(0, 1, 2)
  result <- ifelse(length(intersect(my_list, low_nums)) == 0,
                   0, 1)
  return(result)
}

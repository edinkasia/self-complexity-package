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

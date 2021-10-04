#' Turn string/symbol into a symbol
#'
#' A utility function to ensure that whether user submits column names as
#' strings or symbols, they work appropriately in the key functions
#'
#' @param value a single string or symbol

into_symbol <- function(value) {
  result <- if (typeof(value) != "symbol") rlang::ensym(value)  else value
  return(result)
}

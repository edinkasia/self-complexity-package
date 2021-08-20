#' Extract unique attributes
#'
#' From a vector of characters in which each element
#' is a comma separated collection of words find all
#' the unique words.
#'
#' @param att character vector of comma separated words
#' (typically attributes in this context)
#' @return character vector of unique words
#' @export
extract_unique_attributes <- function(att) {
  att %>%
    stringr::str_split(",") %>%
    unlist %>%
    unique
}

#' Trim na
#'
#' Returns values in a vector, excluding NA
#'
#' @param x a vector
#' @return a vector of the same type as x
#' @export
trim_na <- function(x) {
  x[!is.na(x)]
}

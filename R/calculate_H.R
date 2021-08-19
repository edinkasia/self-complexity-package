#' Calculate the H index of self-complexity (dimensionality measure)
#'
#' @param x factor
#'
#' @return A tibbleA
#' @importFrom rlang .data
#' @export
#' @examples
#' All_Attributes <- c("Capable", "Comfortable", "Communicative", "Confident", "Disagreeing",
#' "Disorganised",
#' "Energetic", "Friendly", "Fun and Entertaining", "Giving", "Happy", "Hardworking",
#' "Hopeless", "Immature", "Incompetent", "Indecisive", "Independent", "Inferior",
#' "Insecure", "Intelligent", "Interested", "Irresponsible", "Irritable", "Isolated",
#' "Lazy", "Like a failure", "Lovable", "Mature", "Needed", "Optimistic", "Organised",
#' "Outgoing", "Sad and Blue", "Self-centered", "Successful", "Tense", "Uncomfortable",
#'"Unloved", "Weary", "Worthless")
#'
#' calculate_H(complexity_data, att_column = "Attributes", id_column = "ResponseId",
#' vector = All_Attributes)



calculate_H <- function(data, att_column, id_column, vector) {

  id_col <- rlang::ensyms(id_column) # creates a symbol from the string input (needed to use this column name in further operations)

  hashed_data <- vector %>%
    purrr::map(~stringr::str_detect(data[[att_column]], .x))%>%
    purrr::set_names(nm = vector) %>%
    dplyr::bind_cols(data, .data) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::mutate(power=2^(0:(dplyr::n()-1))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(vector),~.*power)) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(vector),sum)) %>%
    tidyr::pivot_longer(cols=dplyr::all_of(vector)) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::count(.data$value) %>%
    dplyr::group_by(!!!id_col) %>%
    dplyr::summarise(H_index = log2(length(vector)) -
                (sum(.data$n*log2(.data$n))/length(vector)))

  return(hashed_data)
}

# Currently, this function needs clean data in a long format - each non-empty self-aspect is a row, with its Attributes in a column, and a column of unique participant IDs.
# In addition, the function need a vector of Attributes.

# One option to add would be to have an argument that specifies whether the Attributes are already spread wide (which they might be, from Qualtrics).
# This would make life easier, as we could skip the first 3 lines of the function.

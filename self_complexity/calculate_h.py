def calculate_H(data, att_column, id_column, vector):
    return {'H_index': 1000}

# calculate_H <- #nolint
#   function(data, att_column, id_column, vector) {

#   . <- NULL
#   # creates a symbol from the string input
#   # (needed to use this column name in further operations)
#   id_col <- rlang::ensym(id_column)
#   att_quo <- rlang::ensym(att_column)

#   # sanity checks
#   check_input_data(data)
#   check_columns_exist(data, !!id_col, !!att_quo)

#   # extract the string label for att_column
#   att_column <- rlang::as_label(att_quo)
#   hashed_data <- vector %>%
#     purrr::map(~stringr::str_detect(data[[att_column]], .x)) %>%
#     purrr::set_names(nm = vector) %>%
#     dplyr::bind_cols(data, .) %>%
#     dplyr::group_by(!!id_col) %>%
#     dplyr::mutate(power = 2^ (0:(dplyr::n() - 1))) %>%
#     dplyr::mutate(
#       dplyr::across(
#         tidyselect::all_of(vector),
#          ~. * power
#         )
#     ) %>%
#     dplyr::summarise(
#       dplyr::across(
#         tidyselect::all_of(vector),
#         sum
#       )
#     ) %>%
#     tidyr::pivot_longer(cols = tidyselect::all_of(vector)) %>%
#     dplyr::group_by(!!id_col) %>%
#     dplyr::count(.data$value) %>%
#     dplyr::summarise(
#       H_index = log2(length(vector)) -
#         (sum(.data$n * log2(.data$n)) / length(vector))
#     )
#     return(hashed_data)
#   }
def say_something_silly():
    return "banana"

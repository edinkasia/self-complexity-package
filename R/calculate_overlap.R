### This is a calculation of overlap scores by Gustaf Rydevik
### The key function is calculate_overlap

calculate_OL<-function(a1,a2){
  overlap<-sum(a1%in%a2)/length(a1)
}

create_overlap_df <- function(subject_df, na.name.rm = TRUE) {
  split_data <- subject_df%>%
  {if (na.name.rm) dplyr::filter(., !is.na(.$Name)) else .} %>%
  dplyr::mutate(attr=stringr::str_split(Attributes,","))%>%
  dplyr::select(ResponseId, Subtype, Name, attr)

overlap_data<-split_data%>%dplyr::full_join(split_data,by=c("ResponseId"))%>%
  dplyr::mutate(overlap=purrr::map2_dbl(attr.x,attr.y,calculate_OL
  ))%>%
  dplyr::filter(Subtype.x!=Subtype.y)%>%
  dplyr::filter(!rlang::is_empty(attr.x)|!rlang::is_empty(attr.y)) %>% # overlap between two empty lists equals 1, so we filter them out here
  dplyr::group_by(ResponseId) %>%
  dplyr::mutate(overlap_norm=sum(overlap)/
                  ((dplyr::n_distinct(Subtype.x))*(dplyr::n_distinct(Subtype.x)-1)))

return(overlap_data)
}

#' Calculate overlap index
#'
#' @param subject_df
#' @param na.name.rm
#'
#' @return dataframe
#' @export
#'
#' @examples
#' calculate_overlap(example_data, na.name.rm = TRUE)


calculate_overlap <- function(subject_df, na.name.rm = TRUE){
  overlap_df <- create_overlap_df(subject_df, na.name.rm = na.name.rm)
  overlap_res <- overlap_df%>%dplyr::select(ResponseId,overlap_norm)%>%unique()
  return(overlap_res)
}


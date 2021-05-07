### This is a calculation of overlap scores by Gustaf Rydevik

calculate_OL<-function(a1,a2){
  overlap<-sum(a1%in%a2)/length(a1)
}

create_overlap_df <- function(subject_df, na.name.rm = TRUE) {
  split_data <- subject_df%>%
  {if (na.name.rm) filter(., !is.na(.$Name)) else .} %>%
  mutate(attr=str_split(Attributes,","))%>%
  select(ResponseId, Subtype, Name, attr)

overlap_data<-split_data%>%full_join(split_data,by=c("ResponseId"))%>%
  mutate(overlap=map2_dbl(attr.x,attr.y,calculate_OL
  ))%>%
  filter(Subtype.x!=Subtype.y)%>%
  filter(!is_empty(attr.x)|!is_empty(attr.y)) %>% # overlap between two empty lists equals 1, so we filter them out here
  group_by(ResponseId) %>%
  mutate(overlap_norm=sum(overlap)/((n_distinct(Subtype.x))*(n_distinct(Subtype.x)-1)))

return(overlap_data)
}

calculate_overlap <- function(subject_df, na.name.rm = TRUE){
  overlap_df <- create_overlap_df(subject_df, na.name.rm = na.name.rm)
  overlap_res <- overlap_df%>%select(ResponseId,overlap_norm)%>%unique()
  return(overlap_res)
}


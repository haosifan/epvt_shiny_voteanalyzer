f_groupmajorities <- function(data){
  group_majorities <- data %>% 
    group_by(title, desc, epgroup) %>% 
    count(result) %>% 
    filter(n == max(n)) %>% 
    mutate(dups = ifelse(duplicated(epgroup) | duplicated(epgroup, fromLast = TRUE), 1,0),
           result = case_when(dups == 1 ~ "draw",
                              TRUE ~ result),
           dups = duplicated(epgroup)) %>% 
    filter(dups == FALSE) %>% 
    rename(majority = result) %>% 
    select(title, desc, majority)
  return(group_majorities)
}
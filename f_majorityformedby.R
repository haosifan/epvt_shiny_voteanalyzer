f_majorityformedby <- function(data){
  majority_formed_by <- data %>% 
    group_by(identifier_vote, date, report, desc, title, `for`, against, abstention) %>% 
    nest() %>% 
    left_join(data %>% 
                group_by(identifier_vote) %>% 
                select(identifier_vote, `for`,against, abstention) %>% 
                pivot_longer(`for`:abstention) %>% 
                slice(which.max(value)) %>% 
                select(identifier_vote, vote_result = name),
              by = "identifier_vote"
    ) %>% 
    left_join(data %>% f_groupmajorities(), by = c("title" = "title", "desc" = "desc")) %>% 
    mutate(epgroup = replace_na(epgroup, "NI"),
           epgroup = case_when(epgroup == "GUE/NGL" ~ "LEFT",
                               epgroup != "GUE/NGL" ~ epgroup)) %>% 
    filter(majority == vote_result) %>% 
    group_by(identifier_vote, date, report, desc, title, `for`, against, abstention, data, vote_result) %>% 
    summarise(majority_formed_by = str_c(epgroup, collapse = ", "))
  return(majority_formed_by)
}



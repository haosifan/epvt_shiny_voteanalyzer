f_make_mep_table <- function(data){
  
  ft_votes <- data %>% 
    mutate(fullname = paste(firstname, lastname, sep = " ")) %>% 
    select(report, result, fullname) %>% 
    pivot_wider(names_from = fullname, values_from = result)
  
  ft_groups <- data %>% 
    mutate(fullname = paste(firstname, lastname, sep = " ")) %>% 
    select(epgroup, fullname) %>% 
    unique() %>% 
    mutate(epgroup = replace_na(epgroup, "NI")) %>% 
    pivot_wider(names_from = fullname, values_from = epgroup) %>% 
    mutate(report = "ep group")
  
  save_header <- bind_rows(ft_groups, ft_votes) %>% names()
  
  bind_and_replace <- bind_rows(ft_groups, ft_votes) %>% 
    clean_names() %>% 
    mutate_all(str_replace_all, pattern = "pro", replacement = "🗸") %>% 
    mutate_all(str_replace_all, pattern = "against", replacement = "🗴") %>% 
    mutate_all(str_replace_all, pattern = "abstention", replacement = "~") %>% 
    mutate_all(str_replace_all, pattern = "did not vote", replacement = "–")
  
  names(bind_and_replace) <- save_header
  
  return(bind_and_replace %>% 
    select(report, dplyr::everything())
  )
}

f_make_mep_table <- function(data, descriptions){
  
  ft_votes <- data %>% 
    mutate(lastname = iconv(lastname,from = "UTF-8", to = "ASCII", sub = ""),
           firstname = iconv(firstname,from = "UTF-8", to = "ASCII", sub = "")) %>% 
    mutate(epgroup = replace_na(epgroup, "NI"),
           epgroup = factor(epgroup, levels = c("GUE/NGL","Greens/EFA","S&D","Renew","EPP","ECR","ID","NI"))) %>% 
    arrange(epgroup, lastname) %>% 
    mutate(fullname = paste(firstname, lastname, sep = " "),
           identifier_vote = as.character(identifier_vote)) %>% 
    select(identifier_vote, result, fullname) %>% 
    pivot_wider(names_from = fullname, values_from = result)
  
  ft_groups <- data %>%
    mutate(lastname = iconv(lastname,from = "UTF-8", to = "ASCII", sub = ""),
           firstname = iconv(firstname,from = "UTF-8", to = "ASCII", sub = "")) %>% 
    mutate(epgroup = replace_na(epgroup, "NI"),
           epgroup = factor(epgroup, levels = c("GUE/NGL","Greens/EFA","S&D","Renew","EPP","ECR","ID","NI"))) %>% 
    arrange(epgroup, lastname) %>%
    mutate(fullname = paste(firstname, lastname, sep = " ")) %>%
    select(epgroup, fullname) %>%
    unique() %>%
    pivot_wider(names_from = fullname, values_from = epgroup) %>%
    mutate(identifier_vote = "ep group")

  save_header <- bind_rows(ft_groups, ft_votes) %>% names()

  bind_and_replace <- bind_rows(ft_groups, ft_votes) %>%
    clean_names() %>%
    mutate_all(str_replace_all, pattern = "for", replacement = "\U2713") %>% 
    mutate_all(str_replace_all, pattern = "against", replacement = "\U2717") %>%
    mutate_all(str_replace_all, pattern = "abstention", replacement = "~") %>%
    mutate_all(str_replace_all, pattern = "did not vote", replacement = "–")
  
  # t_flex <- bind_and_replace %>%
  #   mutate_at(.vars = vars(!contains("identifier")),
  #             factor,
  #             levels = c("EPP", "S&D", "GUE/NGL", "Greens/EFA","Renew","ECR","ID","NI", "\U0001f5f8","\U0001f5f4","~"))
  
  t_flex <- bind_and_replace %>%
    mutate_at(.vars = vars(!contains("identifier")),
              factor,
              levels = c("EPP", "S&D", "GUE/NGL", "Greens/EFA","Renew","ECR","ID","NI", "\U2713","\U2717","~"))

  
  bgcolour <- scales::col_factor(
    palette = c("#3399FF","#FF0000","#990000", "#009900", "#FFFF00", "#1866A5", "#26428B", "gray70", "green", "coral1", "mediumorchid3"), 
    na.color = "gray80", 
    domain = NULL)
  
  names(t_flex) <- save_header
  
  # Column text instead of report/identifier
  text <- descriptions %>% 
    mutate(text = paste0(str_trunc(title, width = 20), str_trunc(desc, width = 15, side = "left", ellipsis = "")),
           identifier = as.character(identifier)) %>%
    select(identifier, text)
  
  t_flex <- t_flex %>% select(identifier_vote, dplyr::everything()) %>% 
    mutate(identifier_vote = case_when(identifier_vote == "ep group" ~ "",
                                       TRUE ~ identifier_vote)) %>%
    left_join(., text, by = c("identifier_vote" = "identifier")) %>% 
    select(text, dplyr::everything(), -identifier_vote)
  
  ftable <- flextable(t_flex) %>%
    flextable::bg(j = names(t_flex)[-1], bg = bgcolour) %>%
    merge_h(i = 1) %>%
    align(align = "center", part = "all") %>%
    border(i = 2:dim(t_flex)[1], border.right = fp_border_default()) %>%
    border(i = 1, border.bottom = officer::fp_border(color = "black", width = 1.5))
  
  return(ftable)
             
}

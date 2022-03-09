f_plot_cohesion_density <- function(df_groupvotecohesion){
  plot <- df_groupvotecohesion %>% 
    ungroup() %>% 
    select(-identifier_vote, -date, -report, -desc, -title, -`for`, 
           -against, -abstention, -committee) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(name = case_when(name == "ecr" ~ "ECR",
                            name == "gue_ngl" ~ "LEFT",
                            name == "id" ~ "ID",
                            name == "na" ~ "NI",
                            name == "epp" ~ "EPP",
                            name == "renew" ~ "RE",
                            name == "s_d" ~ "S&D",
                            name == "greens_efa" ~ "Greens/EFA"
    ),
    name = factor(name, levels = c("LEFT","Greens/EFA","S&D","RE","EPP","ECR","ID","NI")),
    value = value*100) %>% 
    ggplot()+
    geom_density(aes(x = value), fill = "blue", alpha = .3)+
    scale_x_continuous(limits = c(0,100))+
    facet_wrap(~ name, scales = "free_y", nrow = 2)+
    labs(x = "Cohesion Rate (%)",
         y = "Density")+
    theme_minimal(base_size = 20)
  return(plot)
}
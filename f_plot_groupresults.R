f_plot_groupresults <- function(data){
  helper <- tibble(n=1:300) %>% 
    mutate(cols = rep(1:10, 30),
           rows = rep(1:30,each = 10))
  
  df_plot <- data %>% 
    mutate(result = factor(result, levels = c("for","against","abstention")),
           epgroup = replace_na(epgroup, "NI"),
           epgroup = factor(epgroup, levels = c("GUE/NGL","Greens/EFA","S&D","Renew","EPP","ECR","ID","NI")),
           fullname = paste0(firstname, " ", lastname)) %>% 
    group_by(epgroup) %>% 
    arrange(epgroup, result) %>% 
    mutate(n = 1:n()) %>% 
    left_join(., helper, by = "n") %>% 
    select(epgroup, n, rows, cols, result, fullname)
  
  p_groupresults <- ggplot(df_plot)+
    aes(x = rows, y = cols, color = result, text = fullname)+
    geom_point()+
    facet_wrap(~epgroup, ncol = 8)+
    coord_fixed(ratio = 1)+
    theme_minimal()+
    labs(x = NULL, y = NULL)+
    scale_color_manual(name = NULL, values = c("for" = "green3", "against" = "red", "abstention" = "blue"))+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  plotly_output <- ggplotly(p_groupresults, tooltip = "text", height = 200) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  
  return(plotly_output)
}


f_makeresultdonut <- function(data){
  df_plot_donut <- data %>% 
    # select(date:committee) %>% 
    # distinct() %>% 
    # filter(report == "B9-0099/2022") %>% 
    select(`for`:abstention) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(share = value/sum(value),
           ymax = cumsum(share),
           ymin = lag(ymax),
           ymin = case_when(is.na(ymin) ~ 0,
                            !is.na(ymin) ~ ymin)) %>% 
    mutate_at(.vars = vars(starts_with("y")), rescale, to=pi*c(-.5,.5), from=0:1) %>% 
    mutate(name = factor(name, levels = c("for","against","abstention")))
  
  p_result_donut <- ggplot(df_plot_donut)+
    aes(text = round(share*100, 1))+
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .5, r = 1, start = ymin, end = ymax, fill=name)) + 
    geom_vline(xintercept = 0, linetype="dashed")+
    coord_fixed()+
    scale_fill_manual(name = NULL, 
                      values = c("for" = "green2", "against" = "red", "abstention" = "lightblue"))+
    theme_void()+
    theme(legend.position = "bottom")
  
  return(p_result_donut)
}
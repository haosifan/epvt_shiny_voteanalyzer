f_agreementindex <- function(m, type = c("group","vote","group_vote")){
  if (type == "group") {
    df <- m %>% group_by(identifier_vote, date, report, desc, title, `for`, 
                         against, abstention, committee, epgroup)
  }
  
  if (type == "vote"){
    df <- m %>% group_by(identifier_vote, date, report, desc, title, `for`, 
                         against, abstention, committee)
  }
  
  if (type == "group_vote") {
    df <- m %>% group_by(identifier_vote, date, report, desc, title, `for`, 
                         against, abstention, committee, epgroup)
  }
  
  df_ai <- df %>% 
    summarise(Y = sum(result == "for"),
              N = sum(result == "against"),
              A = sum(result == "abstained"),
              Ai = ((max(Y,N,A)-(0.5*((Y+N+A)-max(Y+N+A))))/(Y+N+A)))
  
  if (type == "group_vote") {
    output_ai <- df_ai %>% 
      select(epgroup, Ai) %>% 
      pivot_wider(names_from = epgroup, values_from = Ai) %>% 
      clean_names()
  }
  
  if (type == "vote") {
    output_ai <- df_ai %>% 
      select(vote_ai = Ai)
  }
  
  if (type == "group"){
    output_ai <- df_ai %>% 
      group_by(epgroup) %>% 
      summarise(Ai = mean(Ai, na.rm = TRUE)) %>% 
      pivot_wider(names_from = epgroup, values_from = Ai) %>% 
      clean_names()
  }
  
  
  return(output_ai)
}
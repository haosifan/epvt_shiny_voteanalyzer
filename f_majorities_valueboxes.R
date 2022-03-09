f_majorities_valuebox <- function(df_majorities, party){
  majority <- df_majorities %>% filter(epgroup == party) %>% pull(majority)
  if(majority == "for"){
    value <- "in favour"
    icon <- icon("thumbs-up")
    color <- "green"
  } else {
    if(majority == "against"){
      value <- "against"
      icon <- icon("thumbs-down")
      color <- "red"
    } else {
      if(majority == "abstention"){
        value <- "abstained"
        icon <- icon("minus")
        color <- "aqua"
      } else {
        value <- "draw"
        icon <- icon("tilde")
        color <- "yellow"
      }
    }
  }
  return(list(party = party, value = value, icon = icon, color = color))
}
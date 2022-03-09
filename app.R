## app.R ##
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(janitor)
library(DT)
library(ggplot2)

source("f_agreementindex.R") 
source("f_groupmajorities.R")
source("f_majorities_valueboxes.R")
source("f_groupvotecohesion.R")




## Download overview-data from mepwatch.eu

descriptions <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/text_tabled.csv") %>% 
    select(reference, title, committee, oeil)

item_rollcall <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/item_rollcall.csv")

rollcall_descriptions <- left_join(item_rollcall, descriptions, by = c("report" = "reference")) %>% 
    select(identifier:report, desc = `title.x`, title = `title.y`, everything(), -desc)

mep_infos <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/meps.csv")

#################
###### UI #######
#################

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Search and Select", tabName = "search", icon = icon("search")),
        menuItem("Inspect single vote stats", tabName = "singlevote", icon = icon("vote-yea"), badgeColor = "green"),
        menuItem("Inspect overall stats", tabName = "multvotes", icon = icon("poll-h"), badgeColor = "green"),
        menuItem("MEPs", tabName = "MEPs", icon = icon("users"), badgeColor = "blue"),
        menuItem("Download", tabName = "download", icon = icon("download"), badgeColor = "red"),
        dateRangeInput("daterange_files", "Date range:",
                       start = "2019-07-01",
                       end   = NULL,
                       weekstart = 1),
        fileInput("votes_input", "Upload Data", accept = ".csv")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "search",
                h2("Search and select"),
                h4("First, select all files you want to inspect"),
                fluidRow(
                    DT::dataTableOutput("files") 
                ),
                hr(),
                #checkboxInput("finalvotes", "Select only final votes", value = FALSE, width = NULL),
                fluidRow(
                    DT::dataTableOutput("votes")
                ),
                actionButton("submit_votes", "Submit and Download"),
                hr()
        ),
        
        tabItem(tabName = "singlevote",
                h2("Analyse votes")
        ),
        tabItem(tabName = "multvotes",
                h2("Statistics"),
                h3("Dataset Statistics (All selected votes)"),
                fluidRow(
                    valueBoxOutput("epp_cohesion"),
                    valueBoxOutput("sd_cohesion"),
                    valueBoxOutput("greens_cohesion"),
                    valueBoxOutput("re_cohesion"),
                    valueBoxOutput("ecr_cohesion"),
                    valueBoxOutput("id_cohesion"),
                    valueBoxOutput("left_cohesion"),
                    valueBoxOutput("ni_cohesion")
                ),
                fluidRow(
                    plotOutput("cohesion_density")  
                ),
                h3("Single vote statistics"),
                fluidRow(
                    selectInput(inputId = "vote_selector_stats", 
                                label = NULL, 
                                choices = NULL,
                                width = "100%"),
                    textOutput("test2"),
                    dataTableOutput("test")
                    ),
                fluidRow(
                    valueBoxOutput("epp_majority"),
                    valueBoxOutput("sd_majority"),
                    valueBoxOutput("ecr_majority"),
                    valueBoxOutput("greens_majority"),
                    valueBoxOutput("id_majority"),
                    valueBoxOutput("left_majority"),
                    valueBoxOutput("renew_majority"),
                    valueBoxOutput("ni_majority")
                )
                ),
        tabItem(tabName = "meps",
                h2("Analyse MEPs")
        ),
        tabItem(tabName = "download",
                h2("Download Section"),
                fluidRow(
                    DT::dataTableOutput("mepvotes")
                ),
    )
    )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
    dashboardHeader(title = "EPvotetracker@UDE"),
    sidebar,
    body
)

##################
##### SERVER #####
##################


server <- function(input, output, session) {
    # Upload data
    datasetInput <- eventReactive(input$votes_input, {
        infile <- input$votes_input
        read_csv(infile$datapath)
    })
    
    ## Output of Files
    
    output_files_df <- reactive({
        startdate <- format(input$daterange_files[1])
        enddate <- format(input$daterange_files[2])
        
        output_files <- rollcall_descriptions %>% 
            filter(date >= startdate, date <= enddate) %>% 
            arrange(desc(date)) %>% 
            select(report, title) %>% 
            distinct()
        
        return(output_files)
    })
    
    ## Output of Votes
    
    output_votes_df <- reactive({
        fileids_selected <- output_files_df() %>% 
            rownames_to_column("rowid") %>% 
            mutate(rowid = as.numeric(rowid)) %>% 
            filter(rowid %in% input$files_rows_selected) %>% 
            pull(report)
        
        output_votes <- rollcall_descriptions %>% 
            select(report, date, title, desc, `for`, against, abstention, committee) %>% 
            filter(report %in% fileids_selected) %>% 
            arrange(desc(date))
        
        return(output_votes)
    })
    
    ## Retrieve file IDs and download
    
    output_mepvotes_df <- eventReactive(input$submit_votes, {
        voteids_selected <- output_votes_df() %>% 
            rownames_to_column("rowid") %>% 
            mutate(rowid = as.numeric(rowid)) %>% 
            filter(rowid %in% input$votes_rows_selected) %>% 
            left_join(., rollcall_descriptions, 
                      by = c("report","date","title","desc","for","against","abstention","committee")) %>% 
            pull(identifier)
        
        download_links <- paste0("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/cards/",voteids_selected,".csv")
        
        votes_mep_downloaded <- map_df(download_links, read_csv) %>%
            left_join(., mep_infos, by = c("mepid" = "voteid")) %>%
            left_join(., rollcall_descriptions, by = c("identifier" = "identifier")) %>% 
            select(identifier_vote = identifier, 
                   date:oeil, 
                   mepid, lastname, firstname, result, 
                   epgroup = `eugroup.x`, party, 
                   start:country, term, -oeil) %>% 
            arrange(lastname)
        
        return(votes_mep_downloaded)
    })
    
### Table Outputs (Search and select) ####
    
    output$files <- DT::renderDataTable(output_files_df(), server = TRUE)
    output$votes <- DT::renderDataTable(output_votes_df(), server = TRUE)
    output$mepvotes <- DT::renderDataTable(output_mepvotes_df(), 
                                           server = TRUE, 
                                           options = list(scrollX = TRUE))
    
##### END OF PAGE1 - SEARCH AND SELECT #####
    
##### PAGE 3 - MULTIPLE VOTES STATS #####   
    choices_vote_stats <-  observeEvent(input$submit_votes, {
        x <- output_votes_df() %>% 
            rownames_to_column("rowid") %>% 
            mutate(rowid = as.numeric(rowid)) %>% 
            filter(rowid %in% input$votes_rows_selected) %>% 
            mutate(output = paste(title, desc, sep = " - ")) %>% 
            pull(output)

        # Can also set the label and select items
        updateSelectInput(session, "vote_selector_stats",
                          choices = x,
                          selected = NULL
        )
    })
    
    #### Update data based on selection ####
    
    
    group_cohesion <- eventReactive(input$submit_votes, {
        output_mepvotes_df() %>% f_agreementindex("group")
        })
    group_majorities <- reactive({
        output_mepvotes_df() %>% 
            f_groupmajorities() %>% 
            left_join(output_votes_df(), by = c("title" = "title", "desc" = "desc")) %>% 
            mutate(selection = paste(title, desc, sep = " - ")) %>% 
            filter(selection == input$vote_selector_stats)
        })
    groupvote_cohesion <- eventReactive(input$submit_votes, {
        output_mepvotes_df() %>% f_agreementindex("group_vote")
        })
    
    ####
    
    ##### MAJORITY VALUE BOXES
    
    valueboxattr_epp <- reactive({f_majorities_valuebox(group_majorities(), "EPP")})
    valueboxattr_sd <- reactive({f_majorities_valuebox(group_majorities(), "S&D")})
    valueboxattr_ecr <- reactive({f_majorities_valuebox(group_majorities(), "ECR")})
    valueboxattr_left <- reactive({f_majorities_valuebox(group_majorities(), "GUE/NGL")})
    valueboxattr_id <- reactive({f_majorities_valuebox(group_majorities(), "ID")})
    valueboxattr_greens <- reactive({f_majorities_valuebox(group_majorities(), "Greens/EFA")})
    valueboxattr_renew <- reactive({f_majorities_valuebox(group_majorities(), "Renew")})
    valueboxattr_ni <- reactive({f_majorities_valuebox(group_majorities(), "")})
    
    output$epp_majority <- renderValueBox({valueBox(value = valueboxattr_epp()$value,
                                                    icon = valueboxattr_epp()$icon,
                                                    color = valueboxattr_epp()$color,
                                                    subtitle = "EPP majority")})
    
    output$sd_majority <- renderValueBox({valueBox(value = valueboxattr_sd()$value,
                                                    icon = valueboxattr_sd()$icon,
                                                    color = valueboxattr_sd()$color,
                                                    subtitle = "S&D majority")})
    
    output$ecr_majority <- renderValueBox({valueBox(value = valueboxattr_ecr()$value,
                                                    icon = valueboxattr_ecr()$icon,
                                                    color = valueboxattr_ecr()$color,
                                                    subtitle = "ECR majority")})
    
    output$left_majority <- renderValueBox({valueBox(value = valueboxattr_left()$value,
                                                    icon = valueboxattr_left()$icon,
                                                    color = valueboxattr_left()$color,
                                                    subtitle = "LEFT majority")})
    
    output$id_majority <- renderValueBox({valueBox(value = valueboxattr_id()$value,
                                                    icon = valueboxattr_id()$icon,
                                                    color = valueboxattr_id()$color,
                                                    subtitle = "ID majority")})
    
    output$greens_majority <- renderValueBox({valueBox(value = valueboxattr_greens()$value,
                                                    icon = valueboxattr_greens()$icon,
                                                    color = valueboxattr_greens()$color,
                                                    subtitle = "Greens/EFA majority")})
    
    output$renew_majority <- renderValueBox({valueBox(value = valueboxattr_renew()$value,
                                                    icon = valueboxattr_renew()$icon,
                                                    color = valueboxattr_renew()$color,
                                                    subtitle = "RENEW majority")})
    
    output$ni_majority <- renderValueBox({valueBox(value = valueboxattr_ni()$value,
                                                      icon = valueboxattr_ni()$icon,
                                                      color = valueboxattr_ni()$color,
                                                      subtitle = "NI majority")})

    #### END MAJORITY VALUE BOXES
    
    #### DENSITY PLOT ####
    
    output$cohesion_density <- renderPlot({f_plot_cohesion_density(groupvote_cohesion())},height = 400)
    
    #### END DENSITY PLOT ####
    
    
        
    # output$test <- renderDataTable({group_majorities()}, options = list(scrollX = TRUE))
    # output$test2 <- renderText({valueboxattr_epp()$value})

    
    ####### Cohesion Value Boxes
    
    output$epp_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$epp*100,0),"%"),
                 subtitle = "EPP Cohesion", color = "light-blue", width = "12%")
    })

    output$sd_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$s_d*100,0),"%"),
                 subtitle = "S&D Cohesion", color = "red", width = "12%")
    })

    output$greens_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$greens_efa*100,0),"%"),
                 subtitle = "Greens/EFA Cohesion", color = "green", width = "12%")
    })

    output$re_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$renew*100,0),"%"),
                 subtitle = "RENEW Cohesion", color = "yellow", width = "12%")
    })

    output$ecr_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$ecr*100,0),"%"),
                 subtitle = "ECR Cohesion", color = "blue", width = "12%")
    })

    output$id_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$id*100,0),"%"),
                 subtitle = "ID Cohesion", color = "navy", width = "12%")
    })

    output$left_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$gue_ngl*100,0),"%"),
                 subtitle = "LEFT Cohesion", color = "purple", width = "12%")
    })

    output$ni_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$na*100,0),"%"),
                 subtitle = "NI Cohesion", color = "black", width = "12%")
    })
    
    ## END COHESION VALUE BOXES
    

##### PAGE 3 - MULTIPLE VOTES STATS #####  

}

shinyApp(ui, server)
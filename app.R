## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(janitor)
library(DT)
library(ggplot2)
library(lubridate)
library(labelled)

source("f_agreementindex.R") 
source("f_groupmajorities.R")
source("f_majorities_valueboxes.R")
source("f_groupvotecohesion.R")
source("f_mepoverview_ftable.R")




## Download overview-data from mepwatch.eu

descriptions <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/text_tabled.csv") %>% 
    select(reference, title, committee, oeil)

item_rollcall <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/item_rollcall.csv")

rollcall_descriptions <- left_join(item_rollcall, descriptions, by = c("report" = "reference")) %>% 
    select(identifier:report, desc = `title.x`, title = `title.y`, everything(), -desc)

mep_infos <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/meps.csv") %>% 
    arrange(lastname)

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
                h4("Second, select specific votes within the files you want to inspect"),
                #checkboxInput("finalvotes", "Select only final votes", value = FALSE, width = NULL),
                fluidRow(
                    DT::dataTableOutput("votes")
                ),
                actionButton("submit_votes", "Submit and Download"),
                hr()
        ),
        
        tabItem(tabName = "singlevote",
                h2("Single vote statistics"),
                fluidRow(
                    selectInput(inputId = "vote_selector_stats", 
                                label = NULL, 
                                choices = NULL,
                                width = "100%"),
                ),
                fluidRow(
                    plotOutput("plot_result_single_vote")
                ),
                fluidRow(
                    valueBoxOutput("epp_majority",width = 3),
                    valueBoxOutput("sd_majority",width = 3),
                    valueBoxOutput("ecr_majority",width = 3),
                    valueBoxOutput("greens_majority",width = 3),
                    valueBoxOutput("id_majority",width = 3),
                    valueBoxOutput("left_majority",width = 3),
                    valueBoxOutput("renew_majority",width = 3),
                    valueBoxOutput("ni_majority",width = 3)
                )
        ),
        tabItem(tabName = "multvotes",
                h2("Statistics"),
                h3("Dataset Statistics (All selected votes)"),
                fluidRow(
                    valueBoxOutput("epp_cohesion", width = 1),
                    valueBoxOutput("sd_cohesion", width = 1),
                    valueBoxOutput("greens_cohesion", width = 1),
                    valueBoxOutput("re_cohesion", width = 1),
                    valueBoxOutput("ecr_cohesion", width = 1),
                    valueBoxOutput("id_cohesion", width = 1),
                    valueBoxOutput("left_cohesion", width = 1),
                    valueBoxOutput("ni_cohesion", width = 1)
                ),
                fluidRow(
                    plotOutput("cohesion_density")
                )
                
        ),
        tabItem(tabName = "MEPs",
                h2("MEPs statistics"),
                fluidRow(
                    box(
                        pickerInput("country_select", label = "Filter MEPs by country", 
                                    choices = unique(mep_infos$country), multiple = TRUE,
                                    selected = unique(mep_infos$country),
                                    options = list(`actions-box` = TRUE))),
                    box(
                        pickerInput("group_select", label = "Filter MEPs by group affiliation",
                                    choices = unique(mep_infos$eugroup), multiple = TRUE,
                                    selected = unique(mep_infos$eugroup), 
                                    options = list(`actions-box` = TRUE)))
                ),
                fluidRow(
                    box(width = 12,
                    pickerInput("mep_select", label = "Select one ore more MEPs", 
                                choices = NULL, multiple = TRUE,
                                options = list(`actions-box` = TRUE,
                                               `live-search` = TRUE)),
                    textOutput("test1"),
                    ),
                fluidRow(
                    box(width = 12,
                        dataTableOutput("mep_stats")
                        )
                )
                )
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
    
    output_files_df <- reactiveValues()
    
    observe({
        startdate <- format(input$daterange_files[1])
        enddate <- format(input$daterange_files[2])
        
        output_files_df$data <- rollcall_descriptions %>% 
            filter(date >= startdate, date <= enddate) %>% 
            arrange(desc(date)) %>% 
            select(report, title) %>% 
            distinct()
    })
    
    ## Output of Votes
    
    output_votes_df <- reactiveValues()
    
    observe({
        fileids_selected <- output_files_df$data %>% 
            rownames_to_column("rowid") %>% 
            mutate(rowid = as.numeric(rowid)) %>% 
            filter(rowid %in% input$files_rows_selected) %>% 
            pull(report)
        
        output_votes_df$data <- rollcall_descriptions %>% 
            select(report, date, title, desc, `for`, against, abstention, committee) %>% 
            filter(report %in% fileids_selected) %>% 
            arrange(desc(date))
    })
    
    ## Retrieve file IDs and download
    
    output_mepvotes_df <- reactiveValues()
    
    observeEvent(input$submit_votes, {
        shinyalert(title = "Data being prepared.",
                   text = "This might take a while. You will see another notification when finished.",
                   type = "info",
                   timer = 5*1000, 
                   showConfirmButton = FALSE)
        
        voteids_selected <- output_votes_df$data %>% 
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
        
        output_mepvotes_df$data <- votes_mep_downloaded
        shinyalert(title = "Check!", text = "Data is ready.", type = "success")
    })
    
### Table Outputs (Search and select) ####
    
    output$files <- DT::renderDataTable(output_files_df$data, server = TRUE)
    output$votes <- DT::renderDataTable(output_votes_df$data, server = TRUE)
    output$mepvotes <- DT::renderDataTable(output_mepvotes_df$data, 
                                           server = TRUE, 
                                           options = list(scrollX = TRUE))
    
##### END OF PAGE1 - SEARCH AND SELECT #####
    
##### PAGE 3 - MULTIPLE VOTES STATS #####   
    choices_vote_stats <-  observeEvent(input$submit_votes, {
        x <- output_votes_df$data %>% 
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
        output_mepvotes_df$data %>% f_agreementindex("group")
        })
    group_majorities <- reactive({
        output_mepvotes_df$data %>% 
            f_groupmajorities() %>% 
            left_join(output_votes_df$data, by = c("title" = "title", "desc" = "desc")) %>% 
            mutate(selection = paste(title, desc, sep = " - ")) %>% 
            filter(selection == input$vote_selector_stats)
        })
    groupvote_cohesion <- eventReactive(input$submit_votes, {
        output_mepvotes_df$data %>% f_agreementindex("group_vote")
        })
    
    ####
    
    #### Graph Result Single Vote
    
    
    
    
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

##### PAGE 4 - MEPs #####################
    

    mep_infos_update <- reactive({
        r <- mep_infos %>% 
            arrange(lastname) %>% 
            filter(country %in% input$country_select) %>% 
            filter(eugroup %in% input$group_select) %>% 
            mutate(html = paste("<span style='font-size: 80%; color: grey'>",firstname,"</span>"," ",
                               "<span style='font-size: 100%;'>",lastname,"</span>"))
        return(r)
        })
    
    observe({
        updatePickerInput(
            session,
            "mep_select",
            choices = mep_infos_update()$voteid,
            choicesOpt = list(
                content = mep_infos_update()$html))})
        
    
    observe({output$test1 <- renderText(input$mep_select)})
    
    mep_table <- reactive({
        if (!is.null(input$mep_select)){
            data <- output_mepvotes_df$data %>%
                filter(mepid %in% input$mep_select) %>%
                filter(country %in% input$country_select) %>% 
                filter(epgroup %in% input$group_select) %>% 
                arrange(lastname)
        } else {
            data <- output_mepvotes_df$data %>%
                filter(country %in% input$country_select) %>% 
                filter(epgroup %in% input$group_select) %>% 
                arrange(lastname)
        }
        
        output_mep_table <- data %>% 
            group_by(mepid, lastname, firstname, country, epgroup, party) %>%
            mutate(result = factor(result, levels = c("for","against","abstention"))) %>% 
            dplyr::count(result, .drop = FALSE) %>% 
            pivot_wider(names_from = result, values_from = n, values_fill = 0) %>% 
            mutate(Ai = ((max(`for`,against,abstention)-(0.5*((`for`+against+abstention)-max(`for`+against+abstention))))/(`for`+against+abstention))) %>% 
            arrange(lastname)
              
        return(output_mep_table)
    })
    
    # mep_table_overview <- eventReactive(input$mep_select, {
    #     
    #     data <- output_mepvotes_df$data %>% filter(mepid %in% input$mep_select) %>% arrange(lastname)
    #     output_mep_table <- f_make_mep_table(data)
    #     
    #     return(output_mep_table)
    # })
    
    output$mep_stats <- renderDataTable({mep_table() %>% mutate(Ai = round(Ai*100,2))})
    #output$test2 <- renderDataTable({mep_table_overview()})
}

shinyApp(ui, server)
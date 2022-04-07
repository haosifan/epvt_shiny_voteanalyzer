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
library(plotly)
library(flextable)
library(ggforce)
library(scales)
library(stringr)
library(forcats)

source("f_agreementindex.R") 
source("f_groupmajorities.R")
source("f_majorities_valueboxes.R")
source("f_groupvotecohesion.R")
source("f_mepoverview_ftable.R")
source("f_plot_groupresults.R")
source("f_makeresultdonut.R")
source("f_majorityformedby.R")




## Download overview-data from mepwatch.eu

descriptions <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/text_tabled.csv") %>% 
    select(reference, title, committee, oeil)

item_rollcall <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/item_rollcall.csv")

rollcall_descriptions <- left_join(item_rollcall, descriptions, by = c("report" = "reference")) %>% 
    select(identifier:report, desc = `title.x`, title = `title.y`, everything(), -desc)

mep_infos <- read_csv("https://raw.githubusercontent.com/TechToThePeople/mepwatch/production/9/data/meps.csv") %>% 
    arrange(lastname)

committees <- descriptions$committee %>% 
    replace_na(replace = "no committee") %>% 
    unique() %>% 
    sort() %>% 
    as_factor() %>% 
    fct_relevel("Special Committee on Beating Cancer", after = Inf) %>% 
    fct_relevel("Special Committee on Foreign Interference in all Democratic Processes in the European Union, including Disinformation", after = Inf) %>% 
    fct_relevel("no committee", after = Inf) %>% 
    sort() %>% 
    as.character()

#################
###### UI #######
#################

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Search and Select", tabName = "search", icon = icon("search")),
        menuItem("Inspect single vote stats", tabName = "singlevote", icon = icon("vote-yea"), badgeColor = "green"),
        menuItem("Inspect overall stats", tabName = "multvotes", icon = icon("poll-h"), badgeColor = "green"),
        menuItem("MEPs", tabName = "MEPs", icon = icon("users"), badgeColor = "blue"),
        menuItem("Download", tabName = "download", icon = icon("download"), badgeColor = "red")#,
        #fileInput("votes_input", "Upload Data", accept = ".csv")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "search",
                h2("Search and select"),
                h4("First, select all files you want to inspect"),
                fluidRow(
                    column(6,
                           dateRangeInput("daterange_files", "Date range:",
                                          start = "2019-07-01",
                                          end   = NULL,
                                          weekstart = 1),
                           materialSwitch(inputId = "final_votes", label = "Show only final votes in votes section", status = "primary"),
                           materialSwitch(inputId = "activate_committee", label = "Choose specific committees", status = "primary"),
                           ),
                    column(6,
                           checkboxGroupButtons(
                               inputId = "committee_select", label = "Choose one ore more committees", 
                               choices = committees, 
                               selected = committees,
                               justified = FALSE, status = "primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                               disabled = TRUE
                           ))
                ),
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
                    pickerInput(inputId = "vote_selector_stats", 
                                label = NULL, 
                                choices = NULL,
                                width = "100%"),
                ),
                fluidRow(
                    column(4,
                           valueBoxOutput("result_date", width = 12),
                           valueBoxOutput("result_report", width = 12),
                           valueBoxOutput("result_committee", width = 12)
                    ),
                    column(4,
                           valueBoxOutput("result_for", width = 12),
                           valueBoxOutput("result_against", width = 12),
                           valueBoxOutput("result_abstention", width = 12)
                           ),
                    column(4, plotOutput("donut"))
                ),
                fluidRow(
                    plotlyOutput("plot_result_single_vote"),
                    h3("Majority formed by: "),
                    uiOutput("majority_formed_by"),
                    hr()
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
                h2("Statistics for all votes in selected data"),
                h3("Summary of majorities"),
                fluidRow(
                    column(6,
                           h4("Top Majorities"),
                           p("The majorities that are most frequently found in the data set are displayed. The top 3 are displayed, but in some cases more than three are displayed in the case of ties."),
                           dataTableOutput("top_majorities")),
                    column(6,
                           h4("Number EP Groups involved in majorities"),
                           p("In this plot you can find the overview of how often the EP groups were involved in majorities."),
                           plotOutput("n_majorities"))
                ),
                h3("Cohesion Statistics"),
                fluidRow(
                    column(6,
                            valueBoxOutput("greens_cohesion", width = 3),
                            valueBoxOutput("sd_cohesion", width = 3),
                            valueBoxOutput("epp_cohesion", width = 3),
                            valueBoxOutput("re_cohesion", width = 3),
                            valueBoxOutput("ecr_cohesion", width = 3),
                            valueBoxOutput("id_cohesion", width = 3),
                            valueBoxOutput("ni_cohesion", width = 3)
                            ),
                    column(6,
                           plotOutput("cohesion_density")
                           )
                ),
                h3(""),
                fluidRow(
                    column(6,
                           h3("Votes coming from following committees"),
                           dataTableOutput("committee_count"))
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
                        #dataTableOutput("test2"),
                        dataTableOutput("mep_stats"),
                        uiOutput("mep_overview")
                        #dataTableOutput("data_mep")
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
    
    observeEvent(input$activate_committee, {
        
        if (input$activate_committee == TRUE) {
            updateCheckboxGroupButtons(session, inputId = "committee_select", disabled = FALSE, selected = "")}
        else {updateCheckboxGroupButtons(session, inputId = "committee_select", disabled = TRUE, selected = committees)}
        })
    
    ## Output of Files
    
    output_files_df <- reactiveValues()
    
    observe({
        startdate <- format(input$daterange_files[1])
        enddate <- format(input$daterange_files[2])
        
        output_files_df$data <- rollcall_descriptions %>% 
            filter(date >= startdate, date <= enddate) %>% 
            filter(committee %in% input$committee_select) %>% 
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
        
        output_votes_prepared <- rollcall_descriptions %>% 
            select(report, date, title, desc, `for`, against, abstention, committee) %>% 
            filter(report %in% fileids_selected) %>% 
            arrange(desc(date))
        
        ## filter for final votes / or latest vote
        ### filter for keywords
        
        final_votes_keys <- c("Projet du Conseil", 
                              "[Ee]nsemble du texte", 
                              "Proposition de l[ae] Commission", 
                              "Proposition de décision", 
                              #"Accord provisoire",
                              "Proposition de résolution",
                              "Vote unique",
                              "Vote final") %>% 
            paste(collapse = "|")
        
        if (input$final_votes){
            output_votes_df$data <- output_votes_prepared %>% 
                group_by(report) %>% 
                filter(str_detect(desc, pattern = final_votes_keys) | str_detect(report, "\\(COD\\)$|\\(BUD\\)$"))
        } else {output_votes_df$data <- output_votes_prepared}
        
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
            mutate(epgroup = replace_na(epgroup, "NI")) %>% 
            arrange(lastname)
        
        output_mepvotes_df$data <- votes_mep_downloaded
        shinyalert(title = "Check!", text = "Data is ready.", type = "success", 
                   closeOnEsc = FALSE, closeOnClickOutside = FALSE, showCancelButton = FALSE)
    })
    
### Table Outputs (Search and select) ####
    
    output$files <- DT::renderDataTable(output_files_df$data, server = TRUE)
    output$votes <- DT::renderDataTable(output_votes_df$data, server = TRUE)
    output$mepvotes <- DT::renderDataTable(output_mepvotes_df$data, 
                                           server = TRUE, 
                                           options = list(scrollX = TRUE))
    
##### END OF PAGE1 - SEARCH AND SELECT #####

##### PAGE 2 - SINGLE VOTE STATS ########
    
    observeEvent(input$submit_votes, {
        choices <- output_votes_df$data %>% 
            rownames_to_column("rowid") %>% 
            mutate(rowid = as.numeric(rowid)) %>% 
            filter(rowid %in% input$votes_rows_selected) %>% 
            mutate(output = paste(title, desc, sep = " - ")) %>% 
            pull(output)

        # Can also set the label and select items
        updatePickerInput(session, "vote_selector_stats",
                          choices = choices,
                          selected = NULL
        )
    })
    
    ###### Update data based on selection ######
    
    
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
    
    singlevote_data <- reactive({
        singlevote <- output_mepvotes_df$data %>% 
            mutate(selection = paste(title, desc, sep = " - ")) %>% 
            filter(selection == input$vote_selector_stats)
        return(singlevote)
    })
    
    
    
    
    ###### Extract results ######
    
    voteinfo <- reactiveValues()
    observeEvent(input$vote_selector_stats, {
        selected_vote <- singlevote_data() %>%
            select(date:committee) %>%
            mutate(date = ymd_hms(date)) %>% 
            distinct()
        
        voteinfo$date <- selected_vote$date
        voteinfo$report <- selected_vote$report
        voteinfo$rfor <- selected_vote$`for`
        voteinfo$ragainst <- selected_vote$against
        voteinfo$rabstention <- selected_vote$abstention
        voteinfo$committee <- selected_vote$committee
        
        voteinfo$donut <- f_makeresultdonut(selected_vote)
        
        voteinfo$majorityformedby <- f_majorityformedby(singlevote_data()) %>% 
            pull(majority_formed_by) %>% 
            str_replace_all(pattern = ",", replacement = "") %>% 
            str_replace(pattern = "EPP", replacement = '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/c6/EPP_EP_group_logo_2015.svg/1024px-EPP_EP_group_logo_2015.svg.png" height="50"/>') %>% 
            str_replace(pattern = "S&D", replacement = '<img src="https://www.socialistsanddemocrats.eu/sites/default/files/styles/large/public/2019-11/logo_white_solo.png?itok=e7BDUZ1M" height="50"/>') %>% 
            str_replace(pattern = "ECR", replacement = '<img src="https://upload.wikimedia.org/wikipedia/commons/5/55/ECR_Group_logo.png?20200219214944" height="50"/>') %>% 
            str_replace(pattern = "LEFT", replacement = '<img src="https://europa.blog/wp-content/uploads/2021/02/The-Left-rund_400x400.jpg" height="50"/>') %>% 
            str_replace(pattern = "Greens/EFA", replacement = '<img src="https://yt3.ggpht.com/ytc/AKedOLRsCh3D-EDf9AGBloxjRAaFONPaaTSufIzGmIFE=s900-c-k-c0x00ffffff-no-rj" height="50"/>') %>% 
            str_replace(pattern = "ID", replacement = '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Identity_and_democracy_logo.png/200px-Identity_and_democracy_logo.png" height="50"/>') %>% 
            str_replace(pattern = "NI", replacement = '<img src="https://raw.githubusercontent.com/haosifan/epvt_shiny_voteanalyzer/main/NI.png" height="50"/>') %>% 
            str_replace(pattern = "Renew", replacement = '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Logo_of_Renew_Europe.svg/2000px-Logo_of_Renew_Europe.svg.png" height="50"/>')
            
        })
    
    output$donut <- renderPlot({voteinfo$donut}, bg="transparent")
    
    output$result_report <- renderValueBox({
        valueBox(paste(voteinfo$report),
                 subtitle = "report identifier", color = "navy")
    })
    
    output$result_date <- renderValueBox({
        valueBox(paste(as.character(voteinfo$date)),
                 subtitle = "", color = "navy")
    })
    
    output$result_committee <- renderValueBox({
        valueBox(paste0(voteinfo$committee),
                 subtitle = "responsible committee", color = "navy")
    })
    
    output$result_for <- renderValueBox({
        valueBox(paste(voteinfo$rfor),
                 subtitle = "votes in favour", color = "green")
    })
    
    output$result_against <- renderValueBox({
        valueBox(paste(voteinfo$ragainst),
                 subtitle = "votes against", color = "red")
    })
    
    output$result_abstention <- renderValueBox({
        valueBox(paste(voteinfo$rabstention),
                 subtitle = "votes abstained", color = "aqua")
    })
    
    ###### Majority formed by ######
    
    output$majority_formed_by <- renderUI({HTML(voteinfo$majorityformedby)})
    
    
    ###### Graph EPGroups Result Single Vote ######
    
    output$test3 <- renderDataTable({singlevote_data()})
    
    output$plot_result_single_vote <- renderPlotly(f_plot_groupresults(singlevote_data()))
    
    
    ###### Value Boxes Group Majority #######
    
    valueboxattr_epp <- reactive({f_majorities_valuebox(group_majorities(), "EPP")})
    valueboxattr_sd <- reactive({f_majorities_valuebox(group_majorities(), "S&D")})
    valueboxattr_ecr <- reactive({f_majorities_valuebox(group_majorities(), "ECR")})
    valueboxattr_left <- reactive({f_majorities_valuebox(group_majorities(), "GUE/NGL")})
    valueboxattr_id <- reactive({f_majorities_valuebox(group_majorities(), "ID")})
    valueboxattr_greens <- reactive({f_majorities_valuebox(group_majorities(), "Greens/EFA")})
    valueboxattr_renew <- reactive({f_majorities_valuebox(group_majorities(), "Renew")})
    valueboxattr_ni <- reactive({f_majorities_valuebox(group_majorities(), "NI")})
    
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
    
    ##### PAGE 3 - MULTIPLE VOTES STATS ##### 
    
    ###### Top3 majorities and Number Groups are part of the majority ######
    
    observeEvent(input$submit_votes, {
        top_majorities <- f_majorityformedby(output_mepvotes_df$data) %>%
            group_by(majority_formed_by) %>%
            count() %>%
            arrange(-n) %>%
            ungroup() %>% 
            slice_max(n, n = 3) %>% 
            mutate(majority_formed_by = str_replace(majority_formed_by, pattern = "EPP", replacement = '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/c6/EPP_EP_group_logo_2015.svg/1024px-EPP_EP_group_logo_2015.svg.png" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "S&D", replacement = '<img src="https://www.socialistsanddemocrats.eu/sites/default/files/styles/large/public/2019-11/logo_white_solo.png?itok=e7BDUZ1M" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "ECR", replacement = '<img src="https://upload.wikimedia.org/wikipedia/commons/5/55/ECR_Group_logo.png?20200219214944" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "LEFT", replacement = '<img src="https://europa.blog/wp-content/uploads/2021/02/The-Left-rund_400x400.jpg" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "Greens/EFA", replacement = '<img src="https://yt3.ggpht.com/ytc/AKedOLRsCh3D-EDf9AGBloxjRAaFONPaaTSufIzGmIFE=s900-c-k-c0x00ffffff-no-rj" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "ID", replacement = '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Identity_and_democracy_logo.png/200px-Identity_and_democracy_logo.png" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "Renew", replacement = '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Logo_of_Renew_Europe.svg/2000px-Logo_of_Renew_Europe.svg.png" height="30"/>'),
                   majority_formed_by = str_replace(majority_formed_by, pattern = "NI", replacement = '<img src="https://raw.githubusercontent.com/haosifan/epvt_shiny_voteanalyzer/main/NI.png" height="30"/>'),
                   majority_formed_by = str_replace_all(majority_formed_by, pattern = ",", replacement = ""))
        
        part_of_majority <- f_majorityformedby(output_mepvotes_df$data) %>% 
            separate(majority_formed_by, into = c("x1","x2","x3","x4","x5","x6","x7","x8"), sep = ", ") %>% 
            pivot_longer(x1:x8) %>% 
            ungroup() %>% 
            filter(!is.na(value)) %>% 
            count(value) %>% 
            mutate(ep_group = factor(value, levels = c("LEFT","Greens/EFA","S&D","Renew","EPP","ECR","ID","NI"))) %>% 
            arrange(ep_group) %>% 
            select(ep_group, n) %>% 
            ggplot()+
            aes(x = ep_group, y = n)+
            geom_col(fill = "#003194")+
            geom_text(aes(label = n), vjust = -0.5)+
            labs(x = NULL, y = "# group is part of majority")+
            theme_minimal(base_size = 18)
        
        committee_count <- output_mepvotes_df$data %>% 
            group_by(identifier_vote) %>% 
            select(identifier_vote, committee) %>% 
            distinct() %>% 
            ungroup() %>% 
            count(committee) %>% 
            arrange(-n) %>% 
            rename(`# of votes from committee` = n)
        
        output$top_majorities <- renderDataTable({
            top_majorities %>% 
                rename(`Majority formed by` = majority_formed_by, Count = n) %>% 
                DT::datatable(escape = FALSE, options = list(searching = FALSE,
                                                             lengthChange = FALSE)) 
            })
        output$n_majorities <- renderPlot({part_of_majority})
        
        output$committee_count <- renderDataTable({committee_count %>%
                DT::datatable(options = list(searching = FALSE,
                                             lengthChange = FALSE))})
    })
    
    
    ###### Density Plot ######
    
    output$cohesion_density <- renderPlot({f_plot_cohesion_density(groupvote_cohesion())},height = 400)

    
    ###### Cohesion Value Boxes #####
    
    output$epp_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$epp*100,0),"%"),
                 subtitle = "EPP", color = "light-blue", width = "12%")
    })

    output$sd_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$s_d*100,0),"%"),
                 subtitle = "S&D", color = "red", width = "12%")
    })

    output$greens_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$greens_efa*100,0),"%"),
                 subtitle = "Greens/EFA", color = "green", width = "12%")
    })

    output$re_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$renew*100,0),"%"),
                 subtitle = "RENEW", color = "yellow", width = "12%")
    })

    output$ecr_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$ecr*100,0),"%"),
                 subtitle = "ECR", color = "blue", width = "12%")
    })

    output$id_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$id*100,0),"%"),
                 subtitle = "ID", color = "navy", width = "12%")
    })

    output$left_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$gue_ngl*100,0),"%"),
                 subtitle = "LEFT", color = "purple", width = "12%")
    })

    output$ni_cohesion <- renderValueBox({
        valueBox(paste0(round(group_cohesion()$ni*100,0),"%"),
                 subtitle = "NI", color = "black", width = "12%")
    })
    
    ## END COHESION VALUE BOXES
    

##### PAGE 4 - MEPs #####################
    
    mep_infos_update <- reactiveValues()
    
    observe({
        mep_infos_update$data <- mep_infos %>% 
            arrange(lastname) %>% 
            filter(country %in% input$country_select) %>% 
            filter(eugroup %in% input$group_select) %>% 
            mutate(html = paste("<span style='font-size: 80%; color: grey'>",firstname,"</span>"," ",
                               "<span style='font-size: 100%;'>",lastname,"</span>"))
        })
    
    observe({
        updatePickerInput(
            session,
            "mep_select",
            choices = mep_infos_update$data$voteid,
            choicesOpt = list(
                content = mep_infos_update$data$html))})
        
    
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

##### MEP stats tables
        
        output_mep_table <- data %>% 
            group_by(mepid, lastname, firstname, country, epgroup, party) %>%
            mutate(result = factor(result, levels = c("for","against","abstention"))) %>% 
            dplyr::count(result, .drop = FALSE) %>% 
            pivot_wider(names_from = result, values_from = n, values_fill = 0) %>% 
            mutate(Ai = ((max(`for`,against,abstention)-(0.5*((`for`+against+abstention)-max(`for`+against+abstention))))/(`for`+against+abstention))) %>% 
            arrange(lastname)
              
        return(output_mep_table)
    })
    
     mep_table_overview <- eventReactive(input$mep_select, {
         
         data <- output_mepvotes_df$data %>% filter(mepid %in% input$mep_select) %>% arrange(lastname)
         output_mep_overview <- data %>% f_make_mep_table(descriptions = rollcall_descriptions)
         
         return(output_mep_overview)
     })
    
    output$mep_stats <- renderDataTable({mep_table() %>% mutate(Ai = round(Ai*100,2))})
    output$mep_overview <- renderUI({mep_table_overview() %>% htmltools_value()})
}

shinyApp(ui, server)
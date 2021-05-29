# Load packages---------
library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(kableExtra)
library(plotly)
library(readxl)

# # Wrangle dataset---------
source("helper-files/wrangle-answer-history-table.R")
source("helper-files/wrangle-answer-history-plotly.R")



# # define some credentials
credentials <- data.frame(
  user = c("Kevin Hallgren", "Christopher",
           "Brett", "Ema"), # mandatory
  password = c("shinymanager", "christopher",
               "brett", "ema"), # mandatory
  comment = c("0", "3", "4", "5"),
  admin = c(TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# Create function that plots graphs---------
plotly_weekly_results <- function(df) {
  
  # IF there are only 4 y tick values
  if (is.na(first(df$y_tick_values_5)) | is.na(first(df$y_tick_labels_5))) {
    
    p <- df %>% 
      ggplot(aes(checkin_start_date, values, 
                 text = paste0(text_choice,'\n',checkin_date_hover))) +
      geom_line(aes(group = 1), color = first(df$line_color)) +
      geom_point(color = first(df$line_color)) +
      # Set x axis breaks to only relevant dates
      scale_x_date(breaks = df$checkin_start_date,
                   labels = df$checkin_date_hover) +
      # Set y axis labels 
      scale_y_continuous(breaks = c(seq(first(df$y_tick_values_1),first(df$y_tick_values_4),
                                        by = first(df$y_tick_values_diff))),
                         limits = c(as.numeric(first(df$y_tick_values_1)), as.numeric(first(df$y_tick_values_4))),
                         labels = str_c(c(first(df$y_tick_labels_1), first(df$y_tick_labels_2), first(df$y_tick_labels_3),
                                          first(df$y_tick_labels_4)), 
                                        "(",
                                        seq(first(df$y_tick_values_1),first(df$y_tick_values_4),
                                            by = first(df$y_tick_values_diff)),
                                        ")")) +
      labs(x = "",
           y = "") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            legend.position = "none")
    
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)
    
    
  } else {  # there are only 5 y tick values
    
    
    p <- df %>% 
      ggplot(aes(checkin_start_date, values, 
                 text = paste0(text_choice,'\n',checkin_date_hover))) +
      geom_line(aes(group = 1), color = first(df$line_color)) +
      geom_point(color = first(df$line_color)) +
      # Set x axis breaks to the only the relevant dates
      scale_x_date(breaks = df$checkin_start_date,
                   labels = df$checkin_date_hover) +
      # Set y axis labels 
      scale_y_continuous(breaks = c(seq(first(df$y_tick_values_1),first(df$y_tick_values_5),
                                        by = first(df$y_tick_values_diff))),
                         limits = c(as.numeric(first(df$y_tick_values_1)), as.numeric(first(df$y_tick_values_5))),
                         labels = str_c(c(first(df$y_tick_labels_1), first(df$y_tick_labels_2), first(df$y_tick_labels_3),
                                          first(df$y_tick_labels_4), first(df$y_tick_labels_5)), 
                                        "(",
                                        seq(first(df$y_tick_values_1),first(df$y_tick_values_5),
                                            by = first(df$y_tick_values_diff)),
                                        ")")) +
      labs(x = "",
           y = "") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            legend.position = "none")
    
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)
  }
}




# Create function that plots Goals graphs---------
## Note: Took out the if statement in this because all goals graph had 4 tick values
plotly_weekly_results_goals <- function(df) {
  
  
  p <- df %>% 
    ggplot(aes(checkin_start_date, values, 
               text = paste0(text_choice,'\n',checkin_date_hover))) +
    geom_line(aes(group = 1), color = first(df$line_color)) +
    geom_point(color = first(df$line_color)) +
    # Set x axis breaks to the only the relevant dates
    scale_x_date(breaks = df$checkin_start_date,
                 labels = df$checkin_date_hover) +
    # Set y axis labels 
    scale_y_continuous(breaks = c(seq(first(df$y_tick_values_1),first(df$y_tick_values_4),
                                      by = first(df$y_tick_values_diff))),
                       limits = c(as.numeric(first(df$y_tick_values_1)), as.numeric(first(df$y_tick_values_4))),
                       labels = str_c(c(first(df$y_tick_labels_1), first(df$y_tick_labels_2), first(df$y_tick_labels_3),
                                        first(df$y_tick_labels_4)), 
                                      "(",
                                      seq(first(df$y_tick_values_1),first(df$y_tick_values_4),
                                          by = first(df$y_tick_values_diff)),
                                      ")")) +
    labs(x = "",
         y = "") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          legend.position = "none")
  
  
  ggplotly(p, tooltip = "text") %>% 
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
    config(displayModeBar = F)
  
}







# Start of shinydashboard---------
header <- dashboardHeader(
  title = "Feedback Report",
  titleWidth = 240
)

sidebar <- dashboardSidebar(
  
  # Insert image of UW Medicine Logo from URL 
  div(img(src = "http://depts.washington.edu/psychcl/PBSCI_UWMC_Gold.jpg", 
          height = 75, width = 240)),
  # Width of the sidebar (in pixels)
  width = 240,
  sidebarMenu(
    
    h1("Welcome!"),
    h4("Please select a patient"),
    # Select patient to review
    uiOutput("patient_id_reactive"),
    
    # Add tabs on Sidebar
    # You can add icons inside menuItem with icon parameter
    menuItem(text = "WEEKLY RESULTS", tabName = "weekly_results"),
    menuItem(text = "GOALS", tabName = "goals_tab"),
    menuItem(text = "ANSWER HISTORY", tabName = "answer_history")
  ),
  
  # Empty spaces to position the contact info
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  
  h5("For assistance or question, 
       please contact study lead Kevin Hallgren at khallgre@uw.edu or call 206-616-2906")
)

body <- dashboardBody(
  
  
  tabItems(
    tabItem(tabName = "weekly_results", 
            
            # Row for graphs
            fluidRow(
              uiOutput(outputId = "graphs_ui")            
            ),
            
            br(),
            
            # Row for Multiple Domains Graph
            fluidRow(
              
              column(width = 9,
                     box(title = "Multiple Domains Graph",
                         footer = "Use the buttons to the right to show multiple recovery domains on the graph above",
                         width = NULL,
                         plotlyOutput("multiple_domains_graph")  
                     )
              ),
              
              column(width = 3,
                     br(),
                     br(),
                     br(),
                     br(),
                     uiOutput(outputId = "switch_ui")  
              )
              
            )
            
    ),
    
    
    tabItem(tabName = "goals_tab",
            
            # Row for graphs
            fluidRow(
              uiOutput(outputId = "goals_graphs_ui")             
            )
            
    ),
    
    
    
    tabItem(tabName = "answer_history",
            tableOutput("answer_history_table")
    )
    
    
  )
)


ui <- dashboardPage(skin = "black", header = header,
                    sidebar = sidebar,
                    body = body)


# shinymanager: Wrap UI with secure_app and enable admin mode 
ui <- secure_app(ui, enable_admin = TRUE)



server <- function(input, output, session) {
  
  
  # call the server part
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "XXXX", # Your sqlite database here
      passphrase = "XXX" # Your passphrase here
    )
  )
  
  
  output$patient_id_reactive <- renderUI({
    # This creates a list called res_auth that contains user info like user,
    # start, expire, and admin
    res_auth <- reactiveValuesToList(res_auth)
    # Specify name of current user
    current_user <- res_auth$user
    # Specify patient id of current user
    current_patient_id <- res_auth$comment
    
    # Show two different patient_ids depending on current user
    if (current_user == "Kevin Hallgren") {
      # Select patient to review
      selectInput("patient_id", "Patient ID",
                  record_id_total)
    } else {
      record_id_total_current <- record_id_total[record_id_total == current_patient_id]
      names(record_id_total_current) <- current_user
      # Select patient to review
      selectInput("patient_id", "Patient ID",
                  record_id_total_current)
    }
    
  })
  
  
  
  # Link to weekly-results.xlsx updating every 1 second---------------------------
  updated_data_weekly_results <- reactiveFileReader(
    intervalMillis = 1000000,
    session = session,
    filePath = "XXX", # Your Google Sheets here
    readFunc = googlesheets4::read_sheet
  )
  
  
  
  # Start of Weekly Results Graph---------
  
  # Manipulate weekly results instructions file---------
  weekly_results_processed <- reactive({
    
    weekly_results <- updated_data_weekly_results() %>% 
      # Convert characters to lowercase
      mutate(yes_no = str_to_lower(yes_no)) %>% 
      # Filter out the “no” rows
      filter(yes_no != "no") %>% 
      # Take out yes_no column
      select(-yes_no) %>% 
      # Filter for outcomes
      filter(outcome_or_goal == "outcome") %>% 
      # Take out outcome_or_goal column
      select(-outcome_or_goal)
    
    # Loop through entire excel file and find maximum length inside y_tick_values
    lengths_y_tick_values <- vector("numeric", length = nrow(weekly_results))
    
    for (ii in 1:nrow(weekly_results)) {
      
      lengths_y_tick_values[ii] <- length(str_split(weekly_results[ii,"y_tick_values"],"\\|")[[1]])
      
    }
    
    
    # Find max_length
    max_length_y_tick_values <- max(lengths_y_tick_values)
    
    
    
    # Loop through entire excel file and find maximum length inside y_tick_labels
    lengths_y_tick_labels <- vector("numeric", length = nrow(weekly_results))
    
    for (ii in 1:nrow(weekly_results)) {
      
      lengths_y_tick_labels[ii] <- length(str_split(weekly_results[ii,"y_tick_labels"],"\\|")[[1]])
      
    }
    # Find max_length
    max_length_y_tick_labels <- max(lengths_y_tick_labels)
    
    # Separate 
    weekly_results_processed <- weekly_results %>% 
      separate(y_tick_values, into = paste0("y_tick_values_", 1:max_length_y_tick_values)) %>% 
      separate(y_tick_labels, into = paste0("y_tick_labels_", 1:max_length_y_tick_labels), sep = "\\|") 
    
    
    # Final output
    weekly_results_processed
    
  })
  
  
  
  
  # Inner join answer_history_plotly and weekly_results_processed 
  answer_history_plotly_instructions <- reactive({
    
    answer_history_plotly %>% 
      inner_join(weekly_results_processed(), by = c("participant_info" = "measure_name"))
    
  }) 
  
  
  
  # Create reactive object of graphs plotted from map
  graphs <- reactive({
    
    req(input$patient_id)
    
    answer_history_plotly_instructions() %>% 
      filter(record_id == input$patient_id) %>% 
      mutate(text_choice = paste0(values)) %>% 
      group_by(participant_info) %>% 
      nest() %>% 
      mutate(
        graphs = map(data, plotly_weekly_results) 
      ) %>% 
      pull(graphs)
    
  })
  
  
  
  
  # UI for Graphs in boxes---------
  output$graphs_ui <- renderUI({
    
    req(input$patient_id)
    
    answer_history_plotly_instructions_data <- answer_history_plotly_instructions() %>% 
      filter(record_id == input$patient_id) %>% 
      mutate(text_choice = paste0(values)) %>% 
      group_by(participant_info) %>% 
      nest() %>% 
      pull(data)
    
    
    iwalk(graphs(), ~{
      output_name <- paste0("plot_", .y)
      output[[output_name]] <- renderPlotly(.x)
    })
    
    
    plots_list <- imap(graphs(), ~{
      
      # Set color of box
      measure_color <- answer_history_plotly_instructions_data %>% 
        nth(.y) %>% 
        pull(line_color) %>% 
        first()
      
      # Set title of graph
      measure_title <- answer_history_plotly_instructions_data %>% 
        nth(.y) %>% 
        pull(measure_display_name) %>% 
        first()
      
      tagList(
        
        
        tags$div(class = "another-box", id = paste0("primary", .y, "_id"),
                 box(width = 12,
                     title = measure_title, 
                     status = "primary", solidHeader = TRUE,
                     plotlyOutput(outputId = paste0("plot_", .y)), 
                     ""
                 ),
                 tags$style(HTML(
                   paste0( 
                     "
                        #primary", .y, "_id .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:", measure_color,"
                        }

                        #primary", .y, "_id .box.box-solid.box-primary {
                        border-bottom-color:", measure_color, ";
                        border-left-color:", measure_color, ";
                        border-right-color:", measure_color,";
                        border-top-color:", measure_color, ";
                        }

                        "
                   )
                 )) 
        )
        
        
        
        
      )
      
    })
    
    tagList(plots_list)
    
    
    
  })
  
  
  
  
  # Create reactive object of data set for Multiple Domains graph---------
  multiple_domains_dataset <- reactive({
    
    req(input$patient_id)
    
    # Pull out maximum value of measure  
    measure_value_max <- updated_data_weekly_results() %>% 
      filter(outcome_or_goal == "outcome") %>% 
      pull(ylim_upper)
    
    
    answer_history_plotly %>%
      filter(record_id == input$patient_id) %>%
      filter(!(str_detect(participant_info, "goal"))) %>% 
      mutate(text_choice = paste0(text_choice, " (", values, ")")) %>% 
      select(record_id, participant_info, checkin_start_date, checkin_date_hover, measure_value = values) %>% 
      mutate(measure_value_modified = measure_value / measure_value_max) %>% 
      ungroup() %>% 
      mutate(participant_info = str_replace(participant_info, "_[^_]+$", ""))
    
  })
  
  
  
  
  
  
  # UI for Switches for Multiple Domains Graph---------
  output$switch_ui <- renderUI({
    
    # Pull out participant_info
    participant_info <- multiple_domains_dataset() %>% 
      pull(participant_info) %>% 
      unique()
    
    
    # Pull out measure display name
    measure_display <- answer_history_plotly_instructions() %>% 
      pull(measure_display_name) %>% 
      unique()
    
    
    # Dynamically create materialSwitches
    map2(participant_info,  measure_display,~
           
           materialSwitch(inputId = paste0(.x, "_multiple"), 
                          label = .y,
                          status = "success", 
                          value = FALSE)
    )
    
    
    
    
  })
  
  
  
  # Create domain_input with participant_info_multiple_domain
  domain_input <- reactive({
    
    
    # Pull out participant_info
    participant_info <- multiple_domains_dataset() %>% 
      pull(participant_info) %>% 
      unique()
    
    # Set initial participant_info
    participant_info_multiple_domain <- c("drinking", "coping") 
    
    
    # Loop over participant_info and manipulate participant_info_multiple_domain
    for (ii in participant_info) {
      
      ## If participant_info_multiple_domain already contains drinking and TRUE, do nothing
      if (any(str_detect(participant_info_multiple_domain, ii)) &
          input[[paste0(ii, "_multiple")]] == TRUE) {
        
        participant_info_multiple_domain <- participant_info_multiple_domain
        
        ## If participant_info_multiple_domain already contains drinking and FALSE, take out drinking  
      } else if (any(str_detect(participant_info_multiple_domain, ii)) &
                 input[[paste0(ii, "_multiple")]] == FALSE) {
        
        remove <- ii
        participant_info_multiple_domain <- participant_info_multiple_domain[!(participant_info_multiple_domain %in% remove)]
        
        ## If participant_info_multiple_domain does not contain drinking and TRUE, insert drinking  
      } else if (!any(str_detect(participant_info_multiple_domain, ii)) &
                 input[[paste0(ii, "_multiple")]] == TRUE) {
        
        participant_info_multiple_domain <- c(participant_info_multiple_domain, ii)
        
        ## If participant_info_multiple_domain does not contain drinking and FALSE, do nothing    
      } else {
        
        participant_info_multiple_domain <- participant_info_multiple_domain
        
      }
      
    }
    
    
    participant_info_multiple_domain
    
  })
  
  
  
  
  
  
  
  # UI for Multiple Domains Graphs---------
  output$multiple_domains_graph <- renderPlotly({
    
    
    # Pull out participant_info
    participant_info <- multiple_domains_dataset() %>% 
      pull(participant_info) %>% 
      unique()
    
    
    # Initialize line colors
    line_cols <- vector("character", length = length(participant_info))
    
    for (ii in seq_along(participant_info)) {
      
      # Set color of graph
      measure_color <- answer_history_plotly_instructions() %>% 
        pull(line_color) %>% 
        nth(ii)  
      
      # A named vector to color points and lines
      line_cols[ii] <- measure_color
      names(line_cols)[ii] <- participant_info[ii]
      
    }
    
    
    # Require user to click on switches before displaying plot
    req(domain_input())
    req(input$patient_id)
    
    df <- multiple_domains_dataset()
    
    p <- df %>% 
      filter(participant_info %in% domain_input()) %>% 
      ggplot(aes(checkin_start_date, measure_value_modified,
                 text = paste0(round(measure_value_modified, 3),'\n',
                               checkin_date_hover))) +
      geom_line(aes(color = participant_info, group = participant_info)) +
      geom_point(aes(color = participant_info)) +
      # Set x axis breaks to only the relevant dates
      scale_x_date(breaks = df %>% 
                     filter(record_id == input$patient_id) %>% 
                     pull(checkin_start_date) %>% 
                     unique(),
                   labels = df %>% 
                     filter(record_id == input$patient_id) %>% 
                     pull(checkin_date_hover) %>% 
                     unique()) +
      # Set breaks from lowest to highest
      scale_y_continuous(breaks = c(0, max(df$measure_value_modified)),
                         labels = c(0, 1),
                         limits = c(0, 1)) +
      # Set colors with named vector cols
      scale_colour_manual(values = line_cols) +
      labs(x = "",
           y = "") +
      theme_light() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)
    
    
    
    
  })
  
  
  
  # Start of Goals Graph---------
  
  
  # Manipulate weekly results instructions file for Goals graph---------
  weekly_results_goals_processed <- reactive({
    
    weekly_results_goals <- updated_data_weekly_results() %>% 
      # Convert characters to lowercase
      mutate(yes_no = str_to_lower(yes_no)) %>% 
      # Filter out the “no” rows
      filter(yes_no != "no") %>% 
      # Take out yes_no column
      select(-yes_no) %>% 
      # Filter for outcomes
      filter(outcome_or_goal == "goal") %>% 
      # Take out outcome_or_goal column
      select(-outcome_or_goal)
    
    # Loop through entire excel file and find maximum length inside y_tick_values
    lengths_y_tick_values <- vector("numeric", length = nrow(weekly_results_goals))
    
    for (ii in 1:nrow(weekly_results_goals)) {
      
      lengths_y_tick_values[ii] <- length(str_split(weekly_results_goals[ii,"y_tick_values"],"\\|")[[1]])
      
    }
    # Find max_length
    max_length_y_tick_values <- max(lengths_y_tick_values)
    
    
    
    # Loop through entire excel file and find maximum length inside y_tick_labels
    lengths_y_tick_labels <- vector("numeric", length = nrow(weekly_results_goals))
    
    for (ii in 1:nrow(weekly_results_goals)) {
      
      lengths_y_tick_labels[ii] <- length(str_split(weekly_results_goals[ii,"y_tick_labels"],"\\|")[[1]])
      
    }
    # Found max_length
    max_length_y_tick_labels <- max(lengths_y_tick_labels)
    
    # Separate 
    weekly_results_goals_processed <- weekly_results_goals %>% 
      separate(y_tick_values, into = paste0("y_tick_values_", 1:max_length_y_tick_values)) %>% 
      separate(y_tick_labels, into = paste0("y_tick_labels_", 1:max_length_y_tick_labels), sep = "\\|") 
    
    
    # Final output
    weekly_results_goals_processed
    
  })
  
  
  
  
  # Inner join answer_history_plotly and weekly_results_processed for Goals graphs
  answer_history_plotly_goals_instructions <- reactive({
    
    answer_history_plotly %>% 
      inner_join(weekly_results_goals_processed(), by = c("participant_info" = "measure_name"))
    
  })
  
  
  
  
  # Create reactive object of graphs plotted from map
  graphs_goals <- reactive({
    
    req(input$patient_id)
    
    answer_history_plotly_goals_instructions() %>% 
      filter(record_id == input$patient_id) %>% 
      mutate(text_choice = paste0(values)) %>% 
      group_by(participant_info) %>% 
      nest() %>% 
      mutate(
        graphs = map(data, plotly_weekly_results_goals) 
      ) %>% 
      pull(graphs)
    
  })
  
  
  # UI for Goals Graphs in boxes---------
  output$goals_graphs_ui <- renderUI({
    
    req(input$patient_id)
    
    answer_history_plotly_instructions_data <- answer_history_plotly_goals_instructions() %>% 
      filter(record_id == input$patient_id) %>% 
      mutate(text_choice = paste0(values)) %>% 
      group_by(participant_info) %>% 
      nest() %>% 
      pull(data)
    
    
    iwalk(graphs_goals(), ~{
      output_name <- paste0("goals_plot_", .y)
      output[[output_name]] <- renderPlotly(.x)
    })
    
    
    plots_list <- imap(graphs_goals(), ~{
      
      # Set color of graph
      measure_color <- answer_history_plotly_instructions_data %>% 
        nth(.y) %>% 
        pull(line_color) %>% 
        first()
      
      # Set title of graph
      measure_title <- answer_history_plotly_instructions_data %>% 
        nth(.y) %>% 
        pull(measure_display_name) %>% 
        first()
      
      tagList(
        
        
        tags$div(class = "another-box", id = paste0("danger", .y, "_id"),
                 box(width = 12,
                     title = measure_title, 
                     status = "danger", solidHeader = TRUE,
                     plotlyOutput(outputId = paste0("goals_plot_", .y)), 
                     ""
                 ),
                 tags$style(HTML(
                   paste0( 
                     "
                        #danger", .y, "_id .box.box-solid.box-danger>.box-header {
                        color:#fff;
                        background:", measure_color,"
                        }

                        #danger", .y, "_id .box.box-solid.box-danger {
                        border-bottom-color:", measure_color, ";
                        border-left-color:", measure_color, ";
                        border-right-color:", measure_color,";
                        border-top-color:", measure_color, ";
                        }

                        "
                   )
                 )) 
        )
        
        
        
        
      )
      
    })
    
    tagList(plots_list)
    
    
    
  })
  
  
  
  
  
  
  
  # Start of Answer History Table----------------------------
  
  # Get column names for answer history table---------------------------
  answer_history_table_columns <- reactive({
    
    req(input$patient_id)
    
    answer_history_table %>% 
      filter(record_id == input$patient_id) %>%
      select(checkin_start_time) %>% 
      mutate(checkin_start_time = as.Date(checkin_start_time)) %>% 
      mutate(checkin_start_time = as.character(checkin_start_time)) %>% 
      pull() %>% 
      unique()
    
  })
  
  
  # Get last completed date for answer history table---------------------------
  answer_history_table_last_completed_date <- reactive({
    
    req(input$patient_id)
    
    answer_history_table %>% 
      filter(record_id == input$patient_id) %>%
      select(checkin_start_time) %>% 
      mutate(checkin_start_time = as.Date(checkin_start_time)) %>% 
      pull(checkin_start_time) %>% 
      last()
    
  })
  
  
  # Link to answer_history_modified.xlsx updating every 1 second---------------------------
  updated_data_answer_history <- reactiveFileReader(
    intervalMillis = 1000000,
    session = session,
    filePath = "XXX", # Your Google Sheets here
    readFunc = googlesheets4::read_sheet
  )
  
  
  
  
  # Objects to customize answer_history table---------------------------
  header_text_substance_use_craving <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(1)
    
    
  })
  
  
  header_text_coping_strategy <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(2)
    
    
  })
  
  
  header_text_confidence <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(3)
    
    
  })
  
  
  header_text_positive_outlook <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(4)
    
    
  })
  
  
  header_text_depression <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(5)
    
    
  })
  
  
  header_text_anxiety <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(6)
    
    
  })
  
  
  header_text_goals <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text) %>% 
      nth(7)
    
    
  })
  
  
  
  header_background_color_substance_use_craving <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(1)
    
    
  })
  
  
  header_background_color_coping_strategy <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(2)
    
    
  })
  
  
  header_background_color_confidence <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(3)
    
    
  })
  
  
  header_background_color_positive_outlook <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(4)
    
    
  })
  
  
  header_background_color_depression <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(5)
    
    
  })
  
  
  header_background_color_anxiety <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(6)
    
    
  })
  
  
  header_background_color_goals <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_background_color) %>% 
      nth(7)
    
    
  })
  
  
  
  
  header_text_color_substance_use_craving <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(1)
    
    
  })
  
  
  header_text_color_coping_strategy <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(2)
    
    
  })
  
  
  header_text_color_confidence <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(3)
    
    
  })
  
  
  header_text_color_positive_outlook <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(4)
    
    
  })
  
  
  header_text_color_depression <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(5)
    
    
  })
  
  
  header_text_color_anxiety <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(6)
    
    
  })
  
  
  header_text_color_goals <- reactive({
    
    updated_data_answer_history() %>% 
      pull(header_text_color) %>% 
      nth(7)
    
    
  })
  
  
  
  ## Answer History Table-----------------
  output$answer_history_table <- function() {
    
    req(input$patient_id)
    
    answer_history_table %>%   
      # # Filter for one patient
      filter(record_id == input$patient_id) %>% 
      pivot_wider(id_cols = field_label,
                  names_from = c(record_id, checkin_start_time), 
                  values_from = text_choice) %>% 
      # Take out NA rows
      filter(!is.na(field_label)) %>% 
      # Add html code to put Checkin Time on different line as title
      kbl(caption = paste0("<strong>Answer History Table
                <br>Last completed questionnaire on ",
                           answer_history_table_last_completed_date(), "</strong>"),
          escape = FALSE,
          format = "html",
          col.names = c("", answer_history_table_columns())) %>% 
      kable_paper("striped", full_width = T) %>%
      pack_rows(paste0(header_text_substance_use_craving(),"\nIn the past 7 days..."), 1, 3,
                label_row_css = paste0("background-color: ",header_background_color_substance_use_craving(), "; color: ",header_text_color_substance_use_craving())) %>%
      pack_rows(paste0(header_text_coping_strategy(),"\nIn the past 7 days..."), 4, 6, 
                label_row_css = paste0("background-color: ",header_background_color_coping_strategy(), "; color: ",header_text_color_coping_strategy())) %>% 
      pack_rows(paste0(header_text_confidence(),"\nIn the past 7 days..."), 7, 8, 
                label_row_css = paste0("background-color: ", header_background_color_confidence(), "; color: ",header_text_color_confidence())) %>% 
      pack_rows(paste0(header_text_positive_outlook(),"\nIn the past 7 days..."), 9, 10, 
                label_row_css = paste0("background-color: ",header_background_color_positive_outlook(), "; color: ",header_text_color_positive_outlook())) %>% 
      pack_rows(paste0(header_text_depression(),"\nIn the past 7 days..."), 11, 12, 
                label_row_css = paste0("background-color: ",header_background_color_depression(), "; color: ",header_text_color_depression())) %>% 
      pack_rows(paste0(header_text_anxiety(),"\nIn the past 7 days..."), 13, 14, 
                label_row_css = paste0("background-color: ",header_background_color_anxiety(), "; color: ",header_text_color_anxiety())) %>% 
      pack_rows(paste0(header_text_goals(),"\nIn the coming week..."), 15, 19, 
                label_row_css = paste0("background-color: ",header_background_color_goals(), "; color: ",header_text_color_goals())) 
    
    
    
    
  }
  
  
  
  
}


shinyApp(ui, server)
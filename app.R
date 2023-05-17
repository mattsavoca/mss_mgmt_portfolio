# app.R
options(scipen = 999)

# Load helper functions
source("linked_helpers.R")

# Load required libraries ----
#install.packages(shiny)
#library(shiny)
library(shinydashboard)
library(DT)
library(nflplotR)
library(nflreadr)
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(ffsimulator)
library(data.table)
library(noncompliance)
library(ffscrapr)
library(markdown)
library(ggridges)
library(future)
library(usemodels)
library(tidymodels)
library(finetune)
library(tidytext)
library(doParallel)
library(tictoc)
library(plotly)
library(htmlwidgets)
library(janitor)
library(ggrepel)
library(gt)
library(shinycssloaders)
library(profvis)
# Load required data ------
player_name_key <- read_csv("./data/player_names.csv")
team_name_key <- read_csv("./data/team_names.csv")
ud_scoring_history = read_csv("./data/ud_hppr_scoring_history.csv")
fp_latest_rankings = read_csv("./data/fp_latest_rankings.csv")






# Define UI -----
ui <- dashboardPage(
  dashboardHeader(title = "ResQ"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulator", tabName = "simulator", icon = icon("play")),
      menuItem("Instructions", tabName = "instructions", icon = icon("info"))#,
      #menuItem("Live Draft", tabName = "live_draft", icon = icon("list-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "simulator",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Simulator Settings",
                    wellPanel(style = "background-color: #f7f7f7;",
                              h1("ResQ"),
                              h4("Recursive Evaluation of Seasonal Quantiles"),
                              tags$p("click Instructions for more details and on how to use this tool"),
                              fileInput("file_upload", "Underdog Rankings CSV", accept = c(".csv"), width = "100%"),
                              sliderInput("RANK_CUTOFF", "Players to Rank:", 225, min = 50, max = 300, step = 25, width = "100%"),
                              sliderInput("N_SEASONS", "Number of Seasons to Simulate:", 25, min = 5, max = 250, step = 5, width = "100%"),
                              wellPanel(
                                h4("Draft Information"),
                                sliderInput("N_TEAMS", "Drafters:", 12, min = 3, max = 12, step = 1, width = "100%"),
                                fluidRow(
                                  column(width = 6, sliderInput("N_QB", "Min. QB Starters", 1, min = 1, max = 2, step = 1, width = "100%")), 
                                  column(width = 6, sliderInput("N_TE", "Min. TE Starters", 1, min = 0, max = 2, step = 1, width = "100"))
                                  ),
                                fluidRow(
                                  column(width= 6 ,sliderInput("N_RB", "Min. RB Starters:", 2, min = 1, max = 5, step = 1, width = "100%")),
                                  column(width= 6 ,sliderInput("N_WR", "Min. WR Starters:", 3, min = 1, max = 6, step = 1, width = "100%")),
                                  #column(width= 6 ,sliderInput("QB_BASELINE", "QB Baseline:", 12, min = 3, max = 24, step = 1, width = "100%")),
                                  #column(width = 6, sliderInput("FLEX_BASELINE", "Flex Baseline:", 84, min = 12, max = 96, step = 3, width = "100%"))
                                ),
                                sliderInput("N_FLEX", "Extra FLEX Starters:", 1, min = 1, max = 4, step = 1, width = "100%")
                                ),
                              sliderInput("TE_BOOST", "TE Boost:", 0.5, min = 0, max = 1, step = 0.25, width = "100%"),
                              sliderInput("SPIKE_WEIGHT", "Spike Weeks vs. Usable Weeks Ratio", 0.5, min = 0, max = 1, step = 0.25, width = "100%"),
                              sliderInput("SIM_WEIGHT", "Sim Weight:", 0.5, min = 0, max = 1, step = 0.25, width = "100%"),
                              selectizeInput(inputId = "TEAM_BOOSTS", choices = sort(team_name_key$fp_team), "Team Boosts:", multiple = T, width = "100%"),
                              actionButton("submit", "Start Simulations", width = "100%"),
                              div(style = "margin-bottom: 20px;")
                    )
                ),
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Simulator Results",
                    conditionalPanel(
                      condition = "output.download_ready",
                      downloadButton("download_results", "Download Results!"),
                      actionButton("rerun_simulations", "Re-Run Simulations")
                    ),
                    conditionalPanel(
                      condition = "!output.download_ready",
                      tags$h4("Upload Projections and Click Start Simulations Above")
                    ),
                    conditionalPanel(
                      condition = "output.download_ready",
                      DT::DTOutput("results_table"),
                      box(width = 12, solidHeader = TRUE, status = "primary", title = "Interactive Line Chart",plotlyOutput("interactive_chart")),
                      box(
                        fluidRow(
                          column(6, plotlyOutput("spike_weeks_bar_chart")),
                          column(6, plotlyOutput("usable_weeks_bar_chart"))
                        )
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "instructions",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "info", title = "Instructions",
                    includeMarkdown("instructions.md")
                ),
                box(width = 12, solidHeader = TRUE, status = "info", title = "Credits",
                    p("Made by Matt Savoca @draftaholic and made possible by NFLVerse, FFScrapr, and FFSimulator")
                )
              )
      )
    )
  )
)

# Define server logic -----
server <- function(input, output, session) {
  QB_BASELINE = reactive(
    input$N_QB * input$N_TEAMS
  )
    
  FLEX_BASELINE = reactive(
    (input$N_RB + input$N_WR + input$N_TE + input$N_FLEX) * input$N_TEAMS
  )
  # Define functions ----
  create_draft_uiOutputs <- function(n_outputs) {
    lapply(1:n_outputs, function(n) {
      uiOutput(paste0("team_selectize_", n))
    })
  }
  
  generate_simulation_table = function(df){
    df %>%
      mutate(Player = paste0(first_name," ",last_name)) %>%
      select(
        `My Rank` = my_rank,
        Player,
        `Position` = slot_name,
        `Team` = team_name,
        `ADP Rank` = adp_rank,
        `My Pos. Rank` = sim_pos_rank,
        `UD ADP` = ovr_adp,
        `Sim FP/G` = sim_ppg,
        `UD FP/G` = ud_ppg,
        `Usable Weeks` = usable_weeks,
        `Spike Weeks`= spike_weeks,
        `Sim Lo FP/G` = sim_lo,
        `Sim Hi FP/G` = sim_hi,
        `FP/G Diff` = sim_ppg_diff,
        `Rank Diff` = sim_rank_diff,
        `Pos. Rank Diff` = pos_rank_diff,
        id
      )
  }
  
  
  
  # Future Variables -----
  WEEKS <- 1:16
  
  
  # Run Simulations -----
  
  # Process uploaded CSV and generate sim_results_df
  sim_results_df <- reactiveVal()
  #fp_latest_rankings <- ffsimulator::ffs_latest_rankings()
  
  run_simulations <- function() {
    req(input$file_upload)
    
    # Check if the file is a CSV and has size under 5MB
    if (input$file_upload$type != "text/csv" || input$file_upload$size > 5*1024*1024) {
      showModal(modalDialog(
        title = "Error",
        "Invalid file. Please upload a CSV file under 5MB.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Calculate sim_results_df inside a reactive expression
    withProgress(message = "Simulating Results", value = 0, {
      print(QB_BASELINE())
      print(FLEX_BASELINE())
      # Read and process the uploaded CSV
      ud_adp_raw <- read.csv(input$file_upload$datapath)
      incProgress(1/10)
      ud_adp <- janitor::clean_names(ud_adp_raw)
      incProgress(2/10)
      latest_ranks <- convert_ud_adp_to_ffsimulator(
        ud_adp, 
        team_names_key = team_name_key, 
        player_names_key = player_name_key, 
        fp_latest_rankings = fp_latest_rankings)
      
      sim_results <- isolate({
        ffsimulator::ffs_generate_projections(
          adp_outcomes = ffsimulator::ffs_adp_outcomes(
            scoring_history = ud_scoring_history, 
            gp_model = "simple"), 
          latest_rankings = latest_ranks, 
          n_seasons = input$N_SEASONS,
          weeks = WEEKS)  %>% 
          generate_projection_summary(
            ranks = latest_ranks,
            qb_baseline = QB_BASELINE(),
            flex_baseline = FLEX_BASELINE(), 
            spike_weight = input$SPIKE_WEIGHT,
            sim_weight = input$SIM_WEIGHT,
            team_boosts = input$TEAM_BOOSTS,
          ) %>%
          prepare_sims_for_ud_export(
            rank_cutoff = input$RANK_CUTOFF,
            ud_adp = ud_adp_raw %>% clean_names()
          )
      })
      
      incProgress(5/10)
      
      # Set the reactive value
      sim_results_df(sim_results)
      incProgress(2/10)
      
      # Show "Simulation Complete!" message when the sim_results_df is ready
      if (!is.null(sim_results_df())) {
        showNotification("Simulation Complete!", type = "message", duration = 1.5)
      }
      incProgress(1/10)
    })
  }
  
  
  observeEvent(input$submit, {
    run_simulations()
  })
  
  
  # Re-run simulations when the new button is clicked
  observeEvent(input$rerun_simulations, {
    run_simulations()
  })
  
  observeEvent(input$plotly_click, {
    # Access the click event data
    click_data <- input$plotly_click
    
    # Do something with the click data (e.g., print to the console)
    print(click_data)
  })
  
  
  # Create a reactive expression for the simulation table data
  sim_table_data <- reactive({
    req(sim_results_df())
    sim_results_df() %>% generate_simulation_table()
  })
  
  output$results_table <- renderDT({
    datatable(sim_table_data() %>% mutate_if(is.numeric, round, 1), 
              filter = list(position = "top"),
              selection = "single", 
              extensions = c("FixedHeader"),
              options = list(
                pageLength = 12, 
                scrollX = TRUE, 
                columnDefs = list(list(visible=FALSE, targets= 16)),
                fixedHeader = TRUE), 
              rownames = FALSE)
  })
  
  selected_data <- reactiveVal(NULL)
  
  observeEvent(input$results_table_rows_selected, {
    # Get the selected row index
    selected_row <- input$results_table_rows_selected
    
    if (length(selected_row) > 0) {
      # Extract the necessary data from the sim_table_data data frame
      clicked_data <- sim_results_df()[selected_row,]
      selected_data(clicked_data)
      cat("Selected row:", selected_row, "\n")
      cat("Clicked data:\n")
      print(clicked_data)
    } else {
      selected_data(NULL)
    }
  })

  # Sim Summary Data Visualization ----
  #Interactive Chart
  output$interactive_chart <- renderPlotly({
    req(selected_data())
    
    # Get the clicked data
    clicked_data <- selected_data() %>% 
      mutate(pick_num = my_rank, team_num = 12) %>%
      generate_draft_rounds() %>%
      select(-pick_num, -team_num)
    
    if (nrow(clicked_data) == 0) {
      return(NULL)
    }
    
    # Calculate the average lo_range and hi_range for rows where 'pos' is equal to 'pos' in the clicked row
    avg_data <- sim_results_df() %>%
      filter(slot_name == clicked_data$slot_name) %>%
      summarise(avg_lo_range = mean(sim_lo), avg_mid_range = mean(sim_ppg) ,avg_hi_range = mean(sim_hi), avg_spikes = mean(spike_weeks))
    
    # Create the interactive chart
    plot_ly() %>%
      add_trace(x = ~c(clicked_data$sim_lo, clicked_data$sim_hi), y = ~factor(c("Selected Player", "Selected Player")), type = "scatter", mode = "lines+markers", name = paste0(clicked_data$first_name, " ", clicked_data$last_name)) %>%
      add_trace(x = ~c(avg_data$avg_lo_range, avg_data$avg_hi_range), y = ~factor(c("Average", "Average")), type = "scatter", mode = "lines+markers", name = "Pos. Average") %>%
      add_trace(x = ~clicked_data$sim_ppg, y = ~factor("Selected Player"), type = "scatter", mode = "markers", name = paste0(clicked_data$first_name, " ", clicked_data$last_name, " Sim PPG"), showlegend = FALSE, marker = list(color = "red")) %>%
      add_trace(x = ~avg_data$avg_mid_range, y = ~factor("Average"), type = "scatter", mode = "markers", name = "Average Sim PPG", showlegend = FALSE, marker = list(color = "red")) %>%
      layout(yaxis = list(title = "", showticklabels = FALSE), xaxis = list(title = "FP/Gm"))
  })
  
  output$spike_weeks_bar_chart <- renderPlotly({
    req(selected_data())
    
    # Get the clicked data
    clicked_data <- selected_data()
    
    # Calculate the average spike_weeks for rows where 'pos' is equal to 'pos' in the clicked row
    avg_spike_weeks <- sim_results_df() %>%
      filter(slot_name == clicked_data$slot_name) %>%
      summarise(avg_spike_weeks = mean(spike_weeks)) %>%
      pull(avg_spike_weeks)
    
    # Calculate the difference between the selected player's spike_weeks and the average
    diff_spike_weeks <- clicked_data$spike_weeks - avg_spike_weeks
    
    # Set the bar color
    bar_color <- ifelse(diff_spike_weeks > 0, "green", "red")
    
    # Create the horizontal bar chart
    plot_ly() %>%
      add_trace(y = ~paste0(clicked_data$first_name, " ", clicked_data$last_name),
                x = ~diff_spike_weeks,
                type = "bar", orientation = "h", name = "Spike Weeks",
                marker = list(color = bar_color)) %>%
      layout(yaxis = list(title = "", type = "category"), xaxis = list(title = "Spike Weeks - Avg Spike Weeks"))
  })
  output$usable_weeks_bar_chart <- renderPlotly({
    req(selected_data())
    
    # Get the clicked data
    clicked_data <- selected_data()
    
    # Calculate the average usable_weeks for rows where 'pos' is equal to 'pos' in the clicked row
    avg_usable_weeks <- sim_results_df() %>%
      filter(slot_name == clicked_data$slot_name) %>%
      summarise(avg_usable_weeks = mean(usable_weeks)) %>%
      pull(avg_usable_weeks)
    
    # Calculate the difference between the selected player's usable_weeks and the average
    diff_usable_weeks <- clicked_data$usable_weeks - avg_usable_weeks
    
    # Set the bar color
    bar_color <- ifelse(diff_usable_weeks > 0, "green", "red")
    
    # Create the horizontal bar chart
    plot_ly() %>%
      add_trace(y = ~paste0(clicked_data$first_name, " ", clicked_data$last_name),
                x = ~diff_usable_weeks,
                type = "bar", orientation = "h", name = "Usable Weeks",
                marker = list(color = bar_color)) %>%
      layout(yaxis = list(title = "", type = "category"), xaxis = list(title = "Usable Weeks - Avg Usable Weeks"))
  })
  # Create an output to indicate if the download button should be shown
  output$download_ready <- reactive(!is.null(sim_results_df()))
  outputOptions(output, "download_ready", suspendWhenHidden = FALSE)
  
  # Enable the download button only when sim_results_df is ready
  output$download_results <- downloadHandler(
    filename = function() {
      "sim_results.csv"
    },
    content = function(file) {
      req(sim_results_df())
      write.csv(sim_results_df(), file, row.names = FALSE)
    },
    #enabled = reactive(!is.null(sim_results_df()))
  )
}

# Run the app ----
app = shinyApp(ui = ui, server = server)

runApp(app)

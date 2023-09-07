library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(rsconnect) 
library(rmarkdown)
library(lubridate)
library(shinythemes)
library(reprex)





grosses_clean <- readRDS("grosses_clean.rds")
tot_show <- readRDS("tot_show.rds")
theatres <- readRDS("theatres.rds")
week_total <- readRDS("week_total.rds")
week_gross_show <- readRDS("week_gross_show.rds")


#  The UI

ui <- fluidPage(
  theme = shinytheme("united"),  
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "BGA.css")), 
  
  tags$div(
    titlePanel("Broadway Grosses Analysis"),
    style = "font-family: 'Times New Roman', Times, serif; text-align:center; font-weight: 1000; color: #0C356A;"
  ),
  
  tags$div(
    style = "margin: 0 auto; width: 80%; text-align: center; padding: 20px; color: #1450A3;", 
    "This application dives deep into the financial aspects of Broadway, offering a panoramic view of the theater industry's commercial successes and trends. Specifically designed for enthusiasts, investors, theater professionals, and scholars, it elucidates the dynamic monetary landscape of the theater world."
  ),
  
  br(),


  #The Annually Analysis section
  tabsetPanel(
    id = 'main_tabs',
    tabPanel("Annually Analysis",
             tags$div(style = "height: 10px;"),
             tags$div(
               style = "margin: 0 auto; width: 80%; text-align: center; padding: 10px;color: #4477CE;", 
               " Choose features and multiple years to gain comparative trend insights."
             ),
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("year", "Select Years", choices = sort(unique(lubridate::year(week_total$Week_Ending)), decreasing = TRUE), multiple = TRUE, options = list('plugins' = list('remove_button'))),
                 br(),
                 selectInput("feature_selector", "Select Feature to Display", choices = c("Average Ticket Price" = "Weekly_Average_Price", "Weekly Grosses" = "Weekly_Gross_Overall", "Total Seats Sold" = "Weekly_Seats_Sold")),
                 br(),
                 actionButton("plot_button", "Plot")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.main_tabs == 'Annually Analysis'",
                   plotlyOutput("line_plot", width = "100%", height="500px")
                 )
               )
             )
    ),

 #The Weekly Grosses section   
    tabPanel("Weekly Grosses",
             tags$div(style = "height: 10px;"),
             tags$div(
               style = "margin: 0 auto; width: 80%; text-align: center; padding: 10px;color: #4477CE;", 
               "Dive deep into specific weeks within a selected year to analyze the weekly details for Broadway."
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("year_analysis", "Select Year", choices = sort(unique(year(week_gross_show$Week_Ending)), decreasing = TRUE)),
                 selectInput("week_analysis", "Select Week", choices = NULL),
                 br(),
                 actionButton("show_button", "Show Data")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.main_tabs == 'Weekly Grosses'",
                   dataTableOutput("table_output")
                 )
               )
             )
    ),

 #The Shows section
    tabPanel("Shows",
             tags$div(style = "height: 10px;"),
             tags$div(
               style = "margin: 0 auto; width: 80%; text-align: center; padding: 10px;color: #4477CE;", 
               "Discover the core of Broadway through individual shows. This section ranks them based on their all-time gross revenues, ticket prices, and seats sold. Dive into the details by clicking on a show's name."
             ),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("rank_range", "Select Rank Range", min = 1, max = nrow(tot_show), value = c(1, 50)),
                 br(),
                 actionButton("top_show_button", "Show Top Shows")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.main_tabs == 'Shows'",
                   DTOutput("top_show_table")  # Use DTOutput
                 )
               )
             )
    ),
 
 
 #The Theatres section
    tabPanel("Theatres",
             tags$div(style = "height: 10px;"),
             tags$div(
               style = "margin: 0 auto; width: 80%; text-align: center; padding: 10px;color: #4477CE;", 
               "Learn more about the venues that bring Broadway to life. Rank theatres based on size, capacity percentage, and other criteria to understand their importance in the landscape."
             ),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("theatre_rank_range", "Select Rank Range", min = 1, max = nrow(theatres), value = c(1, 50)),
                 br(),
                 actionButton("show_theatre_button", "Show Top Theatres")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.main_tabs == 'Theatres'",
                   DTOutput("theatre_data_table")  # Use DTOutput for the table
                 )
               )
             )
    ),
    
 
 #The Top Shows & Theatres section    
    tabPanel("Top Shows & Theatres",
             tags$div(style = "height: 10px;"),
             tags$div(
               style = "margin: 0 auto; width: 80%; text-align: center; padding: 10px;color: #4477CE;", 
               "Visualize top-performing shows and theatres in an interactive scatter plot. Customize the metrics and explore the data through a dynamic range slider to gain tailored insights."
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("data_type", "Select Type", choices = c("Shows" = "shows", "Theatres" = "theatres")),
                 uiOutput("y_axis_selector"),
                 uiOutput("x_axis_selector"),
                 sliderInput("rank_range_scatter", "Select Rank Range", min = 0, max = 50, value = c(0, 50)),
                 br(),
                 actionButton("plot_top100_button", "Plot")
               ),
               mainPanel(
                 plotlyOutput("top100_scatter", width = "90%", height="450px")
               )
             )
    ),
    
    
    
    
    
 #The About section    
 tabPanel("About",
          tags$div(
            tags$h3("About This App"),
            tags$p("Created by: Zhang Zhang"),
            tags$p("Source Data: ", tags$a("Link to Data Source", href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-04-28")),
            tags$p("Purpose: 'Broadway Grosses Analysis' aims to bridge the gap between Broadway's expansive data resources and its diverse audience. For enthusiasts, we present a curated blend of historical and analytical data, bringing to life the storied past and dynamic present of Broadway. Professionals will find it indispensable for data-driven decision-making, allowing them to grasp market trends and formulate effective strategies. For students and researchers, it becomes a comprehensive academic resource. Furthermore, by emphasizing data communication, we ensure that clients across various sectors can seamlessly interact with and utilize the information. At its core, 'Broadway Insights' isn't just an app – it's a conduit for understanding, valuing, and harnessing the multifaceted world of Broadway."),
            tags$p("Session Information: "),
            verbatimTextOutput("session_info_display")
          )
 )
  )
)





# Define the server 
server <- function(input, output, session) {
  
  # For "Annually Analysis" section
  filtered_data1 <- reactive({
    selected_years <- input$year
    week_data <- week_total %>% 
      filter(lubridate::year(Week_Ending) %in% selected_years) %>%
      mutate(week_num = as.integer(format(Week_Ending, "%U")) + 1)
    return(week_data)
  })
  
  output$line_plot <- renderPlotly({
    if (input$plot_button > 0) {
      selected_feature <- input$feature_selector
      feature_name <- switch(selected_feature,
                             "Weekly_Average_Price" = "Average Ticket Price",
                             "Weekly_Gross_Overall" = "Weekly Grosses",
                             "Weekly_Seats_Sold" = "Total Seats Sold")
      
      data_to_plot <- filtered_data1()
      
      # Create hover_text based on data
      hover_text <- paste("Week:", data_to_plot$week_num, 
                          "<br>", feature_name, ":", data_to_plot[[selected_feature]],
                          "<br> Year:", lubridate::year(data_to_plot$Week_Ending))
      
      p <- plot_ly(data_to_plot, 
                   x = ~week_num, 
                   y = ~get(selected_feature), 
                   color = ~as.factor(lubridate::year(Week_Ending)), 
                   type = "scatter", 
                   mode = "lines",
                   text = ~hover_text,
                   hoverinfo = "text") %>% 
        layout(title = paste(feature_name, "for Selected Years"),
               xaxis = list(title = "Week of the Year"),
               yaxis = list(title = feature_name),
               showlegend = TRUE)
      
      return(p)
    }
  })
  
  
  
  # For "Weekly Grosses Analysis" section
  observe({
    selected_year <- input$year_analysis
    week_choices <- week_gross_show %>%
      filter(year(Week_Ending) == selected_year) %>%
      pull(Week_Ending) %>%
      unique() %>%
      sort(decreasing = TRUE)
    updateSelectInput(session, "week_analysis", choices = week_choices)
  })
  
  filtered_week_data <- reactive({
    selected_year <- input$year_analysis
    selected_week <- input$week_analysis
    week_gross_show %>%
      filter(year(Week_Ending) == selected_year & Week_Ending == selected_week)
  })
  
  output$table_output <- renderDataTable({
    if (input$show_button > 0) {
      filtered_week_data()
    }
  })
  
  #  # For "Shows" section
  output$top_show_table <- renderDT({  # Use renderDT
    if (input$top_show_button > 0) {
      rank_range <- input$rank_range
      tot_show_subset <- tot_show[rank_range[1]:rank_range[2], c("Show", "Historical_Total_Gross", "Avg_Ticket_Price", "Total_Seats_Sold","Rank")]
      tot_show_subset
    }
  })
  
  # Show synopsis pop-up modal
  observeEvent(input$top_show_table_cell_clicked, {
    info <- input$top_show_table_cell_clicked
    if (!is.null(info$value) && info$col == 1) {
      selected_show <- info$value
      synopsis_text <- tot_show[tot_show$Show == selected_show, "Synopsis"]
      showModal(
        modalDialog(
          title = selected_show,
          synopsis_text,
          footer = modalButton("Close")
        )
      )
    }
  })
  
  
  
  # For "About" section
  # For "About" section
  output$session_info_display <- renderPrint({
    session_info_text <- capture.output(sessionInfo())
    cat(session_info_text, sep = "\n")
  })
  
  
  
  # For "Theatres" section
  output$theatre_data_table <- renderDT({  # Use renderDT
    if (input$show_theatre_button > 0) {
      theatre_rank_range <- input$theatre_rank_range
      theatres_subset <- theatres[theatre_rank_range[1]:theatre_rank_range[2], ]
      theatres_subset
    }
  })
  
  # For "Top Shows & Theatres" section
  output$y_axis_selector <- renderUI({
    if (input$data_type == "shows") {
      selectInput("y_axis", "Select Y Axis", choices = c("Historical Total Gross" = "Historical_Total_Gross", "Average Ticket Price" = "Avg_Ticket_Price", "Total Seats Sold" = "Total_Seats_Sold"))
    } else {
      selectInput("y_axis", "Select Y Axis", choices = c("Total Grosses" = "Total_Grosses", "Average Capacity" = "Average_Pct_Capacity", "Seats in Theatre" = "Seats_In_Theatre"))
    }
  })
  
  output$x_axis_selector <- renderUI({
    if (input$data_type == "shows") {
      selectInput("x_axis", "Select X Axis", choices = c("Historical Total Gross" = "Historical_Total_Gross", "Average Ticket Price" = "Avg_Ticket_Price", "Total Seats Sold" = "Total_Seats_Sold", "Years of Run" = "Year_Of_Run"))
    } else {
      selectInput("x_axis", "Select X Axis", choices = c("Total Grosses" = "Total_Grosses", "Average Capacity" = "Average_Pct_Capacity", "Seats in Theatre" = "Seats_In_Theatre"))
    }
  })
  
  scatter_data <- reactive({
    if (input$data_type == "shows") {
      tot_show %>% arrange(desc(Historical_Total_Gross)) %>% head(100)
    } else {
      theatres %>% arrange(desc(Total_Grosses)) %>% head(100)
    }
  })
  
  filtered_data <- reactive({
    data = scatter_data()
    rank_filtered_data = data[input$rank_range_scatter[1]:input$rank_range_scatter[2], ]
    return(rank_filtered_data)
  })
  
  output$top100_scatter <- renderPlotly({
    if (input$plot_top100_button > 0) {
      data <- filtered_data()
      
      if (input$data_type == "shows") {
        data$color_factor <- as.factor(data$Show)
        hover_text <- paste("Show:", data$Show, 
                            "<br>", input$x_axis, ":", data[[input$x_axis]],
                            "<br>", input$y_axis, ":", data[[input$y_axis]])
      } else {
        data$color_factor <- as.factor(data$Theatre)
        hover_text <- paste("Theatre:", data$Theatre, 
                            "<br>", input$x_axis, ":", data[[input$x_axis]],
                            "<br>", input$y_axis, ":", data[[input$y_axis]])
      }
      
      plot_ly(
        data = data, 
        x = ~get(input$x_axis), 
        y = ~get(input$y_axis), 
        color = ~color_factor,  # Use the factor for coloring
        type = "scatter",
        mode = "markers",
        text = ~hover_text,  
        hoverinfo = "text",  
        marker = list(size = 7)
      ) %>%
        layout(
          title = " ",
          xaxis = list(title = input$x_axis),
          yaxis = list(title = input$y_axis),
          margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
          showlegend = FALSE  
        )
    }
  })
  
}



# Run the app
shinyApp(ui = ui, server = server)
# Load necessary libraries
library(shiny)
library(tidyverse)
library(plotly)
library(rentrez)

# Define Shiny app UI
ui <- fluidPage(
  titlePanel("Number of Articles by Term in PubMed by Year"),
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the terms you'd like to search for in PubMed, separated by commas."),
      textInput("terms", "Search Terms", ""),
      numericInput("start_year", "Start Year", 2000, min = 1900, max = 2100, step = 1),
      numericInput("end_year", "End Year", 2020, min = 1900, max = 2100, step = 1),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotlyOutput("articlePlot")
    )
  )
)

# Define Shiny app server
server <- function(input, output) {
  article_counts <- eventReactive(input$submit, {
    req(input$terms, input$start_year, input$end_year)
    
    terms <- strsplit(input$terms, ",")[[1]]
    terms <- str_trim(terms)
    
    years <- input$start_year:input$end_year
    
    counts <- map(terms, function(term) {
      map_int(years, function(year) {
        es <- entrez_search(db = "pubmed", term = paste0(term, " AND ", year, "[PDAT]"), retmax = 0)
        es$count
      })
    })
    
    tibble(term = rep(terms, each = length(years)), year = rep(years, times = length(terms)), count = unlist(counts))
  })
  
  output$articlePlot <- renderPlotly({
    req(article_counts())
    
    plot <- article_counts() %>%
      ggplot(aes(x = year, y = count, fill = term)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Number of Articles by Term in PubMed by Year",
           x = "Year",
           y = "Number of Articles") +
      theme_minimal()
    ggplotly(plot)
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)

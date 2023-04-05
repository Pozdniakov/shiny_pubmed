# Load necessary libraries
library(shiny)
library(tidyverse)
library(plotly)
library(rentrez)

# Define Shiny app UI
ui <- fluidPage(
  titlePanel("Number of Articles by Term in PubMed"),
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the terms you'd like to search for in PubMed, separated by commas."),
      textInput("terms", "Search Terms", ""),
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
    req(input$terms)
    
    terms <- strsplit(input$terms, ",")[[1]]
    terms <- str_trim(terms)
    
    counts <- map_int(terms, function(term) {
      es <- entrez_search(db = "pubmed", term = term, retmax = 0)
      es$count
    })
    
    tibble(term = terms, count = counts)
  })
  
  output$articlePlot <- renderPlotly({
    req(article_counts())
    
    plot <- article_counts() %>%
      ggplot(aes(x = term, y = count, fill = term)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Articles by Term in PubMed",
           x = "Term",
           y = "Number of Articles") +
      theme_minimal()
    ggplotly(plot)
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)

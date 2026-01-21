library(shiny)
library(tidyverse)
library(bslib)

rev_data <- read_csv("DataNetflixRevenue2020_V2.csv")
sub_data <- read_csv("DataNetflixSubscriber2020_V2.csv")

netflix <- inner_join(rev_data, sub_data, by = c("Area", "Years")) %>% #laczenie dwoch tabel danych
  mutate(Years = factor(Years, levels = c(
    "Q1 - 2018", "Q2 - 2018", "Q3 - 2018", "Q4 - 2018",
    "Q1 - 2019", "Q2 - 2019", "Q3 - 2019", "Q4 - 2019",  #posortowanie dancyh
    "Q1 - 2020", "Q2 - 2020"
  )))


ui <- fluidPage(
  theme = bs_theme(bootswatch = "yeti"), 
  titlePanel("Analiza wynikow Netflix (2018-2020)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Wybierz region:", choices = unique(netflix$Area)),
      radioButtons("metric", "Wybierz miarę do analizy:", 
                   choices = c("przychody" = "Revenue", 
                               "Subskrybenci" = "Subscribers")),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres liniowy", br(), plotOutput("trendPlot")),
        tabPanel("Porównanie regionów", br(), plotOutput("barPlot")),
        tabPanel("Tabela danych", br(), tableOutput("table"))
      )
    )
  )
)



server <- function(input, output) {
  
  #wykres liniowy
  output$trendPlot <- renderPlot({
    netflix %>%
      filter(Area == input$region) %>%
      ggplot(aes(x = Years, y = .data[[input$metric]], group = Area)) +
      geom_line(color = "#E50914", linewidth = 1.2) + 
      geom_point(size = 4) +
      theme_minimal() +
      labs(title = paste(input$metric, "- Trend czasowy dla:", input$region),
           x = "Kwartał", 
           y = input$metric) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #regiony
  output$barPlot <- renderPlot({
    netflix %>%
      filter(Years == "Q2 - 2020") %>%
      ggplot(aes(x = reorder(Area, .data[[input$metric]]), y = .data[[input$metric]], fill = Area)) +
      geom_col() +
      coord_flip() + 
      theme_minimal() +
      guides(fill = "none") +
      labs(title = paste("Porównanie regionów: ", input$metric, "(Q2 - 2020)"), 
           x = "Region", 
           y = input$metric)
  })
  
  # Tabela 
  output$table <- renderTable({
    netflix %>% 
      filter(Area == input$region) %>%
      mutate(Years = as.character(Years)) 
  })
}

shinyApp(ui, server)
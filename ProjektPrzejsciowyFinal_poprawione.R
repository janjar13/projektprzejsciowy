library(shiny)
library(tidyverse)
library(bslib)

rev_data <- read_csv("DataNetflixRevenue2020_V2.csv")
sub_data <- read_csv("DataNetflixSubscriber2020_V2.csv")

netflix <- inner_join(rev_data, sub_data, by = c("Area", "Years")) %>% #laczenie dwoch tabel danych
  mutate(Years = factor(Years, levels = c(
    "Q1 - 2018", "Q2 - 2018", "Q3 - 2018", "Q4 - 2018",
    "Q1 - 2019", "Q2 - 2019", "Q3 - 2019", "Q4 - 2019",  #posortowanie danych
    "Q1 - 2020", "Q2 - 2020"
  )))


ui <- fluidPage(
  theme = bs_theme(bootswatch = "yeti"), 
  titlePanel("Analiza wyników Netflix (2018-2020)"),
  
  sidebarLayout(
    sidebarPanel(
      # Pojedynczy region dla zakładek "Wykres liniowy" i "Tabela danych"
      conditionalPanel(
        condition = "input.tabs == 'Wykres liniowy' || input.tabs == 'Tabela danych'",
        selectInput("region", "Wybierz region:", choices = unique(netflix$Area))
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Porównanie regionów'",
        checkboxGroupInput("regions_multi", "Wybierz regiony do porównania:",
                          choices = unique(netflix$Area),
                          selected = unique(netflix$Area)[1:4])  
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Porównanie trendów'",
        checkboxGroupInput("regions_trend", "Wybierz regiony do porównania:",
                          choices = unique(netflix$Area),
                          selected = unique(netflix$Area)[1:4])
      ),
      
      radioButtons("metric", "Wybierz miarę do analizy:", 
                   choices = c("Przychody" = "Revenue", 
                               "Subskrybenci" = "Subscribers")),
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Wykres liniowy", br(), plotOutput("trendPlot")),
        tabPanel("Porównanie regionów", br(), plotOutput("barPlot")),
        tabPanel("Porównanie trendów", br(), plotOutput("multiTrendPlot")),
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
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  #regiony
  output$barPlot <- renderPlot({
    req(input$regions_multi) 
    
    netflix %>%
      filter(Years == "Q2 - 2020", Area %in% input$regions_multi) %>%
      ggplot(aes(x = reorder(Area, .data[[input$metric]]), y = .data[[input$metric]], fill = Area)) +
      geom_col() +
      coord_flip() + 
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      labs(title = paste("Porównanie regionów:", input$metric, "(Q2 - 2020)"), 
           x = "Region", 
           y = input$metric,
           fill = "Region")
  })
  #porownanie
  output$multiTrendPlot <- renderPlot({
    req(input$regions_trend)
    
    netflix %>%
      filter(Area %in% input$regions_trend) %>%
      ggplot(aes(x = Years, y = .data[[input$metric]], 
                 group = Area, color = Area)) +
      geom_line(linewidth = 1.2) + 
      geom_point(size = 3) +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") +
      labs(title = paste("Porównanie trendów:", input$metric),
           x = "Kwartał", 
           y = input$metric,
           color = "Region") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            legend.position = "right")
  })
  
  # Tabela danych
  output$table <- renderTable({
    netflix %>% 
      filter(Area == input$region) %>%
      mutate(Years = as.character(Years)) 
  })
}

shinyApp(ui, server)

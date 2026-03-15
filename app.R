library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(shinydashboard)

df <- read_csv("data/raw/cleaned_full_data.csv")
df$city <- gsub("Branpton", "Brampton", df$city)

cities_coords <- read_csv("data/raw/cities_coordinates.csv")
cities_coords$City <- gsub("Branpton", "Brampton", cities_coords$City)

CITIES <- sort(unique(df$city))
CUISINES <- sort(unique(df$category_1))
PRICE_RANGES <- sort(unique(df$price_range))
CATEGORY_2 <- sort(unique(df$category_2))

OVERALL_N <- nrow(df)
OVERALL_AVG <- mean(df$star, na.rm = TRUE)
AVG_RESTAURANTS_PER_CITY <- OVERALL_N / length(CITIES)

ui <- fluidPage(
  
  titlePanel("FOODLYTICS"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(
        "price_range",
        "Price Range",
        choices = PRICE_RANGES,
        selected = PRICE_RANGES
      ),
      
      selectInput(
        "cuisine",
        "Cuisine / restaurant type",
        choices = CUISINES,
        multiple = TRUE,
        selected = CUISINES
      ),
      
      selectInput(
        "city",
        "Location",
        choices = CITIES,
        multiple = TRUE,
        selected = CITIES
      ),
      
      selectInput(
        "category_2",
        "Dish / food type",
        choices = CATEGORY_2,
        multiple = TRUE,
        selected = CATEGORY_2
      ),
      
      actionButton("reset_filters", "Reset filters")
      
    ),
    
    mainPanel(
      
      fluidRow(
        column(6,
               valueBoxOutput("total_restaurants")
        ),
        column(6,
               valueBoxOutput("avg_rating")
        )
      ),
      
      fluidRow(
        column(
          6,
          plotlyOutput("map")
        ),
        
        column(
          6,
          plotlyOutput("plot_bar_cuisine")
        )
      ),
      
      fluidRow(
        column(
          12,
          DTOutput("tbl_restaurants")
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    
    data <- df
    
    if (!is.null(input$city))
      data <- data %>% filter(city %in% input$city)
    
    if (!is.null(input$cuisine))
      data <- data %>% filter(category_1 %in% input$cuisine)
    
    if (!is.null(input$price_range))
      data <- data %>% filter(price_range %in% input$price_range)
    
    if (!is.null(input$category_2))
      data <- data %>% filter(category_2 %in% input$category_2)
    
    data
  })
  
  
  summary_stats <- reactive({
    
    data <- filtered_df()
    
    list(
      n_restaurants = nrow(data),
      avg_rating = mean(data$star, na.rm = TRUE)
    )
    
  })
  
  
  output$total_restaurants <- renderValueBox({
    
    stats <- summary_stats()
    
    valueBox(
      value = stats$n_restaurants,
      subtitle = "Total Restaurants",
      color = "blue"
    )
    
  })
  
  
  output$avg_rating <- renderValueBox({
    
    stats <- summary_stats()
    
    valueBox(
      value = round(stats$avg_rating,1),
      subtitle = "Average Rating",
      color = "green"
    )
    
  })
  
  
  output$plot_bar_cuisine <- renderPlotly({
    
    data <- filtered_df()
    
    agg <- data %>%
      count(category_1) %>%
      arrange(n)
    
    plot_ly(
      agg,
      x = ~n,
      y = ~category_1,
      type = "bar",
      orientation = "h"
    )
    
  })
  
  
  output$map <- renderPlotly({
    
    data <- filtered_df()
    
    agg <- data %>%
      group_by(city) %>%
      summarise(count = n())
    
    map_df <- merge(
      agg,
      cities_coords,
      by.x = "city",
      by.y = "City"
    )
    
    plot_ly(
      map_df,
      lat = ~Latitude,
      lon = ~Longitude,
      size = ~count,
      type = "scattergeo",
      mode = "markers"
    )
    
  })
  
  
  output$tbl_restaurants <- renderDT({
    
    data <- filtered_df()
    
    data %>%
      select(
        Restaurant = restaurant,
        Stars = star,
        Reviews = num_reviews,
        City = city,
        Price = price_range,
        Cuisine = category_1,
        Dish = category_2
      )
    
  })
  
  
  observeEvent(input$reset_filters, {
    
    updateCheckboxGroupInput(session,"price_range",selected = PRICE_RANGES)
    updateSelectInput(session,"cuisine",selected = CUISINES)
    updateSelectInput(session,"city",selected = CITIES)
    updateSelectInput(session,"category_2",selected = CATEGORY_2)
    
  })
  
}

shinyApp(ui, server)
library(shiny)
library(httr)
library(shinydashboard)
library(plotly)
library(httr)
library(jsonlite)

# Function to fetch weather data from the API
getWeatherData <- function(location, startDate, endDate) {
  api_url <- sprintf("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/%s/%s/%s?unitGroup=metric&include=days&key=AUH8YLV2LLDZUWGXMPBZYJXTP&contentType=json",
                     location, startDate, endDate)
  response <- httr::GET(api_url)
  stop_for_status(response)
  content <- httr::content(response, "text")
  weather_data <- fromJSON(content)$days
  return(weather_data)
}

# Define UI
ui <- fluidPage(
  titlePanel("Weather Details Dashboard and Predicter"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("city", "Enter City:", ""),
      style = "background-color: #337ab7;",
      dateRangeInput("dateRange", "Select Date Range:",
                     start = Sys.Date()+1,
                     min = Sys.Date()+1,
                     format = "yyyy-mm-dd",
                     separator = " - "),
      br(),
      actionButton("updateBtn", "Forecast"),
      actionButton("getWeatherBtn", "Today Weather")
    ),
    
    mainPanel(
      h3("Today Weather:"),
      textOutput("inputCity"),  # Display input city
      textOutput("temperature"),
      textOutput("feelsLike"),
      textOutput("tempMin"),
      textOutput("tempMax"),
      textOutput("pressure"),
      textOutput("humidity"),
      textOutput("windSpeed"),
      textOutput("clouds"),
      textOutput("description"),
      textOutput("sunrise"),
      textOutput("sunset"),
      
      h3("Next days how it be like:"),
      plotlyOutput("weatherPlot"),
      
      h3("Next daily Weather Forecast:"),
      verbatimTextOutput("descriptionOutput")  # Display daily weather description
    )
  )
)

# Define server
server <- function(input, output) {
  # Display input city
  output$inputCity <- renderText({
    paste("City: ", input$city)
  })
  # Reactive function to fetch weather data
  weather_data <- reactive({
    getWeatherData(input$city, format(input$dateRange[1]), format(input$dateRange[2]))
  })
  
  # Reactive function to filter data based on selected date range
  filtered_data <- reactive({
    subset(weather_data(), as.Date(datetime) >= input$dateRange[1] &
             as.Date(datetime) <= input$dateRange[2])
  })
  
  # Update plot based on button click
  observeEvent(input$updateBtn, {
    output$weatherPlot <- renderPlotly({
      plot_ly(filtered_data(), x = ~datetime, type = "scatter", mode = "lines+markers", group = 1) %>%
        add_trace(y = ~tempmax, name = "Max Temperature (°C)", marker = list(color = 'red')) %>%
        add_trace(y = ~tempmin, name = "Min Temperature (°C)", marker = list(color = 'blue')) %>%
        add_trace(y = ~precip, name = "Precipitation (mm)", yaxis = "y2", marker = list(color = 'green')) %>%
        layout(title = sprintf("Weather Forecast for %s", input$location),
               xaxis = list(title = "Date"),
               yaxis = list(title = "Temperature (°C)"),
               yaxis2 = list(title = "Precipitation (mm)", overlaying = "y", side = "right"),
               showlegend = TRUE)
    })
    
    # Generate and display the next daily Weather Forecast
    output$descriptionOutput <- renderPrint({
      daily_description <- data.frame(
        Date = as.Date(filtered_data()$datetime),
        Description = filtered_data()$description
      )
      daily_description
    })
  })
  
  observeEvent(input$getWeatherBtn, {
    
    # Check if the city input is not empty
    if (nchar(input$city) > 0) {
      # OpenWeatherMap API key
      api_key <- "ce5573ce18a852b7828d18500276a331"
      
      # Construct API URL
      api_url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=", input$city, "&appid=", api_key)
      
      # Make API request
      response <- GET(api_url)
      
      # Check if the request was successful (status code 200)
      if (http_status(response)$category == "Success") {
        weather_data <- content(response, "parsed")
        
        # Extract relevant information
        temperature <- round(weather_data$main$temp - 273.15, 2)
        feels_like <- round(weather_data$main$feels_like - 273.15, 2)
        temp_min <- round(weather_data$main$temp_min - 273.15, 2)
        temp_max <- round(weather_data$main$temp_max - 273.15, 2)
        pressure <- weather_data$main$pressure
        humidity <- weather_data$main$humidity
        wind_speed <- weather_data$wind$speed
        clouds <- weather_data$clouds$all
        description <- weather_data$weather[[1]]$description
        sunrise <- as.POSIXct(weather_data$sys$sunrise, origin = "1970-01-01", tz = "UTC")
        sunset <- as.POSIXct(weather_data$sys$sunset, origin = "1970-01-01", tz = "UTC")
        weather_icon <- weather_data$weather[[1]]$icon
        
        # Display weather details
        output$temperature <- renderText(paste("Temperature: ", temperature, "°C"))
        output$feelsLike <- renderText(paste("Feels Like: ", feels_like, "°C"))
        output$tempMin <- renderText(paste("Min Temperature: ", temp_min, "°C"))
        output$tempMax <- renderText(paste("Max Temperature: ", temp_max, "°C"))
        output$pressure <- renderText(paste("Pressure: ", pressure, "hPa"))
        output$humidity <- renderText(paste("Humidity: ", humidity, "%"))
        output$windSpeed <- renderText(paste("Wind Speed: ", wind_speed, "m/s"))
        output$clouds <- renderText(paste("Clouds: ", clouds, "%"))
        output$description <- renderText(paste("Description: ", description))
        output$sunrise <- renderText(paste("Sunrise: ", format(sunrise, "%Y-%m-%d %H:%M:%S")))
        output$sunset <- renderText(paste("Sunset: ", format(sunset, "%Y-%m-%d %H:%M:%S")))
        
        # Display weather icon
        output$weatherIcon <- renderImage({
          list(src = paste0("http://openweathermap.org/img/w/", weather_icon, ".png"),
               contentType = "image/png")
        }, deleteFile = FALSE)
      } else {
        output$temperature <- renderText("Failed to fetch weather details. Please check the city name.")
        output$feelsLike <- renderText("")
        output$tempMin <- renderText("")
        output$tempMax <- renderText("")
        output$pressure <- renderText("")
        output$humidity <- renderText("")
        output$windSpeed <- renderText("")
        output$clouds <- renderText("")
        output$description <- renderText("")
        output$sunrise <- renderText("")
        output$sunset <- renderText("")
        output$weatherIcon <- renderImage({})
      }
    }
  })
}

# Run the app
shinyApp(ui, server)

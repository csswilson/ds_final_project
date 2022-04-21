#Shiny App Example
{
  

  
  
  # ui <- fluidPage(
  #   sliderInput(inputId = "years", 
  #               label = "Year Range",
  #               min = 1880, 
  #               max = 2019, 
  #               value = c(1880,2019),
  #               sep = ""),
  #   textInput("name", 
  #             "Name", 
  #             value = "", 
  #             placeholder = "Lisa"),
  #   selectInput("sex", 
  #               "Sex", 
  #               choices = list(Female = "F", Male = "M")),
  #   submitButton(text = "Create my plot!"),
  #   plotOutput(outputId = "timeplot")
  # )
  # 
  # server <- function(input, output) {
  #   output$timeplot <- renderPlot({
  #     babynames %>% 
  #       filter(name == input$name, 
  #              sex == input$sex) %>% 
  #       ggplot() +
  #       geom_line(aes(x = year, y = n)) +
  #       scale_x_continuous(limits = input$years) +
  #       theme_minimal()
  #   })
  # }
  # 
  # shinyApp(ui = ui, server = server)}
}
# Shiny App Price Line Graph

library(shiny)
library(tidyverse)
library(rsconnect)
library(lubridate)

ui <- fluidPage(
  #sliderInput(inputId = "Year",
  #            label="Select Year",
  #            min = 1970,
  #            max = 2019,
  #            value = c(1970,2019),
  #            sep=""),
  selectInput("State","State", choices = "State"),
  submitButton(text="Submit"),
  plotOutput(outputId = "priceplot")
)

server <- function(input,output) {
  output$priceplot <- renderPlot({
    sector_prices %>% 
      
    filter(State==input$State) %>% 
      ggplot()+
      geom_line(aes(x=sector_prices$Year,y=sector_prices$Price,color=sector_prices$Sector))+
    scale_y_continuous(limits = input$Price)+
      theme_minimal()
    
  })
  
  shinyApp(ui = ui, server = server)}
  
#Shiny App Example
{library(shiny)
library(tidyverse)
library(babynames)
library(rsconnect)

ui <- fluidPage(
  sliderInput(inputId = "years", 
              label = "Year Range",
              min = 1880, 
              max = 2017, 
              value = c(1880,2019),
              sep = ""),
  textInput("name", 
            "Name", 
            value = "", 
            placeholder = "Lisa"),
  selectInput("sex", 
              "Sex", 
              choices = c(Female = "F", Male = "M")),
  submitButton("Submit"),
  plotOutput(outputId = "name_plot")
)

server <- function(input, output){
  output$name_plot <- renderPlot(
    babynames %>% 
      filter(sex == input$sex,
             name == input$name) %>% 
      ggplot(aes(y=n, x=year))+
      geom_line()+
      scale_x_continuous(limits = input$years)
  )
}

shinyApp(ui = ui, server = server)}



#Shiny App Renewables




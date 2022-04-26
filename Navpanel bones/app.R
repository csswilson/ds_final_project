

library(shiny)
library(tidyverse)
library(rsconnect)


ui <- fluidPage(
  
  ui <- navbarPage("My Application",
                   tabPanel("Written Summary",p("Intro"),
                            p("Lit Reveiw"),
                            p("Methods and Analysis"),
                            p("Conclusdion and Results")),
                   tabPanel("Analysis",
                            
                            #YOUR PAGE COMPONENETS HERE
                            
                            
                            ),
                   tabPanel("Extra")
  ))

  #YOUR SERVER HERE !!!

  server <- function(input, output) {
   output$priceplot <- renderPlot({
    sector_prices %>%

    filter(State==input$State) %>%
      ggplot()+
      geom_line(aes(x=sector_prices$Year,y=sector_prices$Price,color=sector_prices$Sector))+
    scale_y_continuous(limits = input$Price)+
      theme_minimal()

  })}
    
   shinyApp(ui = ui, server = server)



library(shiny)
  library(tidyverse)
  library(ggpubr)
  library(ggplot2)
  library(ggthemes)      # for even more plotting themes
  library(gganimate)
  library(ggmap) 
  library(rsconnect)




ui <- fluidPage(
  
  ui <- navbarPage("My Application",
                   tabPanel("Written Summary",p("Intro"),
                            p("Lit Reveiw"),
                            p("Methods and Analysis"),
                            p("Conclusdion and Results")),
                   tabPanel("Analysis",
                            
                    
                            sidebarLayout(
                              sidebarPanel(style = "position:fixed;width:inherit;",
                                           selectInput("Year", 
                                                       "Year", 
                                                       choices = c(2004,2005,2007,2009,2010,2012,2014,2016,2017,2018,2019)),
                                           radioButtons("Sector", "Electricity Sector", 
                                                        choices = c("Commercial", "Residential", "Industrial"),
                                                        selected = "Residential"),
                                           mainPanel(
                                             h1("Renewable Energy and Electricity Prices in the US", align = "center"),
                                             h3("Do higher percentages of renewables lead to a decrease in energy price in the US?",  align = "center"),
                                             #       fluidRow(
                                             #         splitLayout(cellWidths = c("50%", "50%"), plotOutput('renewable_map'), plotOutput('price_map'))
                                             #       ),
                                             plotOutput('renewable_map'), 
                                             plotOutput('price_map'),
                                             plotOutput("renewable_plot")
                              )

                            
                            ),
                   tabPanel("Extra")
                   
                   
  ))))

server <- function(input, output){
  
  
  
  output$renewables_map <-renderPlot(price_by_renewables %>% 
                                      filter(Sector == input$Sector,
                                             Year == input$Year) %>%        
                                      ggplot() +
                                      geom_map(map = states_map,
                                               aes(map_id = states_map$region,
                                                   fill = Percent)) +
                                      expand_limits(x = states_map$long, y = states_map$lat) + 
                                      theme_map() +
                                      labs(title = "Percent Renewable Energy Mix per State", fill = "Percent") +
                                      scale_fill_continuous(low = "#edf6b2" , high = "#1c5f07"),
                                    height = 400, width = 600
  )
  
  output$price_map <-renderPlot(price_by_renewables %>% 
                                  filter(Sector == input$Sector,
                                         Year == input$Year) %>%        
                                  ggplot() +
                                  geom_map(map = states_map,
                                           aes(map_id = states_map$region,
                                               fill = Price)) +
                                  expand_limits(x = states_map$long, y = states_map$lat) + 
                                  theme_map() +
                                  labs(title = "Electricity Price per State", fill = "Price\n$ / BTU") +
                                  scale_fill_continuous(low = "slategray1" , high = "slategray"),
                                height = 400, width = 600
  )
  
  output$renewable_plot <- renderPlot(
    price_by_renewables %>% 
      filter(Sector == input$Sector,
             Year == input$Year) %>% 
      ggplot(aes(y= Percent, x= Price))+
      geom_point() +
      stat_cor( aes( label = paste(..rr.label.., ..p.label.., sep = "*`,`~"))) +
      theme_minimal() +
      #geom_smooth(se = FALSE, method = "lm", color = "darkgreen") +
      labs(title = "Percent Renewable Mix per Price", y = "$ / BTU", x = "%")
  )
  
}
shinyApp(ui = ui, server = server)




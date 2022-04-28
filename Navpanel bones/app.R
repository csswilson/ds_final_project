
  library(shiny)
  library(tidyverse)
  library(ggpubr)
  library(ggplot2)
  library(ggthemes)      # for even more plotting themes
  library(gganimate)
  library(ggmap) 
  library(rsconnect)
  library(lubridate)

# Data cleaning 

# Electricity Price Clean
all_prices <- readr::read_csv('All_price_info.csv')

sector_prices <- all_prices %>% 
  filter(MSN == 'ESCCD' | MSN == 'ESICD' | MSN == 'ESRCD') %>% 
  pivot_longer(cols = -c('Data_Status', 'State', 'MSN'),
               names_to = "Year",
               values_to = "Price") %>% 
  select(-'Data_Status') %>% 
  mutate(Year = year(years(Year)))

sector_prices$MSN[sector_prices$MSN == "ESCCD"] <- "Commercial"
sector_prices$MSN[sector_prices$MSN == "ESICD"] <- "Industrial"
sector_prices$MSN[sector_prices$MSN == "ESRCD"] <- "Residential"

sector_prices <- sector_prices %>% 
  rename(Sector = MSN)

# Percent Renewable Clean

mix_04 <- read.csv("mix_04.csv") %>% 
  tail(-3)
colnames(mix_04) <- mix_04[1,]
mix_04 <- mix_04[-1, ] 
mix_04 <- mix_04 %>% 
  select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2004` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_04 <- mix_04[!(mix_04$PSTATABB == "DC"),] %>% 
  na.omit()

mix_05 <- read.csv("mix_05.csv") %>% 
  tail(-3)
colnames(mix_05) <- mix_05[1,]
mix_05 <- mix_05[-1, ] 
mix_05 <- mix_05 %>% 
  select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2005` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_05 <- mix_05[!(mix_05$PSTATABB == "DC"),] %>% 
  na.omit()

mix_07 <- read.csv("mix_07.csv") %>% 
  tail(-3)
colnames(mix_07) <- mix_07[1,]
mix_07 <- mix_07[-1, ] 
mix_07 <- mix_07 %>% 
  select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2007` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_07 <- mix_07[!(mix_07$PSTATABB == "DC"),] %>% 
  na.omit()

mix_09 <- read.csv("mix_09.csv") %>% 
  tail(-3)
colnames(mix_09) <- mix_09[1,]
mix_09 <- mix_09[-1, ] 
mix_09 <- mix_09 %>% 
  select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2009` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_09 <- mix_09[!(mix_09$PSTATABB == "DC"),] %>% 
  na.omit()

mix_10 <- read.csv("mix_10.csv") %>% 
  tail(-3)
colnames(mix_10) <- mix_10[1,]
mix_10 <- mix_10[-1, ] 
mix_10 <- mix_10 %>% 
  select(c(PSTATABB , STTRPR) )%>% 
  mutate(`2010` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_10 <- mix_10[!(mix_10$PSTATABB == "DC"),] %>% 
  na.omit()

mix_12 <- read.csv("mix_12.csv") %>% 
  tail(-3)
colnames(mix_12) <- mix_12[1,]
mix_12 <- mix_12[-1, ] 
mix_12 <- mix_12 %>% 
  select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2012` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_12 <- mix_12[!(mix_12$PSTATABB == "DC"),] %>% 
  na.omit()

mix_14 <- read.csv("mix_14.csv")
colnames(mix_14) <- mix_14[1,]
mix_14 <- mix_14[-1, ] 
mix_14 <- mix_14 %>%
  rename(PSTATABB = ` PSTATABB `,
         STTRPR = ` STTRPR `) %>% 
  select(c(PSTATABB, STTRPR)) %>% 
  mutate(`2014` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_14 <- mix_14[!(mix_14$PSTATABB == "DC"),] %>% 
  na.omit()

mix_16 <- read.csv("mix_16.csv")
colnames(mix_16) <- mix_16[1,]
mix_16 <- mix_16[-1, ] 
mix_16 <- mix_16 %>% 
  select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2016` = as.numeric(STTRPR)) %>% 
  select(-STTRPR)
mix_16 <- mix_16[!(mix_16$PSTATABB == "DC"),] %>% 
  na.omit()

mix_18 <- read.csv("mix_18.csv")
colnames(mix_18) <- mix_18[1,]
mix_18 <- mix_18[-1, ] 
mix_18 <- mix_18 %>% select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2018` = as.numeric(STTRPR %>% str_remove("%"))) %>% 
  select(-STTRPR)
mix_18 <- mix_18[!(mix_18$PSTATABB == "DC"),] %>% 
  na.omit()

mix_19 <- read.csv("mix_19.csv")
colnames(mix_19) <- mix_19[1,]
mix_19 <- mix_19[-1, ] 
mix_19 <- mix_19 %>% select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2019` = as.numeric(STTRPR %>% str_remove("%"))) %>% 
  select(-STTRPR)
mix_19 <- mix_19[!(mix_19$PSTATABB == "DC"),] %>% 
  na.omit()

mix_20 <- read.csv("mix_20.csv")
colnames(mix_20) <- mix_20[1,]
mix_20 <- mix_20[-1, ] 
mix_20 <- mix_20 %>% select(c(PSTATABB , STTRPR)) %>% 
  mutate(`2020` = as.numeric(STTRPR %>% str_remove("%"))) %>% 
  select(-STTRPR)
mix_20 <- mix_20[!(mix_20$PSTATABB == "DC"),] %>% 
  na.omit()

# Renewable Resource Percentage Mix
percent_renewable <- mix_04 %>% 
  left_join(mix_05, by = 'PSTATABB') %>% 
  left_join(mix_07, by = 'PSTATABB') %>% 
  left_join(mix_09, by = 'PSTATABB') %>% 
  left_join(mix_10, by = 'PSTATABB') %>% 
  left_join(mix_12, by = 'PSTATABB') %>% 
  left_join(mix_14, by = 'PSTATABB') %>% 
  left_join(mix_16, by = 'PSTATABB') %>% 
  left_join(mix_18, by = 'PSTATABB') %>% 
  left_join(mix_19, by = 'PSTATABB') %>% 
  left_join(mix_20, by = 'PSTATABB') %>% 
  rename(State = PSTATABB)

percent_renewable <- 
  percent_renewable %>% 
  pivot_longer(cols = c(-State),
               names_to = 'Year',
               values_to = 'Percent'
  ) %>% 
  mutate(Year = year(years(Year)))

price_by_renewables <- percent_renewable %>% 
  left_join(sector_prices, by = c('State', "Year")) %>% 
  filter(Year != '2020')

state_abbreve <- tibble(state.abb, state.name) %>% 
  mutate(region = str_to_lower(state.name),
         State = state.abb) %>% 
  select(c(State, region))

# sector_prices - just electricity prices by sector
sector_prices <- sector_prices %>% 
  left_join(state_abbreve, by = 'State')

# percent_renewable - just renewable

percent_renewable <- percent_renewable %>% 
  left_join(state_abbreve, by = 'State')

# price_by_renewables - combined, only years: 04, 05, 07, 09, 10, 12, 14, 16, 18, 19

price_by_renewables <- price_by_renewables %>% 
  left_join(state_abbreve, by = 'State')

states_map <- map_data("state")


ui <- navbarPage("My Application",
                 tags$style(type = 'text/css', 
                            HTML(".container-fluid > .nav > li > 
                        a[data-value='Written Summary'] {background-color: grey; color:black}",
                                 ".container-fluid > .nav > li > 
                        a[data-value='Analysis'] {background-color: grey; color:black}" )),
                   tabPanel("Written Summary",
                            h2("Renewable Energy of the Ages",align = "center"),
                            h3("A United States Clean Energy Story", align="center"
),
                            h5("Introduction and Background", align = "center"),
p(""),
                   p("For our project, we explored generation of electricity in United States, with a specific focus on renewable sources of energy. Renewable Sources include wind, solar, geothermal, and hydroelectric energy sources. There are many other energy sources such as energy from coal, natural gas and nuclear. Each state in US has its own supply and demand for electircity, and must determine what energy sources will help meet their demand, and how much of each energy source. Part of this decision is made based on the resources available to states. Some states have a lot of potential solar power that they could harvest while others have more wind power. Some states are located in coal country, or are really close to an already established nuclear power plant. Each state therefore, has a different composition and mix of energy sources per year, meaning a unique percentage. We are interested in exploring how states have incorporated renewables into their energy mix over time and how the price of electricity in different states has change overtime. We believe that the percent of renewables in the energy mixes of states has increased over time just with increase in technology and decrease in prices of ways to harvest energy. We expect the electricity prices in these states with higher percent renewables, to decrease and elecetricy should be cheeper. This is because the cost of harnessing renewable energy should just be the upfront cost of infrastructure, instead of an added cost of purchasing the energy source such as mining coal or extracting raw natural gas. Therefore, we are curious if a the percentage of renewables is related to energy price, and whether the increase in renewables is related to a decrease in price. 
"),
                   h5("Data Collection", align = "center"),
                            p("Price of electricity over time : 
Link
From US Energy Information Administration
Percent renewables over time
Link
EPA eGrid

We used data from two sources in this exploration. The first dataset we looked at was how the price of electricity over time in three different sectors: Residential, Industrial and Commercial. We found this data in a large dataset on the US Energy Information Administration page as a downloadable spreadsheet. We were able to clean it in r and select the prices that we were interested in looking at and the dates we were interested in. The second dataset that explored was the percent of renewable energy, per state, per year. This started as 11 different datasets foun on the EPA eGrid webpage. On their website, they have a dataset per year with all the states information including the column on energy mix percent renewable. We were able to download 11 years of data between 2004 and 2020, clean each one in r and combine them together into a long dataset of each state, per year, and their percent of energy generated that was renewable. Finally, we were able to combine these two datasets by state and by year.
"),
                   h5("Analysis", align = "center"),
                            p("
Based on this piece of preliminary organization, we cannot confidently state that higher percentages of renewables lead to decrease in electricity prices. This takeaway is easily observed by following the case of California, one of the US’ leaders in renewable energy. If you select 2004 as your initial year, you can see that renewable energy and electricity prices are higher than average and that renewable energy is negatively correlated with energy prices in the bottom figure. This holds mostly true for 2019, where electricity price is clearly increasing along with percent renewables. The simple fit line plot shows an apparent  minor positive correlation, yet it also demonstrates poor statistical significance. From a much less technical point of view, the 3rd figure demonstrates a mostly consistently rising price in electricity, across sectors, for all states, despite significant increases in renewable energy mix. Finally, it is worth noting that these figures, as they are designed for visual interpretation, lack the necessary specificity to return meaningful results. These figures are better suited to understanding the fact that there is no strict and obvious correlation between these two systems, observing the way their values change spatially, and coming to understand that electricity prices have been rising consistently for many years but at differing rates across sectors. 
")),
                          
                   tabPanel("Analysis",
                            titlePanel("Analysis"),
                            sidebarLayout(
                              sidebarPanel(style = "position:fixed;width:inherit;",
                                           selectInput("Year", 
                                                       "Year", 
                                                       choices = c(2004,2005,2007,2009,2010,2012,2014,2016,2018,2019)),
                                           radioButtons("Sector", "Electricity Sector", 
                                                        choices = c("Commercial", "Residential", "Industrial"),
                                                        selected = "Residential"),
                                           selectInput("State", 
                                                       "State",
                                                       multiple = TRUE,
                                                       choices = unique(sector_prices$State),selected = "AK")
                              ),
                              mainPanel(
                                h1("Renewable Energy and Electricity Prices in the US", align = "center"),
                                h3("Do higher percentages of renewables lead to a decrease in energy price in the US?",  align = "center"),
                                #       fluidRow(
                                #         splitLayout(cellWidths = c("50%", "50%"), plotOutput('renewable_map'), plotOutput('price_map'))
                                #       ),
                                plotOutput('renewable_map'), 
                                plotOutput('price_map'),
                                plotOutput(outputId = "priceplot"),
                                plotOutput("renewable_plot")
                              
                              )
                            )
                              ),

                            
                tabPanel("Extra"))
                  

server <- function(input, output){
  
  
  
  output$renewable_map <-renderPlot(price_by_renewables %>% 
                                      filter(Sector == input$Sector,
                                             Year == input$Year) %>%        
                                      ggplot() +
                                      geom_map(map = states_map,
                                               aes(map_id = region,
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
                                           aes(map_id = region,
                                               fill = Price)) +
                                  expand_limits(x = states_map$long, y = states_map$lat) + 
                                  theme_map() +
                                  labs(title = "Electricity Price per State", fill = "Price\n$ / BTU") +
                                  scale_fill_continuous(low = "slategray1" , high = "slategray"),
                                height = 400, width = 600
  )
  
  output$priceplot <- renderPlot({
    sector_prices %>%
      filter(State==input$State,
             Sector==input$Sector) %>% 
      ggplot() +
      geom_line(aes(x = Year, y = Price, group = State, color = State)) +
      theme_minimal()+
      labs(title = "Price by Sector", x="",y="")

  })
  
  output$renewable_plot <- renderPlot(
    price_by_renewables %>% 
      filter(Sector == input$Sector,
             Year == input$Year) %>% 
      ggplot(aes(y= Percent, x= Price))+
      geom_point() +
      stat_cor( aes( label = paste(..rr.label.., ..p.label.., sep = "*`,`~"))) +
      theme_minimal() +
      geom_smooth(se = FALSE, method = "lm", color = "darkgreen") +
      labs(title = "Percent Renewable Mix per Price", y = "$ / BTU", x = "%")
  )
  
}
shinyApp(ui = ui, server = server)




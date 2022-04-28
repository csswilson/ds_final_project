
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

ui <- fluidPage(
  selectInput("State", 
              "State",
              multiple = TRUE,
              choices = unique(sector_prices$State),selected = "AK"),
  radioButtons("Sector", "Electricity Sector", 
                           choices = c("Commercial", "Residential", "Industrial"),
                           selected = "Residential"),            
  plotOutput(outputId = "priceplot")
)

server <- function(input, output) {
  output$priceplot <- renderPlot({
  sector_prices %>%
    filter(State==input$State,
           Sector==input$Sector) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = Price, group = State, color = State)) +
    theme_minimal()
  })
}



shinyApp(ui = ui, server = server)
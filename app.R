

library(tidyverse)     # for data cleaning and plotting
library(dplyr)
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(transformr)    # for "tweening" (gganimate)
library(gifski)        # need the library for creating gifs but don't need to load each time
library(shiny)         # for creating interactive apps
theme_set(theme_minimal())

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

census_pop_est_2018 <- 
  read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

covid_dens<- covid19 %>%
  mutate(state=str_to_lower(state))%>%
  select(-c(fips,deaths)) %>%
  left_join(census_pop_est_2018,
            by = "state") %>%
  mutate(`Cumulative cases by state per 10,000` = (cases/est_pop_2018) *10000) %>%
  select(-c(cases,est_pop_2018))%>%
  mutate(state=str_to_title(state))


ui<-fluidPage(
  dateRangeInput(inputId = "date", 
                 label = "Date Range", 
                 format = "yyyy-mm-dd"),
  
  selectInput(inputId="state", 
              label="Choose the state(s): ", 
              choices= c(Alabama="Alabama", 
                         Alaska="Alaska",
                         Arizona="Arizona",
                         Arkansas="Arkansas",
                         California="California",
                         Colarado="Colorado",
                         Connecticut="Connecticut",
                         Delaware="Delaware",
                         Florida="Florida",
                         Georgia="Georgia",
                         Hawaii="Hawaii",
                         Idaho="Idaho",
                         Illinois="Illinois",
                         Indiana="Indiana",
                         Iowa="Iowa",
                         Kansas="Kansas",
                         Kentucky="Kentucky",
                         Louisiana="Louisiana",
                         Maine="Maine",
                         Maryland="Maryland",
                         Massachusetts="Massachusetts",
                         Michigan="Michigan",
                         Minnesota="Minnesota",
                         Mississippi="Mississippi",
                         Missouri="Missouri",
                         Montana="Montana",
                         Nebraska="Nebraska",
                         Nevada="Nevada",
                         `New Hampshire`="New Hampshire",
                         `New Jersey`="New Jersey",
                         `New Mexico`="New Mexico",
                         `New York`="New York",
                         `North Carolina`="North Carolina",
                         `North Dakota`="North Dakota",
                         Ohio="Ohio",
                         Oklahoma="Oklahoma",
                         Oregon="Oregon",
                         Pennsylvania="Pennsylvania",
                         `Rhode Island`="Rhode Island",
                         `South Carolina`="South Carolina",
                         `South Dakota`="South Dakota",
                         Tennessee="Tennessee",
                         Texas="Texas",
                         Utah="Utah",
                         Vermont="Vermont",
                         Verginia="Virginia",
                         Washington="Washington",
                         `West Virginia`="West Virginia",
                         Wisconsin="Wisconsin",
                         Wyoming="Wyoming"
                         ),
              multiple = TRUE),
  
  submitButton(text = "Create my plot!"),
  
  plotOutput(outputId = "covidplot")
  
)


server <- function(input,output) {
 output$covidplot <- renderPlot(
   
   covid_dens %>%
     filter(state== input$state) %>%
     ggplot(aes(x=date,
                y=`Cumulative cases by state per 10,000`,
                color=state)) +
     geom_line() +
     labs(x="",y="",
          title="Cumulative cases per 10,000")+
     scale_x_date(limits =input$date)
  
 ) 
}




shinyApp(ui=ui,server=server)











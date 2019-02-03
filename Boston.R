
# Changing Parameter RDS
library(leaflet.extras)
library(dplyr)

data <- readRDS("crime.rds")
holiday <- readRDS("holiday.rds")
count_holiday <- holiday %>% group_by(crime_date,CRIME) %>% summarise(count = n())
count_holidaytotal <- holiday %>% group_by(crime_date) %>% summarise(count = n())
count_hohoholiday <- holiday %>% group_by(year) %>% summarise(count = n())
count_crime <- data %>% group_by(year,CRIME) %>% summarise(count = n())
count_shift <- data %>%  group_by(year,shift) %>% summarise(count = n())
count_shift$count <- as.numeric(count_shift$count)
count_crime$count <- as.numeric(count_crime$count)
count_district <- data %>% group_by(year,district) %>% summarise(count = n())
choice <- unique(count_crime$CRIME)
total_crime <- data %>% group_by(year) %>% summarise(total = n())
total_crime$count <- as.numeric(total_crime$total)


# -------------------SERVER.R-------
library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(leaflet)
library(googleVis)
library(tidyverse)
library(shinydashboard)
library(googleVis)
library(dplyr)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  reactthis <- reactive({
    count_shift %>% 
      filter(year == input$year)
  })
  output$plotshift <- renderPlot({
    ggplot(data = reactthis(), aes(x = shift, y = count, fill = shift)) + 
      geom_bar(stat = 'identity') + 
      guides(fill = F) + 
      labs(x = 'Shift', y = 'Total Number of Crimes', title = 'Total Number of Crimes per Shift')
  })
  reactcrime <- reactive({
    count_crime %>% 
      filter(year == input$year1)
  })
  output$plotcrime <- renderPlot({
    ggplot(reactcrime(), aes(x = CRIME, y = count, fill = CRIME)) + 
      geom_bar(stat = 'identity') +  
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      labs(x = '', y = 'Total Number of Crimes', title = 'Total Count of Crimes per Category')
  })
  reactdistrict <- reactive({
    count_district %>% 
      filter(year == input$year2)
  })
  output$plotdistrict <- renderPlot({
    ggplot(reactdistrict(),aes(x = district, y = count, fill = district)) + 
      geom_bar(stat = 'identity') +
      guides(fill = F) +
      labs(x = 'District', y = 'Total Number of Crimes', title = 'Total Number of Crimes per District')
  })
  output$rplocation <- renderLeaflet({
    rpt_locs <- data %>% 
      filter(CRIME == input$zone) %>% 
      group_by(LONGITUDE, LATITUDE,district,location) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      arrange(desc(n)) %>% 
      slice(1:10)
    popup <- paste0("<strong>Frequency: </strong>", rpt_locs$n,
                    "<br><strong>Location: </strong>", rpt_locs$location,
                    "<br><strong>Borough: </strong>", rpt_locs$district)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(data = rpt_locs,
                       ~LONGITUDE, ~LATITUDE, 
                       fillColor = "white", color = "red",  
                       radius = ~n*0.1, 
                       popup = ~popup) %>% 
      setView(-71.0589,42.3601,zoom = 12)
  })
  output$total <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-71.0589,42.3601,zoom = 11)
  })
  
  observe({
    proxy <- leafletProxy("total") %>% 
      clearMarkers() %>% 
      clearMarkerClusters() %>% 
      addCircleMarkers(data = data %>% 
                         filter_(ifelse(input$type == "ALL",TRUE,"`CRIME` == input$type")),
                       clusterOptions = markerClusterOptions(), 
                       ~LONGITUDE,~LATITUDE, popup="BOSTON",color = 'Red',radius = 1)
  })
  
  output$heatmap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(-71.0589,42.3601,zoom = 11)
  })
  
  observe({
    proxy <- leafletProxy("heatmap") %>% 
      removeWebGLHeatmap(layerId = 'a') %>% 
      addWebGLHeatmap(layerId = 'a',data = data %>% filter_(ifelse(input$hot == "ALL",TRUE,"`CRIME` == input$hot")), lng=~LONGITUDE, lat=~LATITUDE, size = 200)
    
  })
  
  output$Scatter <- renderGvis({
    gvisScatterChart(count_hohoholiday, 
                     options=list(
                       legend="none",
                       lineWidth=2, pointSize=6,
                       title="Holidays",
                       width=600, height=600))
    
  })
  output$Scatter1 <- renderGvis({
    gvisScatterChart(total_crime,
                     options = list(
                       legend="none",
                       lineWidth=2, pointSize=5,
                       title="ALL TOTAL",
                       width=600,height=600))
  })
})

#--------------------ui.R---------------

library(shiny)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(dplyr)


ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Boston"),
  dashboardSidebar(
    sidebarUserPanel("Crime Analyst 🤓!",image = "https://pngimage.net/wp-content/uploads/2018/05/boston-png-5.png"),
    sidebarMenu(
      menuItem("Crime",
               menuSubItem("shift",tabName = "shift"),
               menuSubItem("Crime Type",tabName = "crime_type"),
               menuSubItem("District",tabName = "district")),
      menuItem("Map",tabName = "map", badgeLabel = "COOL!", badgeColor = "green",icon = icon("globe")),
      menuItem("DENSITY MAP",tabName = "heat", badgeLabel = "HEAT!!!", badgeColor = "red", icon = icon("sun-o")),
      menuItem("WARNING",tabName = "warn", badgeLabel = "!!!", badgeColor = "yellow",icon = icon("ban")),
      menuItem("Holiday",tabName = "holiday", badgeLabel = "FUN", badgeColor = "maroon",icon = icon("flag")),
      menuItem("Conclusion",tabName = "rate", badgeLabel = "?!", badgeColor = "blue", icon = icon("commenting")),
      menuItem("Summarise",tabName = "sum"),
      menuItem("Info",tabName = "info", icon = icon("info"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      
      tabItem(tabName = "shift",
              selectizeInput("year", "Select Year", 
                             c(2015, 2016, 2017,2018), 
                             selected = 2015),
              plotOutput("plotshift")),
      tabItem(tabName = "crime_type",
              h2("Crime"),
              fluidRow(
                box(selectizeInput("year1","Select Year", 
                                   c(2015,2016,2017, 2018), 
                                   selected = 2015)),
                plotOutput("plotcrime")
              )),
      tabItem(tabName = "district",
              selectizeInput("year2","Select Year",
                             c(2015,2016,2017,2018),
                             selected = 2015),
              plotOutput("plotdistrict"),
              br(),
              fluidRow(img(src='https://en.wikipedia.org/wiki/List_of_members_of_Boston_City_Council#/media/File:2003_districts_BostonCityCouncil.jpg'))
      ),
      tabItem(tabName = "map",
              h2("Mapping"),
              selectizeInput("type","Select Crime Type",
                             c(ALL = "ALL",choice)),
              leafletOutput("total",width = 500,height = 500)),
      tabItem(tabName = "heat",
              h2("Density MAP"),
              selectizeInput("hot","Select Crime Type",
                             c(ALL = "ALL",choice)),
              leafletOutput("heatmap")),
      tabItem(tabName = "warn",
              h2("Red Zone"),
              selectizeInput("zone","Select Crime Type",
                             choice),
              leafletOutput("rplocation")),
      tabItem(tabName = "holiday",
              h2("Should We Enjoy the Holidays?"),
              htmlOutput("Scatter"),
              h4("Percentage: 2015:0.55%
                 2016:0.85%
                 2017:0.94%
                 2018:0.86%")),
      tabItem(tabName = "rate",
              h2("Let's Check on the Comparison"),
              htmlOutput("Scatter1"),
              h4("Total Change: 2015:+1.5% 2016:+7% 2017:-3% 2018:-0.2%")),
      tabItem(tabName = "sum",
              h2("Summary"),
              h4("1.The presentation deep dives into the analysis of crimes and felonies that has been occured in the city of Boston from 2015 till today.) "),
              h4("2.The dataset begins from August 2015 to December 13th 2018, As of December 13th, 2018, there are 346,628 incidents and 17 variables; ranging from types of offense, reported area and reporting area, date occured, street, and the longitude and latitude of the incident. The size of the dataset is 68.8mb"),
              h4("3.We plan to explore the incidents backwards through time, and mine for patterns by time and location and to visually represent these results. These results are not only potentially beneficial to the law enforcement, but they can also be beneficial for the residents of Boston to see and understand where and how often crime is happening in their neighborhood. We hope that by understanding the frequency of incidents in a neighborhood, residents can be proactive about how they report incidents."),
              h4("4.After performing in-depth analysis on the Boston Crime Data Set, we can clearly see the trends and relations between the types of crimes, location and the occurance of the crime. Some of the notable takeaways from the analysis are mentioned below:"),
                   
                  h4("5. The most affected districts in Boston are Dorchester, South-End, Roxbury.It can be noted that the highest number of the crimes were reported during summer months of July and Auguest.Motor-Vehicle accident response were the highest number of report registered with the Boston Police.This analysis can help Boston Police act accordingly and try to reduce the crimes frequently occuring in the city of Boston."),
              h4(" Hold your clap one more minutes. lol 🤓 !!! ")),
      tabItem(tabName = "info",
              h4("Data Source: https://data.boston.gov/dataset/6220d948-eae2-4e4b-8723-2dc8e67722a3/resource/12cb3883-56f5-47de-afa5-3b1cf61b257b/download/crime_incident_reports"),
              h4("Newest Data Record Date: 2018/12/21"),
              h4("Holiday Definition:"),
              h4("🍾🍾🍾New Year🍾🍾aaa,"),
              h4("👑👑👑Martin Luther King, Jr.👑👑👑"),
              h4("🎂🎂🎂Washington’s Birthday🎂🎂🎂"),
              h4( "🚀🚀Memorial Day🚀🚀"),
              h4("🇺🇸🇺🇸Independence Day"),
              h4("👳👳👳Labor Day👳👳👳"),
              h4("⛵🕵️️⛵️🕵️⛵️Columbus Day⛵🕵️️⛵🕵️️⛵️⛵️"),
              h4(" 👤☠️☠️ Veterans Day ☠️☠👤️"),
              h4("🙏🙏🙏Thanksgiving Day🙏🙏🙏🙏"),
              h4("☃️☃️☃🎅🎅🎅),️☃️Christmas Day ☃️☃️☃️☃️"),
              h4("Author: 😍 PANKAJ SHAH 😍"),
              h4("Email: pankajautomatic@gmail.com 🤓 🙏"),
              h4("👏👏👏👏👏👏👏👏👏👏👏👏👏👏👏👏"))
              )
    )
  )

#-----------

shinyApp(ui, server)


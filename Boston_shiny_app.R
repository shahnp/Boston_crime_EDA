# Boston Shiny app

library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(leaflet)
library(googleVis)
library(tidyverse)
library(shinydashboard)
library(googleVis)
library(leaflet.extras)
library(dplyr)

#------------------------

holiday <- readRDS("~/Desktop/holiday.rds")
crime <- readRDS("~/Desktop/crime.rds")

#----------------

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

# ---------------------


# Define server logic required to draw a histogram
server <-function (input, output) {
  
  reactthis <- reactive({
    count_shift %>% 
      filter(year == input$year)
  })
  output$plotshift <- renderPlot({
    ggplot(data = reactthis(), aes(x = shift, y = count, fill = shift)) + 
      geom_bar(stat = 'identity') + 
      guides(fill = F) + 
      labs(x = 'Shift', y = 'Total Number of Crimes', title = 'Total Number of Crimes per Shift in Boston Area')
  })
  reactcrime <- reactive({
    count_crime %>% 
      filter(year == input$year1)
  })
  output$plotcrime <- renderPlot({
    ggplot(reactcrime(), aes(x = offense_category, y = count, fill = offense_category)) + 
      geom_bar(stat = 'identity') +  
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      labs(x = '', y = 'Total Number of Crimes in Boston District', title = 'Total Count of Crimes per Category')
  })
  reactdistrict <- reactive({
    count_district %>% 
      filter(year == input$year2)
  })
  output$plotdistrict <- renderPlot({
    ggplot(reactdistrict(),aes(x = distrrict, y = count, fill = district)) + 
      geom_bar(stat = 'identity') +
      guides(fill = F) +
      labs(x = 'District', y = 'Total Number of Crimes', title = 'Total Number of Crimes per District in Boston')
  })
  output$rplocation <- renderLeaflet({
    rpt_locs <- crime %>% 
      filter(offense_category == input$zone) %>% 
      group_by(long,lat,district,location) %>%
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
                       ~long, ~lat, 
                       fillColor = "white", color = "red",  
                       radius = ~n*0.1, 
                       popup = ~popup) %>% 
      setView(-71.0892,42.3398,zoom = 16) 
  })
  output$total <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-71.0892,42.3398,zoom = 16)
  })
  
  observe({
    proxy <- leafletProxy("total") %>% 
      clearMarkers() %>% 
      clearMarkerClusters() %>% 
      addCircleMarkers(data = crime %>% 
                         filter_(ifelse(input$type == "ALL",TRUE,"`offense_code` == input$type")),
                       clusterOptions = markerClusterOptions(), 
                       ~long,~lat, popup="Boston ",color = 'Red',radius = 1)
  })
  
  output$heatmap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      setView(-71.0892,42.3398,zoom = 16)
  })
  
  observe({
    proxy <- leafletProxy("heatmap") %>% 
      removeWebGLHeatmap(layerId = 'a') %>% 
      addWebGLHeatmap(layerId = 'a',data = crime %>% filter_(ifelse(input$hot == "ALL",TRUE,"`offense_code` == input$hot")), lng=~LONGITUDE, lat=~LATITUDE, size = 200)
    
  })
  
  output$Scatter <- renderGvis({
    gvisScatterChart(count_holiday_year, 
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
}

#-----------------

library(shiny)
library(shinythemes)
# library(ggplot2)
# library(data.table)
library(leaflet)
# library(googleVis)
# library(tidyverse)
library(shinydashboard)
# library(googleVis)
library(dplyr)


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Boston"),
  dashboardSidebar(
    sidebarUserPanel("Be Safe!",image = "https://washington.org/ddc_opengraph.png"),
    sidebarMenu(
      menuItem("Crime",
               menuSubItem("Shift",tabName = "shift"),
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
                             c(2015, 2016, 2017, 2018), 
                             selected = 2015),
              plotOutput("plotshift")),
      tabItem(tabName = "crime_type",
              h2("Crime"),
              fluidRow(
                box(selectizeInput("year1","Select Year", 
                                   c(2015,2016,2017), 
                                   selected = 2012)),
                plotOutput("plotcrime")
              )),
      tabItem(tabName = "district",
              selectizeInput("year2","Select Year",
                             c(2015,2016,2017),
                             selected = 2012),
              plotOutput("plotdistrict"),
              br(),
              fluidRow(img(src='districtmap.jpg'))
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
              h4("Percentage: 2012:0.85%
                 2013:0.85%
                 2014:0.94%
                 2015:0.90%
                 2016:0.86%")),
      tabItem(tabName = "rate",
              h2("Let's Check on the Comparison"),
              htmlOutput("Scatter1"),
              h4("Total Change: 2013:+1.5% 2014:+7% 2015:-3% 2016:-0.2%")),
      tabItem(tabName = "sum",
              h2("Summary"),
              h4("1.District is importatnt!"),
              h4("2.Enjoy your holiday!"),
              h4("3.Protect yourself is alwasy important.")),
      tabItem(tabName = "info",
              h4("Data Source: http://opendata.dc.gov/"),
              h4("Newest Data Record Date: 2017/07/13"),
              h4("Holiday Definition: New Year,Independence Day,Thanksgiving,Christmas"),
              h4("Author: Pankaj Shah"),
              h4("Email: Pankajautomatic@gmail.com"))
              )
    )
  )
#-----------------

shinyApp(ui, server)



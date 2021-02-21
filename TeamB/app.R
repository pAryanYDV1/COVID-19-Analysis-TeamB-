library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(leaflet)
library(htmlwidgets)


ui <- shinyUI(
    dashboardPage(skin="black",
                  dashboardHeader(title="Analysis on COVID-19",titleWidth=320),
                  dashboardSidebar(width=320,
                                   sidebarMenu(
                                       menuItem("1) Study on Country Vaccinations",tabName = "one"),
                                       menuSubItem("Geographical Distribution of COVID-19 Vaccines",tabName="first",icon = icon("project-diagram")),
                                       menuSubItem("Plot of Daily Vaccination Amount",tabName="second",icon = icon("chart-line")),
                                       menuSubItem("Prediction on Daily Vaccination",tabName="third",icon = icon("hourglass-start")),
                                       menuItem("2) Study on COVID-19 Healthy Diet",tabName = "two"),
                                       menuSubItem("Correlation Analysis",tabName="fourth",icon = icon("paperclip")),
                                       menuSubItem("Relationship of Foods with Consequences",tabName="fifth",icon = icon("bacon"))
                                   )
                  ),
                  
                  
                  
                  
                  dashboardBody(
                      tabItems(
                          tabItem(tabName = "one"),
                          tabItem(
                              tabName="first",h3("Map View for Number of People Vaccinated & Name of the Vaccines Distributed by the Countries around the Globe "),
                              fluidRow(
                                  box(solidHeader = T,background = "navy",tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations/locations.csv", "COVID-19 Data Repository", target="_blank"),
                                      h5("All data are aggregated by people vaccinated and people fully vaccinated.The repository contains information about vaccine update from the official sites of different country.Likewise, it also reflects name of vaccines distributed by the different countries along with their ISO Code"),
                                      selectInput("name",
                                                  "Select the Name of Country:", 
                                                  choices= unique(CountryVaccinations$country))
                                      
                                      
                                      
                                  ),
                                  
                                  box(background = "orange",tabsetPanel(
                                      tabPanel("Amount of People Vaccinated", leafletOutput("people_vaccinated")))),
                                  
                                  box(solidHeader = T,background = "olive" , tableOutput("data"),width = 8)
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                              )),
                          tabItem(tabName="second",h3("Line Chart showing Daily Vaccination done on different Dates in Various Countries")),
                          tabItem(tabName="third",h3("Prediction of Total Amount of Vaccination at March 30,2021 with current rate of Daily Vaccination")),
                          tabItem(tabName = "two"),
                          tabItem(tabName="fourth",h3("Finding Features that are Correlated with Confirm Cases")),
                          tabItem(tabName="fifth",h3("Searching which type of Foods are Responsible for what kind of Consequences"))
                      ))))






server <- function(input,output){
    
    output$data <- renderTable({
        countryFilter <- subset(CountryVaccinations,CountryVaccinations$country==input$name)
        
        
    })
    
    output$people_vaccinated <- renderLeaflet({
        CountryVaccinations<-CountryVaccinations%>%mutate(popup_info=paste("Name of Country:",country,"<br/>","ISO_Code:",iso_code,"<br/>","Name of the Vaccines Distributed:",vaccines,"<br/>","Amount of People Vaccinated:",people_vaccinated,"<br/>","Amount of People Fully Vaccinated:",people_fully_vaccinated))
        
        CountryVaccinations$popup_info
        
        
        colors<-c("red","navy")
        pal<-colorFactor(colors,CountryVaccinations$people_vaccinated)
        
        leaflet()%>%addProviderTiles(provider = "Esri.NatGeoWorldMap")%>%addCircleMarkers(data=CountryVaccinations,lat = ~lat,lng = ~long,radius = ~6,popup = ~popup_info,color = ~pal(people_vaccinated),stroke = FALSE,fillOpacity = 1.5)
        
        
        
        
        
        
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)





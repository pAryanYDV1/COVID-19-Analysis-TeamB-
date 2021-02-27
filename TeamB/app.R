library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(ggthemes)
library(readxl)
library(corrplot)
library(ggcorrplot)
library(forecast)
library(xts)
library(reshape2)

dailyVaccinations <- read.csv("~/Data Science with R (SOFTANBEES)/Country Vaccines/Daily Vaccinations/dailyVaccinations.csv")

vaccination_1.3 <- read.csv("~/Data Science with R (SOFTANBEES)/Country Vaccination/country_vaccinations.csv",header = T)


CountryVaccinations <- read_excel("~/Data Science with R (SOFTANBEES)/Country Vaccines/CountryVaccinations.xlsx")







Protein_Supply_Quantity_Data <- read.csv("~/Data Science with R (SOFTANBEES)/Covid 19 Healthy Diet/Protein_Supply_Quantity_Data.csv")

Food_Supply_kcal_Data <- read.csv("~/Data Science with R (SOFTANBEES)/Covid 19 Healthy Diet/Food_Supply_kcal_Data.csv")

Fat_Supply_Quantity_Data <- read.csv("~/Data Science with R (SOFTANBEES)/Covid 19 Healthy Diet/Fat_Supply_Quantity_Data.csv")


protein_diet <- na.omit(Protein_Supply_Quantity_Data)

fat_diet <- na.omit(Fat_Supply_Quantity_Data)
food_diet <- na.omit(Food_Supply_kcal_Data)



tat <- protein_diet[,-c(1,26,31,32)]
names(tat)[25] <- c("Confirmed Cases")
tat <- as.matrix(tat)
tat<- round(cor(tat),2)
tac <- as.data.frame(tat)


tat1 <- fat_diet[,-c(1,26,31,32)]
names(tat1)[25] <- c("Confirmed Cases")
tat1 <- as.matrix(tat1)
tat1 <- round(cor(tat1),2)
tac1  <- as.data.frame(tat1)


tat2 <- food_diet[,-c(1,26,31,32)]
names(tat2)[25] <- c("Confirmed Cases")
tat2 <- as.matrix(tat2)
tat2 <- round(cor(tat2),2)
tac2 <- as.data.frame(tat2)



diet=rbind(protein_diet,fat_diet)
diet=rbind(diet,food_diet)
diet[,25:30]=sapply(diet[,25:30],FUN=as.numeric)
diet=na.omit(diet)





ui <- shinyUI(
    dashboardPage(skin="black",
                  dashboardHeader(title="ANALYSIS ON COVID-19",titleWidth=320),
                  dashboardSidebar(width=320,
                                   sidebarMenu(
                                       menuItem(strong("1) Study on Country Vaccinations"),tabName = "one"),
                                       menuSubItem("Geographical Distribution of COVID-19 Vaccines",tabName="first",icon = icon("project-diagram")),
                                       menuSubItem("Plot of Daily Vaccination Amount",tabName="second",icon = icon("chart-line")),
                                       menuSubItem("Prediction on Daily Vaccination",tabName="third",icon = icon("hourglass-start")),
                                       menuItem(strong("2) Study on COVID-19 Healthy Diet"),tabName = "two"),
                                       menuSubItem("Correlation Analysis",tabName="fourth",icon = icon("paperclip")),
                                       menuSubItem("Relationship of Foods with Consequences",tabName="fifth",icon = icon("bacon"))
                                   )
                  ),
                  
                  
                  
                  
                  dashboardBody(
                      tabItems(
                          tabItem(tabName = "one",tags$h2("PROGRESS OF COVID-19 VACCINATION",align = "center") , img(src="image1.jpg",height = 500,width = 900,align = "center")),
                          tabItem(
                              tabName="first",h3("MAP VIEW FOR NUMBER OF PEOPLE VACCINATED & NAME OF THE VACCINES DISTRIBUTED BY THE COUNTRIES AROUND THE GLOBE"),
                              fluidRow(
                                  box(solidHeader = T,background = "navy",tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations/locations.csv", "COVID-19 Data Repository", target="_blank"),
                                      h5("All data are aggregated by people vaccinated and people fully vaccinated.The repository contains information about vaccine update from the official sites of different country.Likewise, it also reflects name of vaccines distributed by the different countries along with their ISO Code"),
                                      selectInput("name",
                                                  h4("Select the Name of Country:"), 
                                                  choices= unique(CountryVaccinations$country))
                                   ),
                                  
                                  box(solidHeader = TRUE,background = "olive" ,h4("Table Showing Vaccination Information Of Selected Country"),tableOutput("data"),width=8),
                                  
                                  box(background = "orange" ,width = 12,tabsetPanel(  
                                      tabPanel("Geographical Distribution Of Vaccines", leafletOutput("people_vaccinated"))))
                                    )),
                          
                          
                          
                          
                          
                          
                          tabItem(tabName="second",h3("VIEW OF ANALYSIS DONE ON DAILY VACCINATION AMOUNT IN DIFFERENT COUNTRIES"),
                                                      fluidRow(
                                                          box(solidHeader = F,background = "blue",
                                                              selectInput("con",
                                                                          h4("Select the Name of Country:"),
                                                                          choices=unique(dailyVaccinations$country))
                                                     ),
                                                          
                                                    box(solidHeader=F,background = "green",plotOutput("lineplot"),width = 12)
                                                          
                                                   )   
                                                 ),
                          
                          
                          
                               
                          
                               tabItem(tabName="third",h3("PREDICTION OF TOTAL AMOUNT OF VACCINATION WITH MARCH 30,2021 BY THE CURRENT RATE OF DAILY VACCINATION"),
                                  fluidRow(
                                      box(background = "navy", solidHeader = TRUE,
                                          selectInput("choice",
                                                      h4("Select the Name of Country:"), 
                                                      choices= unique(vaccination_1.3$country)),
                                          
                                          dateInput("date",h4("Choose the Prediction Date"), value = "2021-03-30")
                                      ),
                                      box(background = "red" ,width = 12,tabsetPanel(
                                          tabPanel(" According To The Current Rate Of Daily Vaccination Predicting How Many Will Be Vaccinated In Total With March 30, 2021", plotOutput("arimaplot")))),
                                      box(background = "aqua" , width=8 , tabsetPanel(
                                          tabPanel(textOutput("pred_1.3")))   
                                      
                                  )
                                  )),
                          
                          
                          
                          
                          
                          
                          tabItem(tabName = "two",tags$h2("COVID-19 HEALTHY DIET",align = "center") , img(src="image2.jpg",height = 500,width = 900, align = "center")),
                         
                          
                          
                          
                          tabItem(tabName="fourth",h3("FINDING FEATURES THAT ARE CORRELATED WITH CONFIRMED CASES"),
                                  fluidRow(
                                      box(solidHeader = T,background = "orange",
                                          selectInput("features",h4("Select Any One Feature From Protein Diet:"),choices = colnames(tac)
                                          ),
                                          
                                          
                                          selectInput("features1",h4("Select Any One Feature From Fat Diet:"),choices = colnames(tac1)
                                          ),
                                          
                                          
                                          selectInput("features2",h4("Select Any One Feature From Food Diet:"),choices = colnames(tac2)
                                          )
                                          
                                      ),
                                      
                                      
                                     box(background = "navy",h4("Table Showing Correlation Coefficient For Selected Feature From Protein Diet & Confirmed Cases"),tableOutput("datatable")),
                                     
                                     
                                     
                                     box(background = "red",h4("Table Showing Correlation Coefficient For Selected Feature From Fat Diet & Confirmed Cases"),tableOutput("datatableA")),  
                                         
                                         
                                     box(background = "blue",h4("Table Showing Correlation Coefficient For Selected Feature From Food Diet & Confirmed Cases"),tableOutput("datatableB")), 
                                     
                                          
                                     box(background = "black",width = 12,h3("Correlogram"),tabsetPanel(type='tabs',tabPanel("Protein Diet",plotOutput("corplot")),tabPanel("Fat Diet",plotOutput("corplot1")),tabPanel("Food Diet",plotOutput("corplot2"))
                                                                                                      
                                                                                                      
                                       ))
                                       )),
                          
                          
                          
                          
                          
                          tabItem(tabName="fifth",h3("SEARCHING WHICH TYPE OF FOODS ARE RESPONSIBLE FOR WHAT KIND OF CONSEQUENCES"),
                                  
                                  fluidRow(
                                      box(solidHeader = TRUE,width= 3 ,background = "green",
                                          selectInput("choices",h4("Select the Effect of Food Intake"),
                                                      colnames(diet[,25:30]))),
                                      box(background = "red" ,width = 12,paste(" Following Type Of Foods(Multiple Foods) Are Responsible For The Consequence Along With The Correlation Coefficient")),
                                      box(background = "navy" , width=8 ,h3("Correlation Table"),tableOutput("cor_table"))  
                                  
                                  
                           )
                      )))))






server <- function(input,output){
    
    
    
    #Server part for 1.1
    
        output$data <- renderTable({
        countryFilter <- subset(CountryVaccinations,CountryVaccinations$country==input$name)
        
        
    })
    
    output$people_vaccinated <- renderLeaflet({
        CountryVaccinations<-CountryVaccinations%>%mutate(popup_info=paste("Name of Country:",country,"<br/>","ISO_Code:",iso_code,"<br/>","Name of the Vaccines Distributed:",vaccines,"<br/>","Amount of People Vaccinated:",people_vaccinated,"<br/>","Amount of People Fully Vaccinated:",people_fully_vaccinated))
        CountryVaccinations$popup_info
        colors<-c("red","navy")
        pal<-colorBin(palette = colors,CountryVaccinations$people_vaccinated)
        leaflet()%>%addProviderTiles(provider = "Esri.NatGeoWorldMap")%>%addCircleMarkers(data=CountryVaccinations,lat = ~lat,lng = ~long,radius = ~6,popup = ~popup_info,color = ~pal(people_vaccinated),stroke = FALSE,fillOpacity = 1.5)%>% addLegend(position = "bottomright",pal = pal,values = CountryVaccinations$people_vaccinated, title = "AMOUNT OF PEOPLE VACCINATED" , opacity = 1)
    })
    
     
    
    
    
    
    #Server part for 1.2
    
    
        output$lineplot <- renderPlot({
        countryFilter <- subset(dailyVaccinations,dailyVaccinations$country==input$con)
        
        
        
        
        
        ggplot(countryFilter,aes(x=date))+geom_line(aes(y=daily_vaccinations),group=1,color="blue",size=2,alpha=1)+theme_grey()+theme(axis.text.x = element_text(angle = 55,hjust = 1))+theme(axis.text.y = element_text(angle = 40,vjust = 0))+labs(title=paste("Line Chart Showing Daily Vaccination Done On Different Dates In", input$con))+labs(x="Dates",y="Daily Vaccination Amount")+geom_line(aes(y=daily_vaccinations_per_million),group=2,color="red",size=2,alpha=1)+geom_point(aes(y=daily_vaccinations),color="yellow")+geom_point(aes(y=daily_vaccinations_per_million),color="black")+labs(caption = "INDEX: Blue line(Yellow dots)=Daily Vaccinations / Red line(Black dots)=Daily Vaccinations Per Million")
        
    })
    
        
    
        
    #Server part for 1.3
     
        output$arimaplot<-renderPlot({
        vaccination_1.3<- subset(vaccination_1.3,vaccination_1.3$country==input$choice)
        vaccination_1.3 <- na.omit(vaccination_1.3[c("country","date", "daily_vaccinations")])
        vaccination_1.3$date <- as.Date (vaccination_1.3$date,format="%Y-%m-%d")
        vaccination_1.3<-vaccination_1.3[order(vaccination_1.3$date),]
        
        ds_ts<-ts(vaccination_1.3)
        fit<-auto.arima(ds_ts[,3])
        dat=(as.Date(input$date) - vaccination_1.3[nrow(vaccination_1.3),2])
        forecast1=forecast(fit,h=dat)
        plot(forecast1,xaxt= 'n',xlab = "Date",ylab = "Daily Vaccinations")
        axis(1,at=1:nrow(ds_ts),labels=vaccination_1.3$date,col.axis="blue")
        
        output$pred_1.3 <- renderText({
            paste("Predicted Value For The Total Vaccinations With March 30, 2021 Is",sum(forecast1$fitted,vaccination_1.3[,2]))
        }
        )
        #accuracy(fit)#mape=8.77,92%accurate
    })
        
    
    
  
    
 #Server part for 2.1  
    
    output$datatable <- renderTable({
        tablefilter= tac[25,which(input$features==colnames(tac))]
    })
    
    
    output$datatableA <- renderTable({
        tablefilterA= tac1[25,which(input$features1==colnames(tac1))]
    })
    
    
    output$datatableB <- renderTable({
        tablefilterB= tac2[25,which(input$features2==colnames(tac2))]
    })
    
    cas <- protein_diet[,-c(1,26,31,32)]
    names(cas)[25] <- c("Confirmed Cases")
    cap <- as.matrix(cas)
    
    cas1 <- fat_diet[,-c(1,26,31,32)]
    names(cas1)[25] <- c("Confirmed Cases")
    cap1 <- as.matrix(cas1)
    
    
    
    cas2 <- food_diet[,-c(1,26,31,32)]
    names(cas2)[25] <- c("Confirmed Cases")
    cap2 <- as.matrix(cas2)  
    
    
    output$corplot <- renderPlot({
        
        
        
        cap <- round(cor(cas,method = "pearson",use="complete.obs"),2)
        
        ggcorrplot(cor(cap)[1:28,25,drop=F],method = "circle",tl.col = "black",tl.srt = 90,colors = c("orange","white","navy"))
        
        
        
        
        
        
    })
    
    
    output$corplot1 <- renderPlot({
        
        
        
        
        
        
        cap1 <- round(cor(cas1,method = "pearson",use="complete.obs"),2)
        
        ggcorrplot(cor(cap1)[1:28,25,drop=F],method = "circle",tl.col = "black",tl.srt = 90,colors = c("green","white","red"))
        
        
        
        
        
        
    })
    
    
    
    output$corplot2 <- renderPlot({
        
        
        
        
        
        
        cap2 <- round(cor(cas2,method = "pearson",use="complete.obs"),2)
        
        ggcorrplot(cor(cap2)[1:28,25,drop=F],method = "circle",tl.col = "black",tl.srt = 90,colors = c("yellow","white","blue"))
        
        
        
        
        
        
    })
    
    
   
        
       #Server part for 2.2
            
            output$cor_table<- renderTable({
            effect=diet[,which(input$choices==colnames(diet))]
            cr=cor(diet[,2:24],effect)
            cr=subset(melt(cr),value>0.3)
            cr=cr[,-2]
            cr
            rename(cr,c("Types Of Food"="Var1","Correlation Coefficient"="value"))
        
            
        })
        }

# Run the application 
shinyApp(ui = ui, server = server)


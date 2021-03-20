#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
#          dev.off()
#      } # code if it shows pdf full

#----------------------------------------------------------------------

#Shiny app gives a SNAPSHOT for distress calls in New YOrk State
#Number of datasets = 1
#Number of Plots = 2
#Number of Maps = 2
#Number of tabs = 4
#Number of inputs = 5
#Info boxes = 3
#Data table = 1
#Download button = 1


## Load libraries
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(dashboardthemes)
library(lubridate)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readxl)
library(stringr)

# Load and clean data from API----------------------------------------------
data_311 <- fromJSON("https://data.cityofnewyork.us/resource/erm2-nwe9.json")

#identify the columns
data_311 <- data_311[,c("city", "park_borough", "latitude", "longitude", "created_date", "agency",
                        "agency_name", "descriptor", "open_data_channel_type", "incident_zip", "status",
                        "complaint_type", "location_type")]

#convert char to date
data_311$date <- as.Date(data_311$created_date)

#calculate if its today
data_311$is.today <- today() - as.Date(data_311$created_date)
data_311$year <- year(data_311$date)

#convert longitude and latitudes to numeric
data_311$latitude <- as.numeric(data_311$latitude)
data_311$longitude <- as.numeric(data_311$longitude)

#total cities
cities <- unique(data_311$city)

#total agencies
agencies <- unique(data_311$agency_name)



# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "311 Calls in New York State",
                          
                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "call 311!", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "EV lists")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "Arun",
                                           message = HTML("311 here <br> be Safe!."),
                                           icon = icon("exclamation-circle"))
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    
    sidebarMenu(
        id = "tabs",
        # Menu Items ----------------------------------------------
        menuItem("City wise calls", icon = icon("phone"), tabName = "city"),
        
        menuItem("Department Wise Channels", icon = icon("car"), tabName = "deptchannel"),
        
        menuItem("DATA Table 311", icon = icon("table"), tabName = "table_311"), #badgeLabel = "new", badgeColor = "green"),
        
        menuItem("MAP", icon = icon("map"), tabName = "map"),
        
        br(), # break for better visibility
        
        #INputs: county selected
        selectInput("city_select",
                    "Select City to get Top 311 Complaints:",
                    choices = cities,
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = "NEW YORK"),
        
        #input: Fuel type wise vehicles sold
        selectInput("agency_select",
                    "Select Agency",
                    choices = agencies,
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = "Department of Health and Mental Hygiene"),
        
        # top x Selection ----------------------------------------------
        sliderInput("topSelect",
                    "How many top reasons for call do you want to see?",
                    min = 5,
                    max = 25,
                    value = 5,
                    step = 1), 
        checkboxInput("layer1", "Layer1", value = FALSE, width = NULL),
        checkboxInput("layer2", "Layer2", value = FALSE, width = NULL),
        
        br(),
        br(),
        br(),
        
        # Write sampled data as csv ------------------------------------------
        actionButton(inputId = "write_csv", 
                     label = "Write CSV")
        
    )
)


# Dashboard body ----------------------------------------------
body <- dashboardBody(shinyDashboardThemes(theme = "blue_gradient"), # add blue_gradient theme
                      
                      
                      # Input and Value Boxes ----------------------------------------------
                      fluidRow(
                          infoBoxOutput("TotalCalls"),
                          valueBoxOutput("YesterdayCalls"),
                          infoBoxOutput("TodayCalls")
                          
                      ),
                      tabItems(
                          # Plot page ----------------------------------------------
                          tabItem("city",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "City Wise calls: Status across NY",
                                             width = 15,
                                             tabPanel("Top Complaints for City", plotlyOutput("plot_city"))
                                             #tabPanel("Inequity within charger distribution", plotlyOutput("plot_char")))
                                  ))
                          ),

                          # Data Table Page ----------------------------------------------
                          tabItem("table_311",
                                  fluidPage(
                                      box(title = "Call details for the selected city", DT::dataTableOutput("table_311"), width = 12))
                          ),
                          
                          ## tab item new vehicle
                          tabItem("deptchannel",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Department wise open channels",
                                             width = 15,
                                             tabPanel("Channel wise", plotlyOutput("plot_dept"))
                                             #tabPanel("Fuel wise trend", plotlyOutput("plot_facet")))
                                  ))
                          ),
                          
                          tabItem("map",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Heat Map for calls for selected City",
                                             width = 15,
                                             tabPanel("MAP for selected layers", leafletOutput("leaflet"))
                                             #tabPanel("Fuel wise trend", plotlyOutput("plot_facet")))
                                     
                                         #     # Using Shiny JS
                                         #     shinyjs::useShinyjs(),
                                         #     # Style the background and change the page
                                         #     tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         # body {background-color: #D4EFDF;}"),
                                         #     # Map Output
                                         #     leafletOutput("leaflet")
                                             ))
                          )
                          # ## tab items total sales
                          # tabItem("sale",
                          #         # Plot ----------------------------------------------
                          #         fluidRow(
                          #             tabBox(title = "Total Electric Vehicle Sales",
                          #                    width = 12,
                          #                    tabPanel("Sales trend for a County", plotlyOutput("sales_county")),
                          #                    #tabPanel("A2", plotlyOutput("plot_char"))),
                          #                    tabPanel("Fuel Type wise distribution", plotlyOutput("pie")))
                          #         )
                          # )
                          
                      )
)

#create ui
ui <- dashboardPage( header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    
    # Reactive data function -------------------------------------------
    #select by charger type
    cityInput <- reactive({
        city_filter <- filter(data_311, city == input$city_select )
        # Return dataframe ----------------------------------------------
        return(city_filter)
    })
    
    # Reactive data function -------------------------------------------
    # select by county
    deptInput <- reactive({
        ev1 <- filter(data_311, agency_name == input$agency_select)
        # Return dataframe ----------------------------------------------
        return(ev1) #
    })
    
    
    # Basic Map
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            # addTiles(group = "OSM (default)") %>%
            addProviderTiles("Stamen.Toner", group = "Toner") %>%
            addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
            setView(-74.0060, 40.7128, 9) %>%
            # Layers control
            addLayersControl(
                baseGroups = c("Toner", "Toner Lite"),
                options = layersControlOptions(collapsed = FALSE))
    })
    

    
    # Green Infrastructure Filtered data
    observe({
        if (input$layer1 == TRUE){
    # Replace layer with filtered greenInfrastructure
    observe({
        city_data <- cityInput()
        # Data is greenInf
        new_Data <- city_data[!is.na(city_data$latitude) & !is.na(city_data$latitude),]
        leafletProxy("leaflet", data = new_Data) %>%
            addProviderTiles("CartoDB.DarkMatter") %>%
            clearGroup(group = "new_Data") %>% 
            addHeatmap(lng = ~longitude, lat = ~latitude, radius = 8) %>% 
            setView(lng = new_Data$longitude[1], lat = new_Data$latitude[1], zoom = 12)
        
    })
        }
    })
    
    observe({
        if (input$layer2 == TRUE){
    observe({
    pal311 <- colorFactor(c("#d73027", "#1a9850", "#CC79A7", "#D55E00"), c("ONLINE", "MOBILE", "PHONE", "UNKNOWN"))
    city_data <- cityInput()
    # Data is greenInf
    new_Data <- city_data[!is.na(city_data$latitude) & !is.na(city_data$latitude),]
    leafletProxy("leaflet", data = new_Data) %>% 
            addProviderTiles("OpenStreetMap.HOT") %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 1.5, color = ~pal311(open_data_channel_type), clusterOptions = markerClusterOptions()) %>%
        addLegend(position = "topright" , pal = pal311, values = dat311$open_data_channel_type, title = "Channel Type")
    })
        }
    })

    
    # A plot showing the chargers with city for top selected -----------------------------
    output$plot_city <- renderPlotly({
        dat <- cityInput() #call data
        # Generate Plot ----------------------------------------------
        top_howmany <- input$topSelect
        
        complaint <- dat %>% 
            group_by(complaint_type) %>% 
            summarise(count = n())
        
        complaint <- complaint[order(-complaint$count),]
        
        top_complaint <- complaint[1:input$topSelect,]
        
        ggplot(complaint[1:input$topSelect,], aes(x =  complaint_type, y = count, fill = complaint_type ))+
            geom_bar(postion= "dodge", stat="identity")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("City in CA") + ylab("Numbers of charging stations")
        

    })
    
    # A plot showing the sales for county -----------------------------
    output$plot_dept <- renderPlotly({
        dat <- deptInput()
        
        #location_data <- data[data$agency_name== input$agency,]
        
        #plot bar plot for top 10 complaint type
        agency_data <- dat %>% 
            group_by(open_data_channel_type, status) %>% 
            summarise(count = n())
        
        ggplot(agency_data, aes(x =  open_data_channel_type, y = count, fill = open_data_channel_type ))+
            geom_bar(postion= "dodge", stat="identity")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("Year") + ylab("Numbers of Electric Vehicles")+
            ggtitle(paste("Year wise sales for", input$county))+
            facet_wrap(.~status)
        
    })
    
    
    # Data table of chargers ----------------------------------------------
    output$table_311 <- DT::renderDataTable({
        data_311
    })
    
    # charger level info box ----------------------------------------------
    output$TotalCalls <- renderInfoBox({
        dat <- cityInput()
        num <- nrow(dat)
        # num <- round(sum(sw[,input$chargeLevel], na.rm = T), 2)
        
        infoBox("Total Calls", value = num, subtitle = paste0("City:", input$city), icon = icon("phone"), color = "purple")
    })
    # vehicle population for fuel type value box ----------------------------------------------
    output$YesterdayCalls<- renderValueBox({
        dat <- cityInput()
        dat <- dat[dat$is.today == 1,]
        num <- nrow(dat)
        # num <- sum(sw$vehicles_population, na.rm = T)
        
        valueBox(subtitle = paste0("Yesterday's Calls:", input$city), value = num, icon = icon("person-booth"))
    })
    
    # Sales per county level info box ----------------------------------------------
    output$TodayCalls <- renderInfoBox({
        dat <- cityInput()
        dat <- dat[dat$is.today == 0,]
        num <- nrow(dat)
        # num <- sum(sw$total_sales , na.rm = T)
        
        infoBox("Today's Calls", value = num, subtitle = paste0("City:", input$city), icon = icon("daily-motion"), color = "purple")
    })
    
  
    
    # Write sampled data as csv ---------------------------------------
    observeEvent(eventExpr = input$write_csv, 
                 handlerExpr = {
                     filename <- paste0("Chargercounties", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")
                     write.csv(data_311, file = filename, row.names = FALSE) 
                 }
    )
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
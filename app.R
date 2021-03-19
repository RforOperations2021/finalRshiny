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

#Shiny app gives a high level view for the status of ELectric Vehicles and its associated infrastructure in CA
#Number of datasets = 3
#Number of Plots = 6
#Number of Menu items = 4
#Number of tabs = 6
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

# Load and clean data ----------------------------------------------
data_311 <- fromJSON("https://data.cityofnewyork.us/resource/erm2-nwe9.json")

data_311 <- data_311[,c("city", "park_borough", "latitude", "longitude", "created_date", "agency",
                        "agency_name", "descriptor", "open_data_channel_type", "incident_zip", "status",
                        "complaint_type", "location_type")]

data_311$date <- as.Date(data_311$created_date)
data_311$is.today <- today() - as.Date(data_311$created_date)
data_311$year <- year(data_311$date)

cities <- unique(data_311$city)
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
                                           message = HTML("311 here <br> Go Green!."),
                                           icon = icon("exclamation-circle"))
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    
    sidebarMenu(
        id = "tabs",
        # Menu Items ----------------------------------------------
        menuItem("City wise calls", icon = icon("phone"), tabName = "city"),
        
        menuItem("Department Wise Open Channels", icon = icon("car"), tabName = "deptchannel"),
        
        menuItem("DATA Table 311", icon = icon("table"), tabName = "table_311"), #badgeLabel = "new", badgeColor = "green"),
        
        br(), # break for better visibility
        
        #INputs: county selected
        selectInput("city_select",
                    "Select City to get Top 311 Complaints:",
                    choices = cities,
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "NEW YORK"),
        
        #input: Fuel type wise vehicles sold
        selectInput("agency_select",
                    "Select Agency to Track Incoming Channels",
                    choices = agencies,
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "Department of Health and Mental Hygiene"),
        
        # top x Selection ----------------------------------------------
        sliderInput("topSelect",
                    "How many top reasons for call do you want to see?",
                    min = 5,
                    max = 25,
                    value = 5,
                    step = 1), 
        
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
                          infoBoxOutput("TodayCalls"),
                          valueBoxOutput("YesterdayCalls")
                      ),
                      tabItems(
                          # Plot page ----------------------------------------------
                          tabItem("city",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "City Wise calls: Status across NY",
                                             width = 12,
                                             tabPanel("Complaints for City", plotlyOutput("plot_city"))
                                             #tabPanel("Inequity within charger distribution", plotlyOutput("plot_char")))
                                  ))
                          ),

                          # Data Table Page ----------------------------------------------
                          tabItem("table_311",
                                  fluidPage(
                                      box(title = "List of level-wise Chargers", DT::dataTableOutput("table_311"), width = 12))
                          ),
                          
                          ## tab item new vehicle
                          tabItem("deptchannel",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Fueltype wise Total Vehicle Population",
                                             width = 12,
                                             tabPanel("Yearly trend for vehicles", plotlyOutput("plot_dept"))
                                             #tabPanel("Fuel wise trend", plotlyOutput("plot_facet")))
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
    
    
    
    # A plot showing the chargers with city for top selected -----------------------------
    output$plot_city <- renderPlotly({
        dat <- cityInput() #call data
        # Generate Plot ----------------------------------------------
        top_howmany <- input$topSelect
        
        complaint <- dat %>% 
            group_by(complaint_type) %>% 
            summarise(count = n())
        
        complaint <- complaint[order(-complaint$count),]
        
        top_complaint <- complaint[1:top_howmany,]
        
        ggplot(top_complaint, aes(x =  complaint_type, y = count, fill = complaint_type ))+
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
            ggtitle(paste("Year wise sales for", input$county))
        
    })
    
    
    # Data table of chargers ----------------------------------------------
    output$table_311 <- DT::renderDataTable({
        data_311
    })
    
    # charger level info box ----------------------------------------------
    output$TotalCalls <- renderInfoBox({
        num <- nrow(data_311)
        # num <- round(sum(sw[,input$chargeLevel], na.rm = T), 2)
        
        infoBox("Chargers", value = num, subtitle = paste("Charger Type:", input$city), icon = icon("battery-half"), color = "purple")
    })
    
    # Sales per county level info box ----------------------------------------------
    output$TodayCalls <- renderInfoBox({
        num <- nrow(data_311)
        # num <- sum(sw$total_sales , na.rm = T)
        
        infoBox("EV vehicles", value = num, subtitle = paste("County:", input$city), icon = icon("car"), color = "purple")
    })
    
    # vehicle population for fuel type value box ----------------------------------------------
    output$YesterdayCalls<- renderValueBox({
        num <- nrow(data_311)
        # num <- sum(sw$vehicles_population, na.rm = T)
        
        valueBox(subtitle = paste("Vehicles by Fuel Type in CA:", input$city), value = num, icon = icon("truck"))
    })
    
    # Write sampled data as csv ---------------------------------------
    observeEvent(eventExpr = input$write_csv, 
                 handlerExpr = {
                     filename <- paste0("Chargercounties", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")
                     write.csv(data311, file = filename, row.names = FALSE) 
                 }
    )
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
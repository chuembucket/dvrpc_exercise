#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinylive)
library(DT)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DVRPC Crash Data by Municipality, 2008 - 2020"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tags$a(href="https://chuembucket.github.io/", 
                 "Return to main website"),
          
          column(
            width = 3,
            br(),
            actionButton(
              "select_all_rows_button",
              "Select All Table Rows"
            ),
            br(),
            actionButton(
              "clear_rows_button",
              "Clear Table Selections"
            )
          ),
            
          leafletOutput('map', width = "100%", height = 400)
          ),

        # Show a plot of the generated distribution
        mainPanel(
          
            column(9, DTOutput('tbl')),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## download data on app refresh
  crash_data <- read_csv("https://catalog.dvrpc.org/dataset/916c9fc4-c0c9-4d70-98f6-bd7f76e594b1/resource/ce75c010-3a79-4a67-b7b6-0e16fb83edaf/download/crash_summary_08_20.csv")
  mcd_sf <- read_sf("https://arcgis.dvrpc.org/portal/rest/services/Demographics/Forecast_2015to2050_MCD/FeatureServer/0/query?where=1=1&outsr=4326&outfields=*&f=geojson")
  
  mcd_sf <- mcd_sf %>% mutate(
    geoid = geoid %>% as.numeric()
  )
  
  
  ## group by muni and generate summary statistics 
  years <- unique(crash_data$`Crash Year`)
  
  tab1 <- crash_data %>% group_by(`MCD Name`, `GEOID10`) %>%
    summarise(avg_annual_crashes = round(mean(`TOTAL CRASH`),0),
              min = paste0(min(`TOTAL CRASH`),
                           " (",
                           years[which.min(`TOTAL CRASH`)],
                           ")"),
              max = paste0(max(`TOTAL CRASH`),
                           " (",
                           years[which.max(`TOTAL CRASH`)],
                           ")")
    ) %>% ungroup()
  
  
  ## give geometry, calc per capita stats, and prepare for shiny
  muni_table <- left_join(tab1, mcd_sf %>% select(geoid, pop20),
                          by =c("GEOID10"="geoid")) %>% st_as_sf() %>%
    mutate(
    id = row_number(),
    avg_annual_crash_per_1000 = round(1000*(avg_annual_crashes/pop20),0)
  )  
  
  
    muni_table1 <- reactive({ muni_table })
    pal<-colorNumeric("viridis", muni_table$avg_annual_crashes)

    
    ### TABLE CODE
    output$tbl <- renderDT({
      muni_table1() %>% st_drop_geometry() %>%
        select(-c('GEOID10', 'id')) %>% datatable()
      
    })
    
      
    ### MAP Init
    output$map <- renderLeaflet({
      
      leaflet() %>% addProviderTiles(providers$CartoDB.Voyager) %>%
        setView(lng = -75.161802, lat = 39.957673, zoom = 11)
    })
    
    ### Observe muni select in table
    observeEvent(input$tbl_rows_selected, {
      
      #get selected rows
      selected_munis <- eventReactive(input$tbl_rows_selected, {
        muni_table1() %>% filter(id %in% input$tbl_rows_selected)
      })
      
      
      #add selected rows to map
      leafletProxy("map") %>% 
            #clear() %>%
            addPolygons(
              data = selected_munis(),
              fillColor = ~pal(selected_munis()$avg_annual_crashes),
              stroke = TRUE,
              color = "white",
              weight = 1,
              fillOpacity = 0.4,
              popup = ~paste0('<strong>',`MCD Name`,'</strong>', "<br>", avg_annual_crashes, " Crashes per 1000 Residents")
            )
    })


    # create a proxy to modify datatable without recreating it completely
    DT_proxy <- dataTableProxy("tbl")
    # clear row selections when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      selectRows(DT_proxy, NULL)
    })

    # clear markers from leaflet when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      clearShapes(leafletProxy("map"))
    })

    # select all rows when select_all_rows_button is clicked
    observeEvent(input$select_all_rows_button, {
      selectRows(DT_proxy, input$tbl_rows_all)
    })

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

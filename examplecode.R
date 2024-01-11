#devtools::install_github("tknoch8/datatableLeafletApp")
require(shiny)
require(leaflet)
require(DT)
require(tidyverse)

shiny::shinyApp(
  ui = fluidPage(
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
    column(
      width = 9,
      fluidRow(
        column(
          width = 12,
          solidHeader = TRUE,
          leafletOutput(
            "my_leaflet"
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          solidHeader = TRUE,
          DTOutput(
            "my_datatable"
          )
        )
      )
    )
  ),
  
  server = function(session, input, output) {
    
    quakes_r <- reactive({ as_tibble(quakes) })
    
    output$my_datatable <- renderDT({
      
      quakes_r() %>% 
        datatable()
      
    })
    
    
    # base map that we will add points to with leafletProxy()
    output$my_leaflet <- renderLeaflet({
      
      leaflet() %>% 
        addProviderTiles(
          provider = providers$CartoDB.Positron,
          options = providerTileOptions(
            noWrap = FALSE
          )
        ) %>% 
        setView(
          lat = -25.5,
          lng = 178.58,
          zoom = 4
        )
      
    })
    
    observeEvent(input$my_datatable_rows_selected, {
      
      selected_lats <- eventReactive(input$my_datatable_rows_selected, {
        as.list(quakes_r()$lat[c(unique(input$my_datatable_rows_selected))])
      })
      
      selected_longs <- eventReactive(input$my_datatable_rows_selected, {
        as.list(quakes_r()$long[c(unique(input$my_datatable_rows_selected))])
      })
      
      selected_depths <- eventReactive(input$my_datatable_rows_selected, {
        as.list(quakes_r()$depth[c(unique(input$my_datatable_rows_selected))])
      })
      
      selected_mags <- eventReactive(input$my_datatable_rows_selected, {
        as.list(quakes_r()$mag[c(unique(input$my_datatable_rows_selected))])
      })
      
      selected_stations <- eventReactive(input$my_datatable_rows_selected, {
        as.list(quakes_r()$stations[c(unique(input$my_datatable_rows_selected))])
      })
      
      # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
      # as well as the popups when the points are hovered over
      map_df <- reactive({
        tibble(lat = unlist(selected_lats()),
               lng = unlist(selected_longs()),
               depth = unlist(selected_depths()),
               mag = unlist(selected_mags()),
               stations = unlist(selected_stations()))
      })
      
      leafletProxy("my_leaflet", session) %>% 
        clearMarkers() %>% 
        addCircleMarkers(
          data = map_df(),
          lng = ~lng,
          lat = ~lat,
          fillColor = "blue",
          stroke = TRUE,
          color = "white",
          radius = 3,
          weight = 1,
          fillOpacity = 0.4,
          popup = paste0("lat: ", map_df()$lat, "<br>",
                         "lng: ", map_df()$lng, "<br>",
                         "depth: ", map_df()$depth, "<br>",
                         "mag: ", map_df()$mag, "<br>",
                         "stations: ", map_df()$stations)
        )
      
    })
    
    # create a proxy to modify datatable without recreating it completely
    DT_proxy <- dataTableProxy("my_datatable")
    
    # clear row selections when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      selectRows(DT_proxy, NULL)
    })
    
    # clear markers from leaflet when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      clearMarkers(leafletProxy("my_leaflet", session))
    })
    
    # select all rows when select_all_rows_button is clicked
    observeEvent(input$select_all_rows_button, {
      selectRows(DT_proxy, input$my_datatable_rows_all)
    })
    
  }
)

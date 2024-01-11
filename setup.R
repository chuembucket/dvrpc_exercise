#data gathering

##packages
library(tidyverse)
library(sf)
# library(tidycensus)
# library(tigris)
library(mapview)
library(DT)
library(leaflet)
library(htmltools)


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
  )


## give geometr, calc per capita stats, and prepare for shiny
muni_table <- left_join(tab1, mcd_sf %>% select(geoid, pop20), by =c("GEOID10"="geoid")) %>% st_as_sf()

muni_table <- muni_table %>% mutate(
  id = row_number(),
  avg_annual_crash_per_1000 = round(1000*(avg_annual_crashes/pop20),0)
)  %>% ungroup()

# muni_table  %>% st_drop_geometry() %>%
#   select(-c('GEOID10', 'id'))%>% datatable()

## map data
crash_sf <- left_join(crash_data, mcd_sf, by = c("GEOID10"="geoid")) %>% st_as_sf()
crash_types <- c("TOTAL CRASH","FATAL CRASH", "INJURY CRASH","PDO_UNKNOWN CRASH","TOTAL KILLED",
                 "TOTAL INJURED", "TOTAL PERSONS", "PEDESTRIAN COUNT", "PEDESTRIANS KILLED", "BICYCLE COUNT", "BICYCLISTS KILLED")




### end setup


per_capita_cols <- c("TOTAL CRASH","FATAL CRASH", "INJURY CRASH","PDO_UNKNOWN CRASH","TOTAL KILLED",
                     "TOTAL INJURED", "TOTAL PERSONS", "PEDESTRIAN COUNT", "PEDESTRIANS KILLED", "BICYCLE COUNT", "BICYCLISTS KILLED")

crash_pc_sf <- crash_sf %>% mutate_at(per_capita_cols, funs(./pop20))

crash1y <- crash_sf %>% filter(`Crash Year` == 2020)



mapview(crash1y, zcol = "TOTAL CRASH")


## button for year, column, percapita switch






crash_type <- "TOTAL CRASH"


pal<-colorNumeric("viridis", crash1y %>% pull(crash_type))


leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
  addPolygons(
    data = crash1y,
    fillColor =  ~pal(crash1y %>% pull(crash_type)),
    color = "lightgrey",
    weight = 1,
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8
    ),
    popup = ~htmlEscape(`MCD Name`)) %>% 
  addLegend(position = "bottomright", 
            pal = pal, values = (crash1y %>% pull(crash_type)))
            




## provide a series of summary statistics by municipality that could be utilized by the OSS team

### export to shiny live
shinylive::export(appdir = "app", destdir = "docs")



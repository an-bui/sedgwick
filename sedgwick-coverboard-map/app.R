#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(sf)
library(leaflet)

metadata <- read_csv("metadata.csv")

points_burn <- st_read("Burn_points.gpx", layer = "waypoints") %>% 
    select(ele, time, cmt, geometry) 

# control points
points_control <- st_read("Control_points.gpx", layer = "waypoints") %>% 
    select(ele, time, cmt, geometry) 

# burn and control points in one data frame
points_all_app <- rbind(points_burn, points_control) %>% 
    # rename cmt column to board_tag
    rename(board_id = cmt) %>% 
    # separate board_id into components to code in meanings for things
    separate(board_id, into = c("treatment_code", "habitat_code", "number"), sep = 1:3, remove = FALSE) %>% 
    # join by board ID column with metadata
    full_join(., metadata, by = c("board_id" = "Board ID")) %>% 
    # adds underscore and lower case
    clean_names() %>% 
    # create column for flag or board coordinates
    mutate(point_type = case_when(
        habitat_code == "F" ~ "flag",
        habitat_code %in% c("G", "W", "C", "X") ~ "board"
    ),
    point_type = fct_relevel(point_type, c("flag", "board")))

# palette for point colors
pal <- colorFactor(c("yellow", "blue"), domain = c("flag", "board"), ordered = TRUE)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Sedgwick Coverboard Locations"),
    leafletOutput("mymap"),
    p()
)

# map output
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        leaflet() %>% 
            # base map
            addProviderTiles(providers$Esri.WorldImagery) %>%
            # markers: points
            addCircleMarkers(data = points_all_app,
                             color = ~pal(point_type),
                             popup = paste("Board ID:", points_all_app$board_id, "<br>",
                                           "Treatment:", points_all_app$treatment, "<br>",
                                           "Habitat:", points_all_app$habitat, "<br>",
                                           "Notes:", points_all_app$notes, "<br>"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

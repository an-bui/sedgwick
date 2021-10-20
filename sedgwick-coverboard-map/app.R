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
    separate(board_id, into = c("code", "number"), sep = "(?<=[A-Za-z])(?=[0-9])", remove = FALSE) %>% 
    separate(code, into = c("treatment_code", "habitat_code"), sep = 1:2, remove = FALSE) %>% 
    # join by board ID column with metadata
    full_join(., metadata, by = c("board_id" = "Board ID")) %>% 
    # adds underscore and lower case
    clean_names() %>% 
    # create a few new columns
    mutate(
        # new column for point type: either flag or board
        point_type = case_when(
            habitat_code == "F" ~ "flag",
            habitat_code %in% c("G", "W", "C", "X") ~ "board"),
        # new column for the text in the marker pop up
        marker_text = case_when(
            point_type == "flag" ~ paste("Flag:", number, "<br>"),
            point_type == "board" ~ paste("Board ID:", board_id, "<br>",
                                          "Treatment:", treatment, "<br>",
                                          "Habitat:", habitat, "<br>",
                                          "Topography:", topography, "<br>", 
                                          "Heading/distance from nearest flag:", direction_distance, "<br>",
                                          "Notes:", notes, "<br>")
        ))


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
                             popup = ~marker_text)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

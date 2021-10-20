
# 1. libraries ------------------------------------------------------------

library(here) # file organization
library(tidyverse) # manipulating
library(sf) # reading in spatial data, etc.
library(leaflet) # one way to make a map
library(mapview) # another way to make a map
mapviewOptions(fgb = FALSE) # lets the mapview map be interactive
library(googlesheets4) # getting metadata from google sheets
library(janitor) # cleaning variable names

# 2. data -----------------------------------------------------------------
  
sheet_id <- "1rT76k5CKds8XSDIFnwukckh-y-3hO3dpaeTBuhNodVc"

metadata <- read_sheet(sheet_id, na = "N/A") %>% 
  select(-Canopy)

# write_csv(metadata, here::here("sedgwick-coverboard-map", "metadata.csv"))
  
# layers
firebreaks <- st_read(here::here("data/spatial/firebreaksd_rds_unimproved"), layer = "FireBreakRoadsUnimproved") %>% 
    st_transform(., crs = 4269)

maintained_roads <- st_read(here::here("data/spatial/maintained_roads"), layer = "MaintainedRoads") %>% 
  st_transform(., crs = 4269)

sedg_boundary <- st_read(here::here("data/spatial/sedg_boundary"), layer = "sedgwick_boundary_adj") %>% 
  st_transform(., crs = 4269)

firebreaks_swcorner <- st_read(here::here("data/spatial/swcorner_firebreaks_rds"), layer = "SWcorner_FireBreaksRoads") %>% 
  st_transform(., crs = 4269)

# GPS points

# burn points
points_burn <- st_read(here::here("data/spatial", "Burn_points.gpx"), layer = "waypoints") %>% 
  select(ele, time, cmt, geometry) 

# control points
points_control <- st_read(here::here("data/spatial", "Control_points.gpx"), layer = "waypoints") %>% 
  select(ele, time, cmt, geometry) 

# burn and control points in one data frame
points_all <- rbind(points_burn, points_control) %>% 
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

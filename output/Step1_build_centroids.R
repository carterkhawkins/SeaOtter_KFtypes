


rm(list=ls())

################################################################################
# Load packages, data, and set directories

require(librarian)

librarian::shelf(ggplot2, tidyverse, here, readxl, sf, leaflet, rnaturalearth,
                 janitor)

datin <- here::here("output", "raw")
datout <- here::here("output", "processed")

# Read the Excel file
otter_raw <- readxl::read_excel(file.path(datin, "HawkinsOtterData.xlsx")) %>%
                #make tidy
                janitor::clean_names()

mba_raw <- readxl::read_excel(file.path(datin, "SORAC_foraging_data_12102020.xlsx")) %>%
  #make tidy
  janitor::clean_names()

# View the first few rows
head(otter_raw)
str(otter_raw)

################################################################################
# compute centroids

site_centroids <- otter_raw %>%
  group_by(site) %>%
  summarise(
    centroid_lat = mean(cp_lat, na.rm = TRUE),
    centroid_long = mean(cp_long, na.rm = TRUE)
  )

################################################################################
# visualize Hawkins data and MBA data

# Convert site centroids to an sf object
site_centroids_sf <- st_as_sf(site_centroids, coords = c("centroid_long", "centroid_lat"), crs = 4326)

# Convert MBA raw data points to an sf object
mba_points_sf <- st_as_sf(mba_raw, coords = c("long", "lat"), crs = 4326)

# Create an interactive leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
  # Add site centroids (red)
  addCircleMarkers(data = site_centroids_sf, 
                   radius = 6, color = "red", fillOpacity = 0.8, stroke = FALSE,
                   popup = ~site, group = "Centroids") %>%  
  # Add MBA data points (blue)
  addCircleMarkers(data = mba_points_sf, 
                   radius = 3, color = "blue", fillOpacity = 0.5, stroke = FALSE,
                   popup = ~paste("Date:", date, "<br>Prey:", prey, "<br>Number:", number), 
                   group = "MBA Observations") %>%
  # Add toggle
  addLayersControl(
    overlayGroups = c("Centroids", "MBA Observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Center the map on the average coordinates of the data
  setView(lng = mean(site_centroids$centroid_long), 
          lat = mean(site_centroids$centroid_lat), 
          zoom = 10)  


################################################################################
# assign mba_raw points to hawkins centroids

















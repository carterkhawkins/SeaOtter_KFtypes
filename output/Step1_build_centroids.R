


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
# calculate centroids and assign 200m buffer

# Convert site centroids to an sf object
site_centroids_sf <- st_as_sf(site_centroids, coords = c("centroid_long", "centroid_lat"), crs = 4326)

# Convert mba_raw to an sf object
mba_raw_sf <- st_as_sf(mba_raw, coords = c("long", "lat"), crs = 4326)

# Create a 200m buffer around each site centroid
site_buffers <- st_buffer(site_centroids_sf, dist = 200)  # Distance in meters

# Perform spatial join to assign site names to points within the buffer
mba_raw_sf <- st_join(mba_raw_sf, site_buffers["site"], left = TRUE)

# Filter for points that were successfully assigned to a site (inside 200m buffer)
mba_filtered_sf <- mba_raw_sf %>% filter(!is.na(site))

################################################################################
# visualize

# Create the interactive leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  
  
  # Add hawkins site centroids (Red)
  addCircleMarkers(data = site_centroids_sf, 
                   radius = 6, color = "red", fillOpacity = 0.8, stroke = FALSE,
                   popup = ~site, group = "Centroids") %>%
  
  # Add all MBA observations (Blue)
  addCircleMarkers(data = mba_raw_sf, 
                   radius = 3, color = "blue", fillOpacity = 0.5, stroke = FALSE,
                   popup = ~paste("Date:", date, "<br>Prey:", prey, "<br>Number:", number), 
                   group = "All MBA Observations") %>%
  
  # Add filtered MBA observations (Green)
  addCircleMarkers(data = mba_filtered_sf, 
                   radius = 3, color = "orange", fillOpacity = 0.7, stroke = FALSE,
                   popup = ~paste("Site:", site, "<br>Date:", date, "<br>Prey:", prey, "<br>Number:", number), 
                   group = "Filtered MBA Observations") %>%
  
  # Add toggle
  addLayersControl(
    overlayGroups = c("Centroids", "All MBA Observations", "Filtered MBA Observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Center the map on the average coordinates of the data
  setView(lng = mean(site_centroids$centroid_long), 
          lat = mean(site_centroids$centroid_lat), 
          zoom = 10)  

################################################################################
# export

# Convert the sf object back to a data frame
mba_raw_assigned <- as.data.frame(mba_raw_sf)

# Save as a CSV file
write.csv(mba_raw_assigned, file = here::here(datout, "mba_hawkins_merged.csv"), row.names = FALSE) #last write 13 Mar 2025













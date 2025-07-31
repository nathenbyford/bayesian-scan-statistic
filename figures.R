## Nathen Byford
## Figures


# Load packages -----------------------------------------------------------

library(tigris)       # for county shapefiles
library(sf)           # for spatial data handling
library(dplyr)        # for data wrangling
library(spdep)        # for neighbors
library(ggplot2)
library(gganimate)
library(gifski)

# Download Texas counties
tx_counties <- counties(state = "TX", cb = TRUE, year = 2022, class = "sf")

# Compute centroids for each county
centroids <- st_centroid(tx_counties)

# Distance matrix between counties
dist_matrix <- st_distance(centroids)

# Get 4 closest neighbors for each county (excluding self)
get_closest_neighbors <- function(i, k = 4) {
  order(dist_matrix[i, ])[2:(k+1)]  # skip first since it's self
}

closest_neighbors <- lapply(1:nrow(tx_counties), get_closest_neighbors)

# Prepare ID variable
tx_counties$county_id <- 1:nrow(tx_counties)

# Build animation data
highlight_data <- lapply(1:nrow(tx_counties), function(i) {
  neighbors <- closest_neighbors[[i]]
  frame_data <- tx_counties[c(i, neighbors), ]
  frame_data$frame <- i  # animation frame = focal county index
  frame_data$highlight <- ifelse(frame_data$county_id == i, "focal", "neighbor")
  frame_data$county_name <- tx_counties$county_name[i]  # add name for title
  frame_data
})

# Combine into one sf object
anim_data <- do.call(rbind, highlight_data)

p <- ggplot() +
  geom_sf(data = tx_counties, fill = "grey90", color = "white") + # base map
  geom_sf(data = anim_data, aes(fill = highlight), color = "black") +
  scale_fill_manual(values = c("focal" = "forestgreen", "neighbor" = "cornflowerblue")) +
  theme_void() +
  labs(title = "County: {closest_state}") + # optional
  transition_states(frame, transition_length = 1, state_length = 1) +
  ease_aes('cubic-in-out')

# Animate
anim <- animate(p, nframes = nrow(tx_counties), fps = 2, renderer = gifski_renderer())

anim

anim_save("images/texas_scan.gif", animation = anim)

library(tidyverse)
library(readxl)
library(here)
library(ggmap)
library(sf)
library(tidycensus)
library(purrr)

# register google API
register_google(key = "AIzaSyA4mUFdkLO91D--MRPnwjBJlXkACzH6sGU")

# download raw data
farm_raw <- read_xlsx(here("data", "farm_name_city.xlsx"))

# get the lat and long for each city
farm_names <- farm_raw %>% 
  mutate(location = paste0(State, ", ", City)) %>% 
  select(`Farm Name`, State, City, location) %>% 
  mutate(
    geocode_result = map(location, ~ geocode(.x)),   # Geocode each location
    lat = map_dbl(geocode_result, "lat"),             # Extract latitude
    lon = map_dbl(geocode_result, "lon")             # Extract longitude
  ) %>%
  select(`Farm Name`, State, City, location, lat, lon)

# save the geolocations
write.csv(farm_names, here("data/farm_geo.csv"), row.names = FALSE)

#----------------
# 
#----------------

census_api_key("a9eb34d84ff4ca0a2f905d4446b154deb90398c8.", install = TRUE)

farm_geo <- farm_names %>% 
  # remove the 32 cities that are not included
  filter(!is.na(lat) & !is.na(!lon)) %>% 
  mutate(
    # Create the 'geo' column by combining lat and lon into spatial points
    geo = st_as_sf(., coords = c("lon", "lat"), 
                   crs = 4326)  # CRS 4326 is WGS84 (standard for GPS coordinates)
  ) 

farm_geo <- farm_geo %>% 
  st_as_sf(farm_geo$geo, crs = 4326)

states <- unique(farm_geo$State)
# get congressional districts

# Query ACS data for Congressional districts in specific states
congress_data_states <- map_dfr(states, function(state_code) {
  get_acs(
    geography = "congressional district", 
    variables = "B01001_001",  # Total population
    year = 2020,
    state = state_code,  # Use state abbreviation from the vector
    geometry = TRUE
  )
})

farm_geo <- sf::st_transform(farm_geo$geo, crs = congress_data_states)

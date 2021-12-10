rm(list = ls())

require(rvest)
require(xml2)
require(tidyverse)

# Gt a list of the URLS
urls <- c(
  "https://www.jacobs.com/locations/united-states",
  "https://www.jacobs.com/locations/canada",
  "https://www.jacobs.com/locations/caribbean",
  "https://www.jacobs.com/locations/europe", 
  "https://www.jacobs.com/locations/mena",
  "https://www.jacobs.com/locations/asia-pacific"
)

# Download the tables from Jacobs Website
get_table <- 
  function(url){
    url %>% 
      read_html() %>% 
      html_element("table") %>%
      html_table()
  }

tables <- map(urls, get_table)


# Turn the tables into a tibble & drop un-needed rows
text <- tables %>% unlist()
address <- tibble(raw = text)

# Drop un-needed tables
address <- address %>% drop_na() %>% filter(raw != "", nchar(raw) > 50) 


# Parse the address data 
address <- 
  address %>% 
  mutate(office_name = str_extract(raw, "^.+([[:punct:]]|[a-z])([A-Z]|[0-9])"),
         office_name = str_remove(office_name, ".$"), 
         office_name_2 =  raw %>% str_extract("^.+\\n") %>% str_remove_all("\\n"),
         office_name = coalesce(office_name, office_name_2),
         address = raw %>% str_replace_all("\\n|\\t", " ") %>% str_replace_all(" +", " "),
         address = address %>% str_remove_all("(Phone|Fax).+$"),
         address_start =  if_else(is.na(office_name), 0, nchar(office_name) + 1),
         address = address %>% str_sub(start = address_start),
         office_name = office_name %>% trimws(),
         address = address %>% trimws()
  )  %>% select(raw, office_name, address)


# Add Geometry -----
# Geocode the addresses
require(tidygeocoder)
require(sf)
coded <- geo(address = address$address, method = "arcgis")
jacobs <- address %>% bind_cols(coded %>% select(lat, long))

# Convert to SF
jacobs <- jacobs %>% st_as_sf(coords = c( "long", "lat"), crs = 4326)

# Add Country element
require(getarc)

ep_world_countries <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/World_Countries_(Generalized)/FeatureServer/0"
world_countries <- query_layer(endpoint = ep_world_countries, in_geometry = st_union(jacobs))

jacobs <- st_join(jacobs, world_countries)


# Tidy For output -----
jacobs_tidy <- 
  jacobs %>% 
  select(office_name, address, country = COUNTRY, iso = ISO)

jacobs_tidy %>% st_write("jacobs-offices.geojson", delete_dsn = TRUE)

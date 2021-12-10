require(rvest)
require(xml2)
require(tidyverse)
url <- "https://www.jacobs.com/locations/europe"

urls <- c(
  "https://www.jacobs.com/locations/united-states",
  "https://www.jacobs.com/locations/canada",
  "https://www.jacobs.com/locations/caribbean",
  "https://www.jacobs.com/locations/europe", 
  "https://www.jacobs.com/locations/mena",
  "https://www.jacobs.com/locations/asia-pacific"
)

get_table <- 
  function(url){
    url %>% 
      read_html() %>% 
      html_element("table") %>%
      html_table()
  }



address_table <- 
  url %>% 
  read_html() %>% 
  html_element("table") %>%
  html_table()

text <- 
  address_table %>% 
  unlist()


text[36]
address <- tibble(raw = text)

address <- 
  address %>% 
  drop_na() %>% 
  filter(raw != "", nchar(raw) > 50) 


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

require(tidygeocoder)
coded <- geo(address = address$address, method = "arcgis")


# reverse_geo_table <- 
#   tidygeocoder::reverse_geo(lat = coded$lat, 
#                             long = coded$long, 
#                             flatten = FALSE, method = "arcgis")


jacobs <- address %>% bind_cols(coded %>% select(lat, long))

require(sf)
jacobs <- jacobs %>% st_as_sf(coords = c( "long", "lat"), crs = 4326)

require(getarc)

world_countries <- query_layer(endpoint = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/World_Countries_(Generalized)/FeatureServer/0",
                              in_geometry = st_union(jacobs)
                              )


jacobs <- st_join(jacobs, world_countries)


jacobs <- 
  jacobs %>% 
  select(office_name, address, country = COUNTRY, iso = ISO)

jacobs %>% st_write("jacobs-offices.geojson")

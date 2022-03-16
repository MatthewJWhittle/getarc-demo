require(sf)
require(dplyr)

jacobs <- st_read("jacobs-offices.geojson")

us_offices <- jacobs %>% filter(iso == "US")


us_offices %>% st_write("jacobs-us-offices.geojson")

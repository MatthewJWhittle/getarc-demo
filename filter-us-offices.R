require(sf)
require(dplyr)

jacobs <- st_read("jacobs-offices.geojson")

us_offices <- jacobs %>% filter(iso == "US")


us_offices %>% st_write("jacobs-us-offices.geojson")


uk_offices <- jacobs %>% filter(iso == "GB")
uk_offices %>% st_write("uk-offices.geojson")

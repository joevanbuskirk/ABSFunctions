library(httr)
library(sf)
library(tidyverse)

### Download LHD map from Google
### Updated now
lhd.url <- paste0('https://www.google.com/maps/d/',
                  'kml?mid=1Dv1JRTGmzlm83tBv7tb8vQcOQXY&forcekml=1')

httr::GET(lhd.url, write_disk(lhd.tf <- tempfile(fileext = "kml")))

## Merge the two sections of this (metro and regional)
LHD.Map <- dplyr::bind_rows(
  sf::st_read(lhd.tf, layer = sf::st_layers(lhd.tf)$name[1]),
  sf::st_read(lhd.tf, layer = sf::st_layers(lhd.tf)$name[2])) %>% 
  ## Convert merged dataframe into simple features object
  sf::st_as_sf() %>% 
  ## Drop the Z dimension from the kml file - not needed and causes errors
  sf::st_zm() %>% 
  ## Convert to the incoming coordinate reference system (GDA2020)
  sf::st_transform(7844)

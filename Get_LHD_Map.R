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
  sf::st_transform(3577)

LHD.Map <- dplyr::bind_rows(
  LHD.Map[sf::st_is_valid(LHD.Map),],
  LHD.Map[!sf::st_is_valid(LHD.Map), ] %>% 
    lwgeom::st_snap_to_grid(., .10) %>% 
    sf::st_make_valid())


rm(lhd.url)
rm(lhd.tf)



Int_with_SLHD <- function(ASGS, LHD = LHD.Map){
  SLHD <- LHD.Map[LHD.Map$Name == 'Sydney', ]
  SLHD <- sf::st_transform(SLHD, 3577)
  ASGS <- sf::st_transform(ASGS, 3577)
  
  AreaVar <- names(ASGS)[grepl("Area.*sqkm", 
                               names(ASGS), 
                               ignore.case = TRUE)]
  
  ## For SA2s
  SLHD.Intersection <- sf::st_intersection(ASGS, SLHD)
  SLHD.Intersection$AreaSLHD <- as.numeric(sf::st_area(SLHD.Intersection))/1000^2
  SLHD.Intersection <- SLHD.Intersection %>% 
    dplyr::mutate(AreaInPct = AreaSLHD/get(AreaVar)) %>% 
    dplyr::filter(AreaInPct > 0.5)
  
  return(SLHD.Intersection)
}


Int_with_LHD <- function(ASGS, LHD = LHD.Map){
  LHD <- sf::st_transform(LHD, 3577)
  ASGS <- sf::st_transform(ASGS, 3577)
  ASGS <- sf::st_make_valid(ASGS)
  AreaVar <- names(ASGS)[grepl("Area.*sqkm", 
                               names(ASGS), 
                               ignore.case = TRUE)]
  
  ## For SA2s
  LHD.Intersection <- sf::st_intersection(ASGS, LHD)
  LHD.Intersection$AreaLHD <- as.numeric(sf::st_area(LHD.Intersection))/1000^2
  LHD.Intersection <- LHD.Intersection %>% 
    dplyr::mutate(AreaInPct = AreaLHD/get(AreaVar)) %>% 
    dplyr::filter(AreaInPct > 0.5)
  
  return(LHD.Intersection)
}



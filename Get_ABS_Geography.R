## Here's a change

Get_ABS_Geography <- function(ASGS, Year, state = 1){
  library(httr)
  library(sf)
  library(furrr)
  
  print(glue::glue("Downloading {Year} {ASGS} boundaries"))
  
  url <- httr::parse_url(glue::glue(
    "https://geo.abs.gov.au/arcgis/rest/services/ASGS{Year}/",
    "{ASGS}/MapServer/0/query"))
  
  if(Year == 2011){
    where_query <- glue::glue("STATE_CODE='{state}'")
  } else {
    where_query <- glue::glue("STATE_CODE_{Year}='{st}'")
  }
  
  url$query <- list(
    where = where_query,
    outFields = '*',
    featureEncoding = "esriDefault",
    returnGeometry = "true",
    f = "geojson")
  
  request <- httr::build_url(url)
  
  out <- sf::st_read(httr::GET(request),
                     quiet = TRUE)
  
  return(out)
  
}

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




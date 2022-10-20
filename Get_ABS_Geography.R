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
    where_query <- glue::glue("STATE_CODE_{Year}='{state}'")
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



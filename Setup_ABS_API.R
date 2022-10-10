########### ABS Census API Setup ###########
########### Date: 19/09/2022 ###############
library(tidyverse)
library(httr)
library(rsdmx)
library(jsonlite)

url <- 'https://api.data.abs.gov.au'

### This may need to be added later
apikey <- ''

### Get 2021 Assets
res <- httr::GET(url = glue::glue("{url}/dataflow/ABS/?format=jsondata")) 

ABS.Contents <- fromJSON(rawToChar(res$content)) 

Assets <- purrr::map_df(ABS.Contents$references,
                        ~data.frame(id = .$id, name = .$name)) 

####### Make a generic data dictionary function ########
GetDataDict <- function(id){
  url <- 'https://api.data.abs.gov.au'
  
  rs <- rsdmx::readSDMX(
    glue::glue("{url}/datastructure/ABS/{id}?references=codelist"))
  
  ds <- rs@datastructures@datastructures[[1]]
  dims <- purrr::map_chr(ds@Components@Dimensions,
                         slot,
                         name = 'codelist')
  
  out <- purrr::map(setNames(dims, dims),
                    ~as.data.frame(rs@codelists, 
                                   codelistId = .), 
                    .id = 'codelistID')
  return(out)
}


# Assets %>% 
#   dplyr::filter(grepl("C21.*SA2", id),
#                 grepl("Time Series", name),)


Get_ABS_Table <- function(table, args){
  url <- 'https://api.data.abs.gov.au'
  
  args.vec <- paste(unlist(args), collapse = '.')
  
  res2 <- httr::GET(
    url = glue::glue("{url}/data/{table}/{args.vec}?format=csv"), 
    add_headers('x-api-key' = apikey))
  readr::read_csv(file = rawToChar(res2$content))
}



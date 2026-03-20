########### ABS Census API Setup ###########
########### Date: 19/09/2022 ###############
pkgs <- c('tidyverse', 'httr', 'rsdmx', 'jsonlite', 'xml2')
pkgs_chk <- sapply(pkgs, require, 
                   character.only = TRUE,
                   quietly = TRUE)
if(any(!pkgs_chk)){
  lapply(names(pkgs_chk)[!pkgs_chk], install.packages)
}

library(tidyverse)
library(httr)
library(rsdmx)
library(jsonlite)
library(xml2)

### This may need to be added later
### Get 2021 Assets
GetAssets <- function(){
  url <- 'https://api.data.abs.gov.au'
  
  Assets <<- xml2::read_xml(sprintf('%s/dataflow/ABS', url)) %>% 
    xml2::xml_child(2) %>%
    xml2::xml_child() %>% 
    xml2::as_list() %>% 
    purrr::map(function(df){
      if(length(df) == 4){
        data.frame(ID = attr(df$Structure$Ref, 'id'),
                   Name = df$Name[[1]])
      }
    }) %>% 
    purrr::list_rbind()
}


####### Make a generic data dictionary function ########
GetDataDict <- function(id, apikey = ''){
  apikey <- ''
  
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


Get_ABS_Table <- function(table, args, apikey = ''){
  
  url <- 'https://api.data.abs.gov.au'
  
  args.vec <- paste(unlist(args), collapse = '.')
  
  res2 <- httr::GET(
    url = glue::glue("{url}/data/{table}/{args.vec}?format=csv"), 
    add_headers('x-api-key' = apikey))
  readr::read_csv(file = I(rawToChar(res2$content)), 
                  show_col_types = FALSE)
}





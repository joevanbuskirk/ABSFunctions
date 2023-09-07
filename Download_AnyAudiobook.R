library(rvest)
library(xml2)

Download_AnyAudiobook <- function(url, file_prefix, destfolder){
  if(!dir.exists(destfolder)){dir.create(destfolder)}
  
  url_read <- rvest::read_html(url)
  files <- url_read %>% 
    rvest::html_elements(css = 'audio') %>% 
    rvest::html_text()
  
  files <- setNames(files, gsub('.*(\\d{2}).mp3$', "\\1", files))
  
  purrr::iwalk(files, 
               ~download.file(
                 .x, 
                 destfile = file.path(destfolder,
                                      glue::glue('{file_prefix}_{.y}.mp3')), 
                 method = 'curl'))
  
}




#' Download AnyAudiobook
#'
#' Download any audiobook hosted on anyaudiobooks.com
#' 
#' @param url Target URL of audiobook, on https://anyaudiobook.com
#' @param file_prefix Prefix for downloaded files 
#' @param destfolder Destination folder for downloaded files
#'
#' @return Downloaded audiobook files in destination folder
#' @export
#'
#' @examples


if(!require(purrr)){install.packages('purrr')}
if(!require(rvest)){install.packages('rvest')}

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




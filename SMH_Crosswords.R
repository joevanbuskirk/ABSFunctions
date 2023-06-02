################################################################################
##### Author:      Joe Van Buskirk                                         #####
##### Date:        2023-06-02                                              #####
##### Purpose:     Assigns a function to pull the SMH/Age crossword,       #####
#####              bypassing the paywall and creates a nifty ggplot        #####
#####              version of it for printing                              #####
##### Next steps:  Convert grid/clues to .puz so it can be loaded on       #####
#####              a phone based crossword app                             #####
################################################################################

if(!require(rvest)){install.packages('rvest')}
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(gridExtra)){install.packages('gridExtra')}

library(rvest)
library(tidyverse)
library(gridExtra)

Get_SMH_Crossword <- function(date = Sys.Date()){
  date <- as.Date(date)
  date_fmt <- format(date, '%Y-%m-%d')
  date_nice_fmt <- format(date, '%A, %B %d, %Y')
  date_nice_fmt <- gsub(' 0(\\d)', ' \\1', date_nice_fmt)
  url <- glue::glue('https://www.smh.com.au/puzzles/crosswords/cryptic/{date_fmt}')
  
  rvest_rd <- rvest::read_html(url)
  
  Setter <- rvest_rd %>%
    html_nodes('h2') %>% 
    html_text()
  
  Setter <- Setter[grepl('Cryptic', Setter)]
  Setter <- gsub('.* by ', 'by ', Setter)
  
  clues <- rvest_rd %>% 
    html_node(xpath = '//*[@id="crossword-clues"]') %>% 
    html_children() %>%
    purrr::map(html_children) %>% 
    setNames(c('Across', 'Down'))
  
  clues <- clues %>% 
    purrr::map(function(direct){
      out <- purrr::map_chr(direct, 
                            html_text)
      out <- gsub('^(\\d{1,2})', '\\1. ', out)
      out <- out[!grepl('^(Across|Down)$', out)]
      return(out)
    }) 
  
  
  lengths <- clues %>% 
    purrr::map(function(direct){
      purrr::map_dbl(
        direct,
        function(clue){
          len <- gsub('.*\\((.*)\\)$', "\\1", clue)
          len <- stringr::str_extract_all(len, pattern = '\\d')
          len <- as.numeric(unlist(len))
          sum(len)  
        })
    }
    )
  
  tables <- rvest_rd %>% 
    html_table()
  
  directs <- as.matrix(tables[[1]])
  
  grid <- as.matrix(tables[[2]])
  grid <- ifelse(as.matrix(grid) == "", 0, 1)
  
  
  d1 <- expand.grid(x = 1:ncol(directs),
                    y = 1:nrow(directs)) 
  
  d1 <- dplyr::arrange(d1, x, -y)
  
  d1$lab <- c(directs)
  d1$grid <- c(grid)
  d1$grid <- as.character(d1$grid)
  
  grid_plot <- ggplot(d1, 
                      aes(x = x, y = y, fill = grid, label = lab)) +
    geom_tile(colour = 'grey20') + 
    scale_fill_manual(values = c('black', 'white'), breaks = NULL) +
    geom_text(na.rm = TRUE, 
              hjust = 0,
              nudge_x = -.45,
              nudge_y = .3,  
              size = 2.5) +
    theme_void() +
    labs(title = date_nice_fmt,
         subtitle = Setter)
  
  grid_plot
  
  len <- 65
  
  clue_df <- purrr::map_df(clues, 
                           data.frame, 
                           .id = 'Direction') %>% 
    dplyr::rename(clue = 2) %>% 
    dplyr::mutate(x = 1,
                  clue = gsub(paste0("(.{", len, "}.*?) "), "\\1\n", clue)) %>% 
    dplyr::mutate(lnes = stringr::str_count(clue, '\n')/1.8 + 1,
                  lnes = ifelse(lnes == 0, 1, lnes)) %>% 
    dplyr::group_by(Direction) %>% 
    dplyr::mutate(clue = rev(clue),
                  lnes = rev(lnes),
                  y = cumsum(lnes),
                  y = y - max(y)) %>% 
    dplyr::ungroup()
  
  
  clue_grid <- ggplot(clue_df, aes(x = x, y = y, label = clue)) +
    geom_text(hjust = 0, 
              vjust = 1,
              lineheight = 0.8,
              size = 3) +
    scale_x_continuous(limits = c(0, len + 30)) +
    scale_y_continuous() +
    theme_void() +
    facet_wrap(~Direction) +
    theme(strip.text = element_text(face = 'bold', hjust = 0.05))
  
  
  gridExtra::grid.arrange(grobs = list(grid_plot, clue_grid), 
                          layout_matrix = rbind(c(1,1,1),
                                                c(1,1,1),
                                                c(2,2,2)))
}



if(!require(rvest)){install.packages('rvest')}
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(gridExtra)){install.packages('gridExtra')}

library(rvest)
library(tidyverse)
library(gridExtra)

date = Sys.Date() - 10
type = 'cryptic'

## Set dates and format them
date <- as.Date(date)
date_fmt <- format(date, '%Y-%m-%d')
date_nice_fmt <- format(date, '%A, %B %d, %Y')
date_nice_fmt <- gsub(' 0(\\d)', ' \\1', date_nice_fmt)

## Read html
url <- glue::glue('https://www.smh.com.au/puzzles/crosswords/{type}/{date_fmt}')
rvest_rd <- rvest::read_html(url)

## Get setter
Setter <- rvest_rd %>%
  html_nodes('h2') %>% 
  html_text()

Setter <- Setter[grepl(type, Setter, ignore.case = TRUE)]
Setter <- gsub('.* by ', 'by ', Setter)

## Get clues
clues <- rvest_rd %>% 
  html_node(xpath = '//*[@id="crossword-clues"]') %>% 
  html_children() %>%
  html_children() %>% 
  as.character() %>% 
  gsub("<.*?>", "|", .) %>% 
  gsub("\\|\\|(.*?)\\|\\|", "\\1: ", .) %>% 
  gsub('\\||\n', '', .)

clues <- data.frame(clue = clues) %>% 
  dplyr::mutate(Direction = ifelse(clue %in% c('Across', 'Down'),
                                   clue,
                                   NA_character_)) %>% 
  tidyr::fill(Direction, .direction = 'down') %>% 
  dplyr::filter(!clue %in% c('Across', 'Down'))

clue_df <- clues %>% 
  dplyr::group_by(Direction) %>% 
  dplyr::mutate(clue = rev(clue)) %>% 
  dplyr::ungroup()


## Get grid layout
## Note this actually gets the **answers** but that's cheating
## So I've suppressed them
tables <- rvest_rd %>% 
  html_table()

directs <- as.matrix(tables[[1]])


grid <- as.matrix(tables[[2]])
# grid <- ifelse(grid == "", character(0), grid)


clues_out <- data.frame(x = integer(),
                        y = integer(),
                        direction = character(),
                        answer = character())


x <- 1
y <- 1

# x <- 5


## Get the across clues first

for(x in 1:ncol(grid)){
  for(y in 1:nrow(grid)){
    i = 1
    j = 1
    
    print(paste0(x, ',', y))
    
    ### No need to do anything for last column for the 'across' clues
    if(x != ncol(grid)){
      ## Need to make sure the preceding cell is either empty, or the first one
      ## so we are not counting parts of words
  
      if(x == 1){
        if(grid[y, x + 1] != ""){
          while(grid[y, x + i] != "" | (x + i) > ncol(grid)){
            i <- i + 1
          }
          newclue <- data.frame(x = x, y = y, direction = 'across',
                                answer = paste0(grid[y, seq(x, x + i - 1, 1)], 
                                                collapse = ''))
          
          if(nchar(newclue$answer) > 1){
            clues_out <- rbind(clues_out, newclue)
          }
        } 
      } else {
        if (grid[y, x + 1] != "" & grid[y, x - 1] == "") {
          while(grid[y, x + i] != ""){
            i <- i + 1
            if((x + i) > ncol(grid)){break}
          }
          newclue <- data.frame(x = x, y = y, direction = 'across',
                                answer = paste0(grid[y, seq(x, x + i - 1, 1)], 
                                                collapse = ''))
          if(nchar(newclue$answer) > 1){
            clues_out <- rbind(clues_out, newclue)
          }
        }
      } 
    }
    
    ### No need to do anything for last row for the 'down' clues
    if(y != nrow(grid)){
      ## Need to make sure the preceding cell is either empty, or the first one
      ## so we are not counting parts of words
      
      if(y == 1){
        if(grid[y + 1, x] != ""){
          while(grid[y + j, x] != "" | (y + j) > nrow(grid)){
            j <- j + 1
          }
          newclue <- data.frame(x = x, y = y, direction = 'across',
                                answer = paste0(grid[seq(y, y + j - 1, 1), x], 
                                                collapse = ''))
          
          if(nchar(newclue$answer) > 1){
            clues_out <- rbind(clues_out, newclue)
          }
        } 
      } else {
        if (grid[y + 1, x] != "" & grid[y - 1, x] == "") {
          while(grid[y + j, x] != ""){
            j <- j + 1
            if((y + j) > nrow(grid)){break}
          }
          newclue <- data.frame(x = x, y = y, direction = 'across',
                                answer = paste0(grid[seq(y, y + j - 1, 1), x], 
                                                collapse = ''))
          if(nchar(newclue$answer) > 1){
            clues_out <- rbind(clues_out, newclue)
          }
        }
      } 
    }
    
    i = 1
    j = 1
  }
}
















# library(crossword.r)
# 
# cw <- Crossword$new(rows = 15, columns = 15)
# 
# 
# cw$letters <- grid
# cw$to_json()

grid
d1 <- expand.grid(x = 1:ncol(directs),
                  y = 1:nrow(directs)) 

d1 <- dplyr::arrange(d1, x, -y)

d1$lab <- c(directs)
d1$grid <- c(grid)
d1$grid <- as.character(d1$grid)
d1$grid <- ifelse(d1$grid == '', NA_character_, d1$grid)

Answers_Down <- d1 %>%
  dplyr::filter((!is.na(grid) & !is.na(dplyr::lead(grid))) | 
                  (!is.na(dplyr::lag(grid)) & !is.na(grid))) %>% 
  tidyr::fill(lab, .direction = 'down') %>% 
  dplyr::group_by(lab) %>% 
  dplyr::summarise(answer = paste0(grid, collapse = ''),
                   y = max(y),
                   x = min(x))
  dplyr::pull(grid) %>% 
  paste0(collapse = '') %>% 
  stringr::str_extract_all("\\w{2,}") %>% 
  unlist()

Answers_Across <- d1 %>% 
  dplyr::arrange(x, -y) %>% 
  dplyr::pull(grid) %>% 
  paste0(collapse = '') %>% 
  stringr::str_extract_all("\\w{2,}") %>% 
  unlist()

grid


stringr::str_extract_all(paste0(d1$grid, collapse = ''),
                         "\\w{2,}")




## Plot the grid
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

## Plot the clues (underneath)
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


## Tack them onto one another
gridExtra::grid.arrange(grobs = list(grid_plot, clue_grid), 
                        layout_matrix = rbind(c(1,1,1),
                                              c(1,1,1),
                                              c(2,2,2)))
}

Get_SMH_Crossword()
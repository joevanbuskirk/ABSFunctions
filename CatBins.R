CatBins <- function(x, brk, type = c('pct', 'num', 'dollar'), acc = 1){
  
  if(missing(type)){type = 'num'}
  if(missing(brk) & type == 'num'){brk = 1000}
  if(missing(brk) & type == 'pct'){brk = 0.2}
  
  if(brk <= 0.01){acc = 0.1}
  
  brkseq <- seq(min(x, na.rm = TRUE), 
                brk * ceiling(max(x, na.rm = TRUE)/brk), 
                brk)
  

  if(type == 'pct'){
    Cat <- cut(x,
               breaks = brkseq,
               labels = paste(
                 scales::number(100 * (brkseq[-1] - brk) + acc,
                                accuracy = acc), 
                 "\u2014",
                 scales::percent(brkseq[-1], 
                                 accuracy = acc)))
  } else if (type == 'num') {
    Cat <- cut(x,
               breaks = brkseq,
               labels = paste(
                 scales::number(brkseq[-1] - brk + acc,
                                accuracy = acc, 
                                big.mark = ','), 
                 "\u2014",
                 scales::number(brkseq[-1], 
                                accuracy = acc,
                                big.mark = ',')))
  } else if (type == 'dollar') {
    Cat <- cut(x,
               breaks = brkseq,
               labels = paste(
                 scales::dollar(brkseq[-1] - brk + acc,
                                accuracy = acc, 
                                big.mark = ','), 
                 "\u2014",
                 scales::dollar(brkseq[-1], 
                                accuracy = acc,
                                big.mark = ',')))
    
  }
  
  CatFact <- Cat
  Cat <- as.character(Cat)
  Cat <- ifelse(x == 0, 
                as.character(levels(CatFact)[1]), 
                Cat)
  Cat <- ifelse(!is.finite(x), NA_character_, Cat)
  Cat <- gsub("^1 ", "0 ", Cat)
  Cat <-  forcats::fct_reorder(Cat, x)
  
  if(length(levels(Cat)) > 15){
    warning("More than 15 levels.. adjust the breaks")
  }
  
  if(length(levels(Cat)) == 1){
    warning("Only 1 level.. breaks too high")
  }
  
  return(Cat)
  
}

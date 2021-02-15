library(tidyverse)

xx <- readLines("input.txt")

# Part 1
no_paren <- function(x) {
  x <- str_remove_all(x, '\\(|\\)')
  while(length(str_split(x, " ")[[1]]) > 1){
    xs <- str_split(x, " ")[[1]]
    x <- paste(c(eval(parse(text=paste(xs[1:3], collapse = ''))), 
                 xs[-1:-3]), 
               sep='', 
               collapse=' ')
  }
  x
}

full_evaluate <- function(x){
  while (length(str_extract_all(x, '\\([^\\(\\)]+\\)')[[1]]) != 0) {
    paren <- str_extract_all(x, '\\([^\\(\\)]+\\)')[[1]]
    for (p in paren){
      x <- str_replace(x, fixed(p), no_paren(p))
    }
  }
  as.numeric(no_paren(x))
}

options(scipen=999)
sum(map_dbl(xx, full_evaluate))

# Part 2
no_paren <- function(x) {
  x <- str_remove_all(x, '\\(|\\)')
  #Add
  while(sum(str_split(x, " ")[[1]] == '+') != 0){
    xs <- str_split(x, " ")[[1]]
    a <- which(xs == '+')[1]
    t <- paste(xs[(a-1):(a+1)], collapse = ' ')
    x <- str_replace(x, fixed(t), eval(parse(text=t)))
  }
  #Mult
  while(sum(str_split(x, " ")[[1]] == '*') != 0){
    xs <- str_split(x, " ")[[1]]
    a <- which(xs == '*')[1]
    t <- paste(xs[(a-1):(a+1)], collapse = ' ')
    x <- str_replace(x, fixed(t), eval(parse(text=t)))
  }
  x
}

options(scipen=999)
sum(map_dbl(xx, full_evaluate))

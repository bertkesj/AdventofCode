
x <- as.character(
  readLines('C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 5/input.txt'))


new_set <- function(n, l, u, ud='B'){
  if (n == ud){
    l <- l + (u - l + 1)/2
  } else u <- u - (u - l + 1)/2
  return(list(n, l, u))
}

get_id <- function(id){
  l <- 0
  u <- 127
  for (i in 1:7){
    n <- new_set(substr(id, i, i), l, u)
    l <- n[[2]]
    u <- n[[3]]
  }
  n1 <- u
  
  
  l <- 0
  u <- 7
  for (i in 8:10){
    n <- new_set(substr(id, i, i), l, u, 'R')
    l <- n[[2]]
    u <- n[[3]]
  }
  n2 <- u
  
  
  n1*8 + n2
}

library(tidyverse)
max(map_dbl(x, get_id))

book <- sort(map_dbl(x, get_id))
book[(which(lag(book) - book != -1)-1):(which(lag(book) - book != -1)+1)]



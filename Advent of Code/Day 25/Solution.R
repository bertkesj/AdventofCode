library(tidyverse)

x <- as.numeric(readLines("input.txt"))

iteration <- function(v, s)  (v*s) %% 20201227

loop_size <- function(x){
  v <- 1
  l <- 0
  while (v != x){
    l <- l+1
    v <- iteration(v, 7)
  }
  return(l)
}

l1 <- loop_size(x[1])
l2 <- loop_size(x[2])

#Part 1
v <- 1
for (i in 1:l1) v <- iteration(v,x[2])
v #17032383
 


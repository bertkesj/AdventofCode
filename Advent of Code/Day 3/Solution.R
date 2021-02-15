library(tidyverse)
input <- read_fwf("C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 3/input.txt", 
                  fwf_widths(rep(1, 31))) 

d1 <- function(right, down){
  tree <- 0
  for (r in 1:(ceiling(nrow(input)/down)+1)){
    row <- down*(r-1) + 1
    if (row <= nrow(input)){
      col <- (r - 1)*right + 1
      col <- ((col - 1) %% ncol(input)) + 1
      tree <- tree + (input[row, col] == '#')
      #print(paste(row, col, input[row, col]))
    }
  }
  tree
}
d1(3,1)
d1(1,1)*d1(3,1)*d1(5,1)*d1(7,1)*d1(1,2)





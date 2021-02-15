library(tidyverse)

#Part 1
x <-
  paste(
    readLines(
      "C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 6/input.txt"
    ),
    collapse = "\n"
  )
x <- str_split(x, '\n\n')
x <- str_replace_all(x[[1]], '\n', "")

ct_unique <- function(str){
  sum(!!str_count(str, letters))
}
sum(map_dbl(x, ct_unique))


#Part 2
x <-
  paste(
    readLines(
      "C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 6/input.txt"
    ),
    collapse = "\n"
  )
x <- str_split(x, '\n\n')

only_one <- function(str){
  num <- str_count(str, letters)
  p <- str_count(str,'\n') + 1
  length(which(num / p == 1))
}
sum(map_dbl(x[[1]], only_one))

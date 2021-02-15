library(tidyverse)
x <- readLines("input.txt")

dir <- str_sub(x, 1, 1)
unit <- as.numeric(str_sub(x, 2, -1))

#Part 1
loc <- c(0, 0, 1, 0)

step <- function(d,u){
  if (d == 'F') {
    loc[1:2] <<- loc[1:2] + u * loc[3:4]
  } else if (d == 'L')
  {
    for(i in 1:(u/90)) loc[3:4] <<- c(-loc[4], loc[3])
  } else if (d == 'R')
  {
    for(i in 1:(u/90)) loc[3:4] <<- c(loc[4], -loc[3])
  } else if (d == 'N')
  {
    loc[1:2] <<- loc[1:2] + u * c(0,1)
  } else if (d == 'S')
  {
    loc[1:2] <<- loc[1:2] + u * c(0,-1)
  }else if (d == 'E')
  {
    loc[1:2] <<- loc[1:2] + u * c(1,0)
  }else if (d == 'W')
  {
    loc[1:2] <<- loc[1:2] + u * c(-1,0)
  }
  print(loc)
}

for (i in seq_along(x)) step(dir[i], unit[i])
sum(abs(loc[1:2]))



#Part 2
loc <- c(0, 0, 10, 1)

step <- function(d,u){
  if (d == 'F') {
    loc[1:2] <<- loc[1:2] + u * loc[3:4]
  } else if (d == 'L')
  {
    for(i in 1:(u/90)) loc[3:4] <<- c(-loc[4], loc[3])
  } else if (d == 'R')
  {
    for(i in 1:(u/90)) loc[3:4] <<- c(loc[4], -loc[3])
  } else if (d == 'N')
  {
    loc[3:4] <<- loc[3:4] + u * c(0,1)
  } else if (d == 'S')
  {
    loc[3:4] <<- loc[3:4] + u * c(0,-1)
  }else if (d == 'E')
  {
    loc[3:4] <<- loc[3:4] + u * c(1,0)
  }else if (d == 'W')
  {
    loc[3:4] <<- loc[3:4] + u * c(-1,0)
  }
  print(loc)
}

for (i in seq_along(x)) step(dir[i], unit[i])
sum(abs(loc[1:2]))

library(tidyverse)
x <- as.numeric(
  readLines('C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 10/input.txt'))
x <- sort(x)
e <- max(x) + 3

xl <- c(0, x, e)

#Part 1
d<-diff(x)
sum(diff==3)*sum(diff==1)

#Part 2
mapping <- tibble()
for (index in xl){
  mapping <- bind_rows(mapping, tibble(old = index,
                    new = xl[between(xl - index, 0, 3)][-1]))
}

paths <- tibble(x = 0,
                ct = 1)
pathscomp <- tibble()
while(nrow(paths) != 0){
  pathsfull <- left_join(paths, mapping, by=c('x' = 'old')) %>%
    select(x = new, ct) %>%
    group_by(x) %>%
    summarize(ct = sum(ct))
  
  paths <- filter(pathsfull, x!=e)
  pathscomp <- bind_rows(pathscomp, filter(pathsfull, x==e))
  print(nrow(pathscomp))
}
options(scipen=999)
print(sum(pathscomp$ct))




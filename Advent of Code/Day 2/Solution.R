library(readr)
library(tidyverse)
input <- read_csv("C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 2/input.txt", 
                  col_names = FALSE) %>%
  separate(X1, c('first', 'password'), ':') %>%
  separate(first, c('l', 'char'), ' ') %>%
  separate(l, c('l', 'u'), '-') %>%
  mutate(count = str_count(password, char),
         good = (as.numeric(l) <= count & count <= as.numeric(u)))
  

sum(input$good)  
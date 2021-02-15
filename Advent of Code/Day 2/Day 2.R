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

input <- input %>%
  mutate(good2 = (substr(password, as.numeric(l)+1, as.numeric(l)+1) == char) +
           (substr(password, as.numeric(u)+1, as.numeric(u)+1)== char) == 1)
sum(input$good2)  
  

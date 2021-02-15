library(tidyverse)
x <- as.numeric(
  readLines('C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 9/input.txt'))
preamble <- 25

#Part 1
for (i in (preamble+1):length(x)){
  if (!x[i] %in% colSums(combn(x[(i-preamble):(i-1)], 2))){
    print(x[i])
    weakness <- x[i]
    break
  }
}

#Part 2

for (i in 1:length(x)){
  for (j in i:length(x)){
    if (sum(x[i:j]) >= weakness) break
  }
  if (sum(x[i:j]) == weakness) break
}
min(x[i:j]) + max(x[i:j])



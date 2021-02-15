library(tidyverse)
x <-  readLines(
      "C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 7/input.txt"
    )

name <- function(chr){
  chr[1]
}
value <- function(chr){
  chr[2]
}
inside <- function(chr){
  str_trim(str_split(str_remove_all(chr, 'bags?|[0-9]+|\\.'), ",")[[1]])
}
in_num <- function(chr){
  as.numeric(str_split(str_remove_all(chr, '[a-z \\.]+'), ",")[[1]])
}

bags <- map_chr(str_split(x, ' bags contain'), value)
names(bags) <- map_chr(str_split(x, ' bags contain'), name)
numbers <- map(bags, in_num)
bags <- map(bags, inside)

# Part 1
bagmap <- list()
for (b in names(bags)){
  for (c in bags[[b]]){
    if (length(bagmap[[c]]) == 0) bagmap[[c]] <- c()
    bagmap[[c]] <- c(bagmap[[c]], b)
  }
}

answer <- bagmap[['shiny gold']]
last <- c()
while (length(last) != length(answer)){
  last <- answer
  for (n in answer){
    answer <- unique(c(answer, bagmap[[n]]))
  }
}
print(length(answer))


# Part 2
n_bags <- function(bg, num) {
  if (is.na(numbers[[bg]])) 0 else{
    print(paste(num, bg))
    num*sum(numbers[[bg]]) + sum(map2_dbl(bags[[bg]], num*numbers[[bg]], n_bags))
  }
}
n_bags('shiny gold', 1)

library(tidyverse)

x <- as.numeric(strsplit(readLines("input.txt"), '')[[1]])
l <- length(x)

next_cup <- rep(NA, l)
next_cup[x] <- c(x[-1], x[1])
current <- x[1]
for (round in 1:100){
  c1 <- next_cup[current]
  c2 <- next_cup[c1]
  c3 <- next_cup[c2]
  pickup <- c(c1, c2, c3)
  dest <- setdiff((current - (1:5) - 1) %% l + 1, pickup )[1]
  
  next_cup[current] <- next_cup[c3]
  next_cup[c3] <- next_cup[dest]
  next_cup[dest] <- c1
  
  current <- next_cup[current]
}

output <- c(1)
for (i in 1:8) {
  output <- c(output, next_cup[last(output)])
}
paste(output[-1], collapse='')
#27956483



# Part 2
x <- as.numeric(strsplit(readLines("input.txt"), '')[[1]])
l <- length(x)
x <- c(x, (l+1):1000000)
l <- 1000000

next_cup <- rep(NA, l)
next_cup[x] <- c(x[-1], x[1])
current <- x[1]

start <- Sys.time()
for (round in 1:10000000){
  c1 <- next_cup[current]
  c2 <- next_cup[c1]
  c3 <- next_cup[c2]
  pickup <- c(c1, c2, c3)
  dest <- setdiff((current - (1:5) - 1) %% l + 1, pickup )[1]
  
  next_cup[current] <- next_cup[c3]
  next_cup[c3] <- next_cup[dest]
  next_cup[dest] <- c1
  
  current <- next_cup[current]
  if (round %% 100000 == 0) {
    print(round)
    time <- Sys.time() - start
    print(time)
    print(paste('Est:', 10000000/round*time/(60), 'min'))
  }
}
a <- next_cup[1] 
b <- next_cup[a]
a*b



library(tidyverse)
x <- readLines("input.txt")
time <- as.numeric(x[1])
buses <- str_split(x[2], ',')[[1]]
buses <- as.numeric(buses[buses != 'x'])

#Part 1
min <- 0
while(TRUE) {
  t <- (min + time) / buses
  c <- buses[floor(t) == t]
  if (length(c) >= 1) break
  min <- min + 1
}
c*min

#Part 2
b <- as.numeric(str_split(x[2], ',')[[1]])
ahead <- 1:length(b) - 1

ahead <- ahead[!is.na(b)]
b <- b[!is.na(b)]

mult <- c(1, b[-1])
indp <- 0
for (e in 2:length(b)){
  i <- 0
  while(TRUE){
    ind <- (indp + i*prod(mult[1:e-1]))*b[1]
    div <- ((ind + ahead[1:e]) %% (b[1:e])) 
    if (sum(div) == 0) {
      break }
    i <- i+1
  }
  indp <- (indp + i*prod(mult[1:e-1]))
}
sprintf('%.0f', ind)

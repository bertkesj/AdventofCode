library(tidyverse)
x <- unlist(str_split(readLines("input.txt"), ","))

#x <- as.character(c(1,3,2))

#Part 1
get_value <- function(v, x){
  pos <- 1:length(x)
  names(pos) <- x
  
  vals <- x[-length(x)]
  l <- tail(x, 1)
  l2 <- x[length(x) - 1]
  
  start <- Sys.time()
  for (i in (length(x) + 1):v){
    if (i %% 100000 == 0){print(paste(i, ':', Sys.time() - start))}
    pos[l2] <- i-2
    vals <- unique(c(vals, l2))
    if (l %in% vals) {
      l2 <- l
      l <- as.character(i-1 - pos[l])
    } else {
      l2 <- l
      l <- '0'
    }
  }
  l
}

get_value(2020, x)
write(get_value(30000000, x), file = 'output.txt')

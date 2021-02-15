library(tidyverse)

x <- readLines("input.txt")



get_tile <- function(x){
  x <- x %>%
    str_replace_all('sw', '1') %>%
    str_replace_all('se', '2') %>%
    str_replace_all('ne', '4') %>%
    str_replace_all('nw', '5')
  
  tile <- c(0,0)
  for (dir in str_split(x,'')[[1]]){
    if (dir == '1') tile <- tile + c(-.5, -1)
    if (dir == '2') tile <- tile + c(+.5, -1)
    if (dir == 'e') tile <- tile + c(+1, 0)
    if (dir == '4') tile <- tile + c(+.5, 1)
    if (dir == '5') tile <- tile + c(-.5, 1)
    if (dir == 'w') tile <- tile + c(-1, 0)
  }
  tile <- paste(tile, collapse = ',')
  return(tile)
  
}

tiles <- sapply(x, get_tile)
sum(table(tiles) %% 2 == 1) #465

# Part 2
b <- names(which(table(tiles) %% 2 == 1))
x <- as.numeric(sapply(str_split(b, ','), `[`, 1))
y <- as.numeric(sapply(str_split(b, ','), `[`, 2))
mx <- max(abs(x)) + 60
my <- max(abs(y)) + 60


xfull <- c()
yfull <- c()
for (i in -mx:mx){
  for (j in my:-my){
    yfull <- c(yfull, j)
    xfull <- c(xfull, i - .5*(j %% 2 == 1))
  }
}
xyfull <- paste0(xfull, ',', yfull)

colorfull <- rep(0, length(xyfull))
colorfull[xyfull %in% b] <- 1

#sw
for (i in 1:100){
  bn <- paste0(xfull - .5, ',', yfull - 1) %in% b +
    paste0(xfull + .5, ',', yfull - 1) %in% b +
    paste0(xfull + 1, ',', yfull - 0) %in% b +
    paste0(xfull + .5, ',', yfull + 1) %in% b +
    paste0(xfull - .5, ',', yfull + 1) %in% b +
    paste0(xfull - 1, ',', yfull - 0) %in% b
  
  switch <- (colorfull == 1 & (bn == 0 | bn > 2)) |
    (colorfull == 0 & bn == 2)
  colorfull <- (colorfull + switch) %% 2
  
  b <- xyfull[colorfull == 1]
  print(paste(i, sum(colorfull)))
}

sum(colorfull)



x <- as.numeric(sapply(str_split(b, ','), `[`, 1))
y <- as.numeric(sapply(str_split(b, ','), `[`, 2))
ggplot() + 
  geom_point(aes(x=xfull, y=yfull), color = 'white', size = .1) +
  geom_point(aes(x=x, y=y), color='black', size = .1) 
       


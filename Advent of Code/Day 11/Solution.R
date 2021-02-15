library(tidyverse)
x <- do.call(rbind, strsplit(readLines("input.txt"), ""))

right <- function(x) cbind(x[,-1], rep('.', nrow(x)))
left <- function(x) cbind(rep('.', nrow(x)), x[,-ncol(x)])
up <- function(x) rbind(rep('.', ncol(x)), x[-nrow(x),])
down <- function(x) rbind(x[-1,], rep('.', ncol(x)))

#Part 1
old <- c()
while (sum(old == x) != length(x)){
  old <- x
  occ <- (left(x) == '#') + (right(x) == '#') + (up(x) == '#' ) + (down(x) == '#') +
    (left(up(x)) == '#') + (left(down(x)) == '#') + (right(up(x)) == '#') + (right(down(x)) == '#')
  x[(x=='L' & occ==0)] <- '#'
  x[(x=='#' & occ>=4)] <- 'L'
}
sum(x=='#')

#Part 2
x <- do.call(rbind, strsplit(readLines("input.txt"), ""))
get_index <- function(r, c) (c - 1)*nrow(x) + r
coord <- function(index) {
  c <- ceiling((index)/ncol(x))
  r <- index - (c-1)*ncol(x)
  c(r,c)
}

l <- rep(0, nrow(x)*ncol(x))
r <- rep(0, nrow(x)*ncol(x))
u <- rep(0, nrow(x)*ncol(x))
d <- rep(0, nrow(x)*ncol(x))
lu <- rep(0, nrow(x)*ncol(x))
ld <- rep(0, nrow(x)*ncol(x))
ru <- rep(0, nrow(x)*ncol(x))
rd<- rep(0, nrow(x)*ncol(x))
for (row in 1:nrow(x)){
  for (col in 1:ncol(x)){

    #walk left
    if (col > 1){
      for (s in 1:(col-1)){
        if (x[get_index(row, col-s)] != ".") {
          l[get_index(row, col)] <- get_index(row, col-s)
          break
        }
      }
    }
    #walk right
    if (col < ncol(x)){
      for (s in 1:(ncol(x)-col)){
        if (x[get_index(row, col+s)] != ".") {
          r[get_index(row, col)] <- get_index(row, col+s)
          break
        }
      }
    }
    #walk up
    if (row > 1){
      for (s in 1:(row-1)){
        if (x[get_index(row-s, col)] != ".") {
          u[get_index(row, col)] <- get_index(row-s, col)
          break
        }
      }
    }
    #walk down
    if (row < nrow(x)){
      for (s in 1:(nrow(x)-row)){
        if (x[get_index(row+s, col)] != ".") {
          d[get_index(row, col)] <- get_index(row+s, col)
          break
        }
      }
    }
    #walk upleft
    if (col > 1 & row > 1){
      for (s in 1:min((col-1), row-1)){
        if (x[get_index(row-s, col-s)] != ".") {
          lu[get_index(row, col)] <- get_index(row-s, col-s)
          break
        }
      }
    }
    #walk leftdown
    if (col > 1 & row < nrow(x)){
      for (s in 1:min((nrow(x)-row),(col-1))){
        if (x[get_index(row+s, col-s)] != ".") {
          ld[get_index(row, col)] <- get_index(row+s, col-s)
          break
        }
      }
    }
    #walk rightup
    if (col < ncol(x) & row > 1){
      for (s in 1:min((row-1), (ncol(x)-col))){
        if (x[get_index(row-s, col+s)] != ".") {
          ru[get_index(row, col)] <- get_index(row-s, col+s)
          break
        }
      }
    }
    #walk right down
    if (col < ncol(x) & row < nrow(x)){
      for (s in 1:min((nrow(x)-row), (ncol(x)-col))){
        if (x[get_index(row+s, col+s)] != ".") {
          rd[get_index(row, col)] <- get_index(row+s, col+s)
          break
        }
      }
    }
  }
}

get_occ <- function(index){
  (min(x[l[index]] , '.') == "#") +(min(x[r[index]] , '.') == "#") +(min(x[u[index]] , '.') == "#") +
    (min(x[d[index]] , '.') == "#") +(min(x[lu[index]] , '.') == "#") +(min(x[ld[index]] , '.') == "#") +
    (min(x[ru[index]] , '.') == "#") +(min(x[rd[index]] , '.') == "#")
}


old <- c()
while (sum(old == x) != length(x)){
  old <- x
  occ <- map_dbl(1:(nrow(x)*ncol(x)), get_occ)
  x[(x=='L' & occ==0)] <- '#'
  x[(x=='#' & occ>=5)] <- 'L'
  x
}
sum(x=='#')

library(tidyverse)
library(abind)
x <- do.call(rbind, strsplit(readLines("input.txt"), ""))

nr <- nrow(x) + 12
nc <- ncol(x) + 12
nv <- 13
nh <- 13
universe <- rep('.', nr*nc*nv*nh)
dim(universe) <- c(nr, nc, nv, nh)
universe[7:(7+nrow(x)-1), 7:(7+ncol(x)-1), 7, 7] <- x

up     <- function(x) abind(x[-1,,,], array(rep('.', nc*nv*nh), dim = c(nc, nv, nh)), along=1)       
right  <- function(x) abind(x[,-1,,], array(rep('.', nr*nv*nh), dim = c(nr, nv, nh)), along=2)
above  <- function(x) abind(x[,,-1,], array(rep('.', nr*nc*nh), dim = c(nr, nc, nh)), along=3)
aboveh <- function(x) abind(x[,,,-1], array(rep('.', nr*nc*nv), dim = c(nr, nc, nv)), along=4)

down   <- function(x) abind(array(rep('.', nc*nv*nh), dim = c(nc, nv, nh)), x[-nr,,,], along=1)       
left   <- function(x) abind(array(rep('.', nr*nv*nh), dim = c(nr, nv, nh)), x[,-nc,,], along=2)
below  <- function(x) abind(array(rep('.', nr*nc*nh), dim = c(nr, nc, nh)), x[,,-nv,], along=3)
belowh <- function(x) abind(array(rep('.', nr*nc*nv), dim = c(nr, nc, nv)), x[,,,-nh], along=4)

shift <- function(coord, out){
  if (coord[1] == 1) out <- out <- up(out)
  if (coord[1] == -1) out <- out <- down(out)
  if (coord[2] == 1) out <- out <- right(out)
  if (coord[2] == -1) out <- out <- left(out)
  if (coord[3] == 1) out <- out <- above(out)
  if (coord[3] == -1) out <- out <- below(out)
  if (coord[4] == 1) out <- out <- aboveh(out)
  if (coord[4] == -1) out <- out <- belowh(out)
  return(out)
}

orig <- universe
#Part 1
grid <- expand.grid(-1:1, -1:1, -1:1, 0)[-14,]
for (state in 1:6){
  occ <- array(0, dim=c(nr, nc, nv, nh))
  for (i in 1:26){
    occ <- occ + (shift(as.numeric(grid[i,]), universe) == '#')
  }
  universe[!(universe == '#' & occ %in% c(2,3))] <- '.'
  universe[(universe == '.' & occ %in% c(3))] <- '#'
}
sum(universe=='#')

#Part 2
universe <- orig
grid <- expand.grid(-1:1, -1:1, -1:1, -1:1)[-41,]
for (state in 1:6){
  occ <- array(0, dim=c(nr, nc, nv, nh))
  for (i in 1:80){
    occ <- occ + (shift(as.numeric(grid[i,]), universe) == '#')
  }
  universe[!(universe == '#' & occ %in% c(2,3))] <- '.'
  universe[(universe == '.' & occ %in% c(3))] <- '#'
}
sum(universe=='#')

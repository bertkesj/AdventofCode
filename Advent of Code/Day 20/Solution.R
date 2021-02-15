library(tidyverse)

xx <- readLines("input.txt")
new_t_line <- which(substr(xx, 1, 4) == "Tile")
tile_names <- str_remove(xx[new_t_line], "Tile ") %>%
  str_remove(':') %>%
  as.numeric()
ntiles <- length(tile_names)
dimension <- sqrt(ntiles)

rotate <- function(x) (apply(t(x), 2, rev))
rot_sid <- function(x) (x %% 4 + 1) 
flip <- function(x) apply(x, 2, rev)
flip_sid <- function(x) case_when(x == 1 ~ 3,
                                  x == 3 ~ 1,
                                  TRUE ~ x)

tiles <- list()
for (t in seq_along(new_t_line)){
  tiles[[t]] <- 
    do.call(rbind, strsplit(xx[(new_t_line[t]+1):(new_t_line[t]+10)], ''))
}

edges <- list()
for (t in seq_along(tiles)){
  edges[[t]] <- cbind((tiles[[t]][1, ]),
                      (tiles[[t]][,1]),
                      (tiles[[t]][10, ]),
                      (tiles[[t]][,10]))
}

matches <- list()
sides <- list()
for (t in 1:length(tile_names)){
  matches[[t]] <- c(1)
  sides[[t]] <- c(1)
  for (c in 1:length(tile_names)){
    if (t == c) next
    for (e1 in 1:4){
      for (e2 in 1:4){
        if (sum(edges[[t]][,e1] == edges[[c]][,e2]) == 10) {
          matches[[t]] <- c(matches[[t]], c)
          sides[[t]] <- c(sides[[t]], e1)
        }
        if (sum(edges[[t]][,e1] == rev(edges[[c]][,e2])) == 10) {
          matches[[t]] <- c(matches[[t]], c)
          sides[[t]] <- c(sides[[t]], e1)
        }
      }
    }
  }
}

# Part 1
matches <- lapply(matches, `[`, -1)
sides <- lapply(sides, `[`, -1) 
nmatches <- sapply(matches, length) 
options(scipen=999)
prod(tile_names[nmatches == 2])

# Part 2
# Orient First Corner
mat <- c()
corn <- which(nmatches == 2)[1]
s <- sides[[corn]]
t <- tiles[[corn]]
mat <- c(mat, corn)
while (!(all(s == c(3,4)) | all(s == c(4,3)))) {
  s <- rot_sid(s)
  t <- rotate(t)
}
sides[[corn]] <- s
tiles[[corn]] <- t
status <- list(mat = mat,
               last = corn)

#Fill out image
add_col <- function(status, dir){
  new <- matches[[status[['last']]]][sides[[status[['last']]]] == dir]
  status[['mat']] <- c(status[['mat']], new)
  tn <- tiles[[new]]
  s <- sides[[new]]
  keepgoing <- TRUE
  while (keepgoing){
    print('flipping...')
    tn <- flip(tn)
    s <- flip_sid(s)
    for (r in 1:4) {
      print('rotating...')
      tn <- rotate(tn)
      s <- rot_sid(s)
      if (dir == 3) {
        if (all(tiles[[status$last]][nrow(tiles[[status$last]]),] == tn[1,])) {
          keepgoing <- FALSE
          break
        }
      } else {
        if (all(tiles[[status$last]][,nrow(tiles[[status$last]])] == tn[,1])) {
          keepgoing <- FALSE
          break
        }
      }

    }
  }
  sides[[new]] <<- s
  tiles[[new]] <<- tn
  status$last <- new
  return(status)
}

#walk through matrix
for (i in 1:(dimension - 1)){
  status <- add_col(status, 3)
}

for (n in 1 + dimension*(0:(dimension - 2))){
  status$last <- status$mat[n]
  status <- add_col(status, 4)
  for (i in 1:(dimension - 1)){
    status <- add_col(status, 3)
  }
}

mat <- matrix(status$mat, nrow=sqrt(ntiles))

#Paste Image
image <- c()
for (col in 1:ncol(mat)){
  cc <- tiles[[mat[1, col]]][c(-1,-10), c(-1,-10)]
  for (row in 2:nrow(mat)){
    cc <- rbind(cc, tiles[[mat[row, col]]][c(-1,-10), c(-1,-10)])
  }
  image <- cbind(image, cc)
}

#Monster
monster <- do.call(rbind, strsplit(readLines('monster.txt'),''))

check_match <- function(){
  match <- 0
  for (r in 1:(nrow(image) - 2)){
    for (c in 1:(ncol(image) - 19)){
      if (identical(intersect(which(image[r:(r+2), c:(c+19)]=='#'),
                              which(monster=='#')),
                    which(monster=='#'))) match <- match + 1
    }
  }
    return(match)
}

searching <- TRUE
while (searching) {
  image <- flip(image)
  print('flipping...')
  for (r in 1:4){
    image <- rotate(image)
    print('rotating...')
    match <- check_match()
    if (match > 0) {
      searching <- FALSE
      break
    }
  }
}

sum(image == '#') - 15*match

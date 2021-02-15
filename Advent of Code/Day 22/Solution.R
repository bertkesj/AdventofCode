library(tidyverse)

x <- readLines("input.txt") 
cuts <- which(substr(x, 1, 3) == 'Pla')[2]
p1 <- as.numeric(x[2:(cuts-2)])
p2 <- as.numeric(x[(cuts+1):length(x)])

#Part 1
while (length(p1) != 0 & length(p2) != 0){
  print(p1)
  print(p2)
  if (p1[1] > p2[1]){
    p1 <- c(p1[-1], p1[1], p2[1])
    p2 <- p2[-1]
  } else {
    p2 <- c(p2[-1], p2[1], p1[1])
    p1 <- p1[-1]
  }
}

if (length(p1) == 0) sum((length(p2):1) * p2)
if (length(p2) == 0) sum((length(p1):1) * p1)

#Part 2
game <- function(p1, p2, g){
  p1_his <- list()
  p2_his <- list()
  round <- 0
  
  while (length(p1) != 0 & length(p2) != 0){
    round <- round + 1
    #print(paste('Round', round, '(Game', g,')' ))
    #print(paste("Player 1:", paste(p1, collapse=' ', separate=' ')))
    #print(paste("Player 2:", paste(p2, collapse=' ', separate=' ')))
    #check
    if (round == 1 || 
        !any(sapply(p1_his, identical, p1) & 
             sapply(p2_his, identical, p2))) {
      p1_his[[round]] <- p1
      p2_his[[round]] <- p2
      
      d1 <- p1[1]
      d2 <- p2[1]
      p1 <- p1[-1]
      p2 <- p2[-1]
      if (d1 <= length(p1) &
          d2 <= length(p2)){
        outcome <- game(p1[1:d1], p2[1:d2], g+1)
        if (outcome$winner == 1) p1 <- c(p1, d1, d2)
        if (outcome$winner == 2) p2 <- c(p2, d2, d1)
      } else {
        if (d1 > d2) p1 <- c(p1, d1, d2)
        if (d2 > d1) p2 <- c(p2, d2, d1)
      }
    } else {
      #print('This was a repeat')
      return(list(winner = 1, score = sum((length(p1):1) * p1)))
    }
  }
  if (length(p1) == 0) return(list(winner = 2, score = sum((length(p2):1) * p2)))
  if (length(p2) == 0) return(list(winner = 1, score = sum((length(p1):1) * p1))) 
}

x <- readLines("input.txt") 
cuts <- which(substr(x, 1, 3) == 'Pla')[2]
p1 <- as.numeric(x[2:(cuts-2)])
p2 <- as.numeric(x[(cuts+1):length(x)])
game(p1, p2, 1) #should be 34173

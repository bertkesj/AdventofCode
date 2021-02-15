library(tidyverse)
x <- as.character(
  readLines('C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 8/input.txt'))
term <- 1 + length(x)

runline <- function(line, x){
  ins <- str_split(x[line], ' ')[[1]]
  if (ins[1] == 'acc') {
    acc <<- acc + as.numeric(ins[2])
    return(line + 1)
  }
  if (ins[1] == 'nop') {
    return(line + 1)
  }
  if (ins[1] == 'jmp') {
    return(line + as.numeric(ins[2]))
  }
}

#Part 1
acc <- 0
lines <- 1
nl <- 1
while(length(unique(lines)) == length(lines)){
  nl <- runline(nl, x)
  lines <- c(lines, nl)
}
print(acc)

#Part 2
f <- str_replace(x, ' [+\\-0-9]+', '')
nops <- which(f == 'nop')
jmps <- which(f == 'jmp')

for (nop in nops){
  xnew <- x
  xnew[nop] <- str_replace(x[nop], 'nop', 'jmp')
  
  acc <- 0
  lines <- 1
  nl <- 1
  while(length(unique(lines)) == length(lines) | nl == term){
    nl <- runline(nl, xnew)
    lines <- c(lines, nl)
  }
  if (nl == term) print(paste('success:', acc))
}

for (nop in jmps){
  xnew <- x
  xnew[nop] <- str_replace(x[nop], 'jmp', 'nop')
  
  acc <- 0
  lines <- 1
  nl <- 1
  while(length(unique(lines)) == length(lines) & nl != term){
    nl <- runline(nl, xnew)
    lines <- c(lines, nl)
  }
  if (nl == term) {
    print(paste('success:', acc))
    break
  }
}






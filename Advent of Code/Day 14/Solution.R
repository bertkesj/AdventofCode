library(tidyverse)
library('combinat')
library(GA)
x <- readLines("input.txt")

slot_length <- max(as.numeric(str_match(x, 'mem\\[([0-9]+)\\]')[,2]), na.rm=T)
slots <- rep(0, slot_length)

# Part 1
for (it in x){
  if (substr(it, 1, 3) == 'mas'){
    mask_inf <- str_remove(it, 'mask = ') %>%
      str_split('')%>%
      `[[`(1)
    mind <- which(mask_inf != 'X')
    mval <- as.numeric(mask_inf[mind])
  }
  if (substr(it, 1, 3) == 'mem'){
    slot <- as.numeric(str_match(it, 'mem\\[([0-9]+)\\]')[,2])
    val <- as.numeric(str_split(it, '=')[[1]][2]) %>%
      decimal2binary(36)
    val[mind] <- mval
    slots[slot] <- binary2decimal(val)
  }
}
sprintf('%.0f', sum(slots))

# Part 2
replaceX <- function(c, slot){
  slot[mvalX] <- c
  binary2decimal(slot)
}

slots <- list()
for (it in x){
  if (substr(it, 1, 3) == 'mas'){
    mask_inf <- str_remove(it, 'mask = ') %>%
      str_split('')%>%
      `[[`(1)
    mind1 <- which(mask_inf == 1)
    mvalX <- which(mask_inf == 'X')
  }
  if (substr(it, 1, 3) == 'mem'){
    slot <- as.numeric(str_match(it, 'mem\\[([0-9]+)\\]')[,2]) %>%
      decimal2binary(36)
    val <- as.numeric(str_split(it, '=')[[1]][2])
    comb <- expand.grid(rep(list(0:1), length(mvalX)))
    slot[mind1] <- 1
    slotl <- apply(as.matrix(comb), 1, replaceX, slot)
    slots[as.character(slotl)] <- val
  }
}

sprintf('%.0f', sum(unlist(slots), na.rm=T))



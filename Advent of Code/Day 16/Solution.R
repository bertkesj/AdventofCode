library(tidyverse)
x <- readLines("input.txt")

where <- which(x %in% c("your ticket:", "nearby tickets:"))
rules <- x[1:(where[1] - 2)]
yours <- x[where[1]+1]
tickets <- tail(x, length(x) - where[2])

#Part 1
valid <- map_chr(str_split(rules, ": "), function(x) x[2]) %>%
  str_split(' or ') %>%
  unlist() %>%
  str_split('-') %>%
  map(as.numeric) %>%
  map(full_seq, 1) %>%
  unlist() %>%
  unique()
tn <- as.numeric(unlist(str_split(tickets, ',')))
sum(tn[tn %in% setdiff(tn, valid)])

#Part 2
valid_list <- map_chr(str_split(rules, ": "), function(x) x[2]) %>%
  str_split(' or ') %>%
  unlist() %>%
  str_split('-') %>%
  map(as.numeric) %>%
  map(full_seq, 1)
rule_name <- map_chr(str_split(rules, ": "), function(x) x[1]) 
vtick <- tickets[map_dbl(tickets, ~mean(as.numeric(str_split(., ",")[[1]]) %in% valid)) == 1] %>%
read_csv(col_names = F)


mapping <- list()
for (rn in 1:length(rule_name)){
  v <- c(valid_list[[2*rn - 1]], valid_list[[2*rn]])
  mapping[rule_name[rn]] <- c(21)
  for (col in 1:length(rule_name)){
    if (length(setdiff(vtick[[col]], v)) == 0){
      print(paste(rn, col))
      mapping[[rule_name[rn]]] <- c(mapping[[rule_name[rn]]], col)
    }
  }
}
mapping <- map(mapping, function(x) x[x!=21])

mapping2 <- c()
while(length(mapping2) != length(rule_name)){
  name <- names(mapping)
  for (out in which(map_dbl(mapping, length) == 1)){
    print(paste(name[out], mapping[[out]]))
    mapping2[names(mapping[out])] <- mapping[[out]]
    mapping <- map(mapping, function(x) x[x!=mapping[[out]]])
  }
}

yours <- as.numeric(str_split(yours, ",")[[1]])
prod(yours[mapping2[substr(names(mapping2), 1, 5) == 'depar']])


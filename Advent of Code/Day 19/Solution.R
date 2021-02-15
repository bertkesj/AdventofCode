library(tidyverse)

xx <- readLines("input.txt")
w <- which(xx == '')
x <- xx[1:(w-1)]
test <- xx[(w+1):length(xx)]
x <- str_remove_all(x, '"')
rules <- lapply(str_split(x, ":"), `[`, 2)
rules <- lapply(rules, paste0, ' ')
names(rules) <- sapply(str_split(x, ":"), `[`, 1)


while (str_detect(rules['0'], '[0-9]')){
  for (r in str_extract_all(rules['0'], '[0-9]+')[[1]]){
    rules['0'] <- str_replace_all(rules['0'], paste0(' ', r, ' '),
                                  fixed(paste0(" (", rules[r], ") ")))
    
  }
  
}
comp <- str_remove_all(rules['0'][[1]], ' ')
comp <- paste0('^', comp, '$')

#Part 1
sum(str_detect(test, comp))






#Part 2
rules <- lapply(str_split(x, ":"), `[`, 2)
rules <- lapply(rules, paste0, ' ')
names(rules) <- sapply(str_split(x, ":"), `[`, 1)
rules['8'] <- ' 42 | 42 {r} | 42 {s} | 42 {t} | 42 {u} '
rules['11'] <- ' 42 31 | 42 {r} 31 {r} | 42 {s} 31 {s} | 42 {t} 31 {t} | 42 {u} 31 {u}'
step <- 0
while (str_detect(rules['0'], '[0-9]')){
  for (r in str_extract_all(rules['0'], '[0-9]+')[[1]]){
    rules['0'] <- str_replace_all(rules['0'], paste0(' ', r, ' '),
                                  fixed(paste0(" (", rules[r], ") ")))
    
  }
  print(rules['0'])
  step <- step + 1
  print(step)
}
comp <- str_remove_all(rules['0'][[1]], ' ') %>%
  str_replace_all('r', '2') %>%
  str_replace_all('s', '3') %>%
  str_replace_all('t', '4') %>%
  str_replace_all('u', '5')
comp <- paste0('^', comp, '$')

#not 385, 396 answer:374
sum(str_detect(test, comp))



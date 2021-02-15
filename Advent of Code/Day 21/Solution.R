library(tidyverse)

x <- readLines("input.txt") %>%
  str_remove_all("\\(|\\)") %>%
  str_split('contains')
xx <- tibble(ingredients =  lapply(x, `[`, 1) %>%
               str_trim() %>%
               str_split(' '),
             allergen = lapply(x, `[`, 2) %>%
               str_split(',') %>%
               lapply(str_trim))
ing <- xx %>%
  separate_rows(allergen) %>%
  filter(!allergen %in% c('', 'c')) %>%
  group_by(allergen) %>%
  summarize(poss = list(reduce(ingredients, intersect)))
notposs <- setdiff(poss, unique(unlist(ing$poss)))

#Part 1
ct <- c()
for (i in notposs){
  n <- 0
  for (rec in seq_along(xx$ingredients)){
    if (i %in% xx[rec,]$ingredients[[1]]) n <- n + 1
  }
  ct <- c(ct, n)
}
sum(ct)

#Part 2
final <- tibble()
while (!nrow(ing) == 0){
  new <- ing %>%
    filter(map_int(poss, length) == 1) %>%
    mutate(ing = unlist(poss))
  ing <- ing %>%
    filter(!map_int(poss, length) == 1)
  rm_un <- 
  for (i in new$ing){
    ing <- ing %>%
      mutate(poss = map(poss, ~ .x[!.x == i]))
  }
  final <- rbind(final, new)
}
final %>%
  arrange(allergen) %>%
  `$`(ing) %>%
  paste(collapse=',')



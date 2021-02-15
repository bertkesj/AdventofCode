library(tidyverse)

n <- tibble()
x <-
  paste(
    readLines(
      "C:/Users/bertk/OneDrive/Desktop/Advent of Code/Day 4/input.txt"
    ),
    collapse = "\n"
  )
x <- str_split(x, '\n\n')
x <- str_replace_all(x[[1]], '\n', " ")
x <- str_split(x, ' ')

#part 1
data <- tibble()
for (line in x) {
  new <- c()
  for (i in str_split(line, ':')) {
    new[i[1]] = !is.na(i[2])
  }
  data <- bind_rows(data, as_tibble(t(new)))
}
valid <- data %>%
  select(-cid) %>%
  rowSums()
sum(!is.na(valid))

#part 1.1
data <- tibble()
for (line in x) {
  new <- c()
  for (i in str_split(line, ':')) {
    new[i[1]] = i[2]
    if (i[1] == 'pid') {
      if (nchar(i[2]) == 9) {
        if (!is.na(as.numeric(i[2]))) {
          new['check'] = 1 * (as.numeric(i[2]) < 999999999)
        }
      }
    }
  }
  data <- bind_rows(data, as_tibble(t(new)))
}




#part 2
valid <- 0
for (line in x) {
  v <- 0
  #print(x)
  for (i in str_split(line, ':')) {
    if (i[1] == 'byr') {
      v <- v + (between(as.numeric(i[2]), 1920, 2002))
    }
    if (i[1] == 'iyr') {
      v <- v + (between(as.numeric(i[2]), 2010, 2020))
    }
    if (i[1] == 'eyr') {
      v <- v + (between(as.numeric(i[2]), 2020, 2030))
    }
    if (i[1] == 'hgt') {
      if (str_count(i[2], 'cm')) {
        v <- v + (between(as.numeric(str_remove(i[2], 'cm')), 150, 193))
      }
      if (str_count(i[2], 'in')) {
        v <- v + (between(as.numeric(str_remove(i[2], 'in')), 59, 76))
      }
    }
    if (i[1] == 'hcl') {
      if (substr(i[2], 1, 1) == '#') {
        v <- v + (str_count(str_remove(i[2], '#'), '[0-9a-f]+') >= 1)
      }
    }
    if (i[1] == 'ecl') {
      v <- v + (str_count(i[2], 'amb|blu|brn|gry|grn|hzl|oth') >= 1)
    }
    if (i[1] == 'pid') {
      if (nchar(i[2]) == 9) {
        if (!is.na(as.numeric(i[2]))) {
          v <- v + (as.numeric(i[2]) < 999999999)
        }
      }
    }
  }
  valid <- valid + (v == 7)
}
valid

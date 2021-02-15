library(readr)
numbers <- read_csv("numbers.txt", 
                    col_names = FALSE)

for (first in 1:length(numbers$X1)){
  for (second in 1:length(numbers$X1)){
    #print(numbers$X1[first] + numbers$X1[second])
    if (numbers$X1[first] + numbers$X1[second] == 2020) {
      print(first)
      print(second)
      print(numbers$X1[first])
      print(numbers$X1[second])
      print(numbers$X1[first]*numbers$X1[second])
    }
  }
}


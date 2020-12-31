#!/usr/bin/env Rscript

library(data.table)

data <- readLines('input.txt')
data <- as.numeric(data)

### PART ONE

preamble <- 25
current.count <- preamble + 1

while (data [current.count] 
       %in% 
       unlist(
         lapply(
           data[(current.count-preamble):(current.count-1)], 
           function (this) this + data[(current.count-preamble):(current.count-1)][which (this != data[(current.count-preamble):(current.count-1)])]))) {
  current.count <- current.count + 1
}

print(paste('The first number that does not meet property is', data [current.count]))

### PART TWO

identical.sums <- sapply(1:length(data), function (this.try) {
  current.row <- this.try
  current.sum <- 0
  while (current.sum < data [current.count]) {
    current.sum <- current.sum + data[current.row]
    current.row <- current.row + 1
  }
  ifelse(current.sum==data[current.count], return (current.row-this.try), return (0))
})

longestrange <- data[which (identical.sums==max(identical.sums)):(which (identical.sums==max(identical.sums))+max(identical.sums))]

print (paste('The encryption weakness is', min(longestrange) + max(longestrange)))

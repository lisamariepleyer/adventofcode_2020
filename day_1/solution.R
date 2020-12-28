#!/usr/bin/env Rscript

library(data.table)

data <- fread('input.txt')
data <- as.vector(data$V1)

### PART ONE
results <- sapply(data, function (x) {
  added <- x+data
  if (2020 %in% added) {
    return (data[which (added==2020)]*x)
  }
})

paste('The answer to part one is:', unique(unlist(results)))

### PART TWO
results <- sapply(data, function (x) {
  results.two <- sapply(data, function (y) {
    added <- x+y+data
    if (2020 %in% added) {
      return (data[which (added==2020)]*x*y)
    }
  })
})

paste('The answer to part two is:', unique(unlist(results)))

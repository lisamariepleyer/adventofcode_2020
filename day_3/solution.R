#!/usr/bin/env Rscript

library(data.table)

data <- fread('input.txt', header = F)
data <- do.call (rbind, strsplit(data$V1, ''))
matr <- ifelse(data=='.', 0, 1)

### PART ONE
x <- 1
y <- 1
while (y <= nrow (matr)) {
  matr [y,x] <- matr[y,x] + 1
  x <- x + 3
  y <- y + 1
  
  if (x > ncol(matr)) {
    x <- x - ncol(matr)
  }
}

paste(length(which (matr==2)), 'trees are encountered.')

### PART TWO 
slopes <- list (c(1,1), c(3,1), c(5,1), c(7,1), c(1,2))

results <- lapply(slopes, function (this.slope) {
  matr <- ifelse(data=='.', 0, 1)
  
  x <- 1
  y <- 1
  while (y <= nrow (matr)) {
    matr [y,x] <- matr[y,x] + 1
    x <- x + this.slope[1]
    y <- y + this.slope[2]
    
    if (x > ncol(matr)) {
      x <- x - ncol(matr)
    }
  }
  return(length(which (matr==2)))
})

paste ('The product of all trees encountered is', prod (unlist(results)))

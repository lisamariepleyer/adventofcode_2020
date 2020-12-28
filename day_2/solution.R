#!/usr/bin/env Rscript

library(data.table)

data <- fread('input.txt', header = F)
colnames(data) <- c('range', 'sug.letter', 'password')

# make sure all ranges contain 2 numbers
# unique(lengths(strsplit(data$range, '-')))
data[, c("min", "max") := tstrsplit(range, "-")]
#invisible(sapply(c('min', 'max'), function (thiscol) {data [[thiscol]] <- as.integer(data [[thiscol]])}))

# make sure letter column contains only one letter
#unique(lengths(strsplit(data$sug.letter, '')))
# ... and the second element is always a semicolon
#unique(sapply(strsplit(data$sug.letter, ''), "[[", 2))
data [, letter := sapply(strsplit(sug.letter, ''), '[[',1)]
data [, c('range', 'sug.letter') := NULL]

### PART ONE
data [, num.occurrences := sapply(1:nrow(data), function (this) {
  return (length(which(strsplit(password[this], '')[[1]]==letter[this])))
})]

data [, valid.pt1 := sapply (1:nrow (data), function (this) {
  ifelse(num.occurrences[this] %in% min[this]:max[this], T, F)
})]

paste (sum (data$valid.pt1), 'passwords are valid.')

### PART TWO
data [, occurrences.atpositions := sapply(1:nrow(data), function (this) {
  split.pw <- strsplit(password[this], '')[[1]]
  letters.atpositions <- c(split.pw[as.integer(min[this])], split.pw[as.integer(max[this])])
  return (length(which (letters.atpositions==letter[this])))
})]

paste (nrow(data[occurrences.atpositions==1]), 'passwords are valid.')

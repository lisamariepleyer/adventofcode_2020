#!/usr/bin/env Rscript

library(data.table)

data <- readLines ('input.txt')
data <- paste(data, collapse = ' ')
data <- data.table(answers.per.group=strsplit(data, '  ')[[1]])

### PART ONE
data [, yes.per.group := sapply(answers.per.group, function (group.answers) {
  length(
    unique(
      unlist(
        strsplit(
          strsplit(group.answers, ' ')[[1]], ''))))
})]

print(paste("sum of questions answered 'yes' across all groups is", sum(data$yes.per.group)))

### PART TWO
data [, common.yes.per.group := sapply(answers.per.group, function (group.answers) {
  length(
    which(
      table(
        unlist(
          strsplit(
            strsplit(group.answers, ' ') [[1]], ''))) == length(strsplit(group.answers, ' ') [[1]])))
})]

print(paste("sum of common questions answered 'yes' across all groups is", sum(data$common.yes.per.group)))



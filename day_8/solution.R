#!/usr/bin/env Rscript

library(data.table)

data <- fread ('input.txt', header = F)
colnames(data) <- c('operation', 'argument')
#data [, sign := '+'][grep ('-', argument), sign := '-']
data [, touched := F]

### PART ONE

position <- 1
accumulator <- 0
retouched <- F
while (retouched == F) {
  if (data [position, touched == F]) {
    data [position, touched := T]
    if (data [position, operation] == 'nop') {
      position <- position + 1
    } else if (data [position, operation] == 'jmp') {
      position <- position + data [position, argument]
    } else if (data [position, operation] == 'acc') {
      accumulator <- accumulator + data [position, argument]
      position <- position + 1
    } else {
      print ('unknown operation')
    }
  } else {
    print (paste('Value in accumulator is:', accumulator))
    retouched <- T
  }
}

### PART TWO

input.operators <- data$operation

result <- sapply(grep ('nop|jmp', input.operators), function (try.num) {
  # reset data
  data [, touched := F]
  data$operation <- input.operators
  position <- 1
  accumulator <- 0
  
  # change current operator
  data [try.num, operation := ifelse(operation=='nop', 'jmp', 'nop')]
  
  retouched <- F
  while (retouched == F & position <= nrow (data)) {
    if (data [position, touched == F]) {
      data [position, touched := T]
      if (data [position, operation] == 'nop') {
        position <- position + 1
      } else if (data [position, operation] == 'jmp') {
        position <- position + data [position, argument]
      } else if (data [position, operation] == 'acc') {
        accumulator <- accumulator + data [position, argument]
        position <- position + 1
      } else {
        print ('unknown operation')
      }
    } else {
      retouched <- T
    }
  }
  
  ifelse(position > nrow (data), return (accumulator), return (NULL))
})

print(paste('The accumulator value after the program terminates is', unlist(result)))








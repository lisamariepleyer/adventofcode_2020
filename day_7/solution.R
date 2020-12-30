#!/usr/bin/env Rscript

library(data.table)

rules <- readLines ('input.txt')

# make sure colour descriptions are only two words long
# unique(sapply(strsplit(rules, ' '), function(this) this [3]))

### PART ONE

# function to fetch all lines that mention given colour anywhere but in the beginning
contained.in.colours <- function(thiscol) {
  contained.in <- grep (thiscol, rules)[-which (grep (thiscol, rules)==grep (paste0('^', thiscol), rules))]
  return(contained.in)
}

find.parents <- function(colour) {
  success <- F
  this.set <- unlist( # set of colours that can contain a given bag
    lapply(
      strsplit(rules [contained.in.colours(colour)], ' '), 
      function (thisone) 
        paste(thisone [1:2], collapse = ' ')))
  
  possible.colours <- c(length(this.set))
  
  while (!success) {
    new.set <- unique(
      unlist(
        lapply(this.set, function (this.col) {
          unlist(
            lapply(
              strsplit(rules [contained.in.colours(this.col)], ' '), 
              function (split.this) 
                paste(split.this [1:2], collapse = ' ')))})))
    
    possible.colours <- c(possible.colours, sum(!new.set %in% this.set)) # if not already in list, add colours that can contain given colour
    success <- sum(!new.set %in% this.set) == 0 # set success TRUE if all colours have been found before
    this.set <- unique(c(this.set, new.set))
  }
  return (possible.colours)
}

paste(sum (find.parents('shiny gold')), 'colours can eventually contain a shiny gold bag.')

### PART TWO

# prep data
split.rules <- strsplit(rules, "contain |, ")
rules <- lapply(split.rules, 
                function (split.this) {
                  sub.dt <- as.data.table(
                    do.call(rbind, 
                            lapply(strsplit(split.this [2:length(split.this)], ' '), 
                                   function (split.this.this) c(split.this.this[1], paste(split.this.this[2:3], collapse = ' ')))))
                  colnames(sub.dt) <- c('quan', 'bag')
                  return (sub.dt)})
names(rules) <- lapply(split.rules, 
                       function (split.this) paste(strsplit(split.this [1], ' ') [[1]] [1:2], collapse = ' '))

# in the first round, we have e.g. two red bags in our shiny gold bag, 
# the red bags contain two orange bags each,
# the orange bags contain two yellow bags each. 
# we can add a counter and multiply by the number of parent bags.
# we are not interested in the level we are currently in, because only 
# the abslute number of parent bags is required to calculate the 
# absolute number of children bags.

find.children <- function (colour) {
  success <- F
  this.set <- rules[[colour]]
  bag.counter <- c(sum(as.integer(this.set$quan)))
  
  while (!success) {
    this.set <- do.call (rbind,
                         lapply(1:nrow(this.set), 
                                function (this.row) {
                                  quan.parent <- this.set [this.row, as.integer(quan)]
                                  daughter.bags <- rules [[this.set [this.row, bag]]]
                                  if (daughter.bags$quan [1] != 'no') {
                                    daughter.bags$quan <- quan.parent*as.integer(daughter.bags$quan)
                                    return (daughter.bags)
                                  }
                                }))
    bag.counter <- c(bag.counter, as.integer(sum (this.set$quan)))
    success <- is.null(this.set)
  }
  return (bag.counter)
}

paste (sum (find.children('shiny gold')), 'individual bags are required in a single shiny gold bag.')





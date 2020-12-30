#!/usr/bin/env Rscript

library(data.table)

rules <- readLines ('input.txt')

# make sure colour descriptions are only two words long
# unique(sapply(strsplit(rules, ' '), function(this) this [3]))

contained.in.colours <- function(thiscol) {
  contained.in <- grep (thiscol, rules)[-which (grep (thiscol, rules)==grep (paste0('^', thiscol), rules))]
  return(contained.in)
}

### PART ONE

col.from.shiny.gold <- unlist(
  lapply(
    strsplit(rules [contained.in.colours('shiny gold')], ' '), 
    function (thisone) 
      paste(thisone [1:2], collapse = ' ')))

this.func <- function() {
  success <- F
  this.set <- col.from.shiny.gold
  
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
    
    possible.colours <- c(possible.colours, sum(!new.set %in% this.set))
    success <- sum(!new.set %in% this.set) == 0
    this.set <- unique(c(this.set, new.set))
  }
  return (possible.colours)
}

paste(sum (this.func()), 'colours can eventually contain a shiny gold bag.')

### PART TWO







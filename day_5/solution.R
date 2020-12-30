#!/usr/bin/env Rscript

library(data.table)

data <- fread ('input.txt', header = F)
setnames(data, 'V1', 'binary.partitioning')

allrows <- 0:127
allcolumns <- 0:7

calc.row.and.col <- function(bin.part) {
  bin.part.vector <- strsplit(bin.part, '') [[1]]
  thisrow <- allrows
  thiscol <- allcolumns
  
  for (sub.part in bin.part.vector) {
    if (sub.part == 'F') {
      thisrow <- thisrow[1:((length(thisrow))/2)]
    } else if (sub.part == 'B') {
      thisrow <- thisrow[((length(thisrow))/2+1):length(thisrow)]
    } else if (sub.part == 'R') {
      thiscol <- thiscol [((length(thiscol))/2+1):length(thiscol)]
    } else if (sub.part == 'L') {
      thiscol <- thiscol [1:(length(thiscol)/2)]
    } else {
      print (paste('binary code contains unknown letter:'))
    }
  }
  return (c(thisrow, thiscol))
}

data <- cbind (data, 
               as.data.table(
                 matrix(
                   unlist(lapply(data$binary.partitioning, calc.row.and.col)), 
                 ncol = 2,  byrow = TRUE)))
setnames(data, 'V1', 'row'); setnames(data, 'V2', 'column')

### PART ONE
data [, seatID:=row*8+column]
paste ('The highest seat ID is:', max (data$seatID))

### PART TWO
all.possible.ID <- 0:(max(allrows)*8+max(allcolumns)) #c(0,1023)
#overlapping.seatIDs <- all.possible.ID %in% data$seatID

for (x in 1:length(all.possible.ID)) {
  if ((all.possible.ID[x] %in% data$seatID)==F) {
    if (x==1 | x==length(all.possible.ID)) {}
    else if(all.possible.ID[x-1] %in% data$seatID & all.possible.ID[x+1] %in% data$seatID) {
      print (paste('The ID of my seat is', all.possible.ID[x]))
    }
  }
}



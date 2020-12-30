#!/usr/bin/env Rscript

library(data.table)

#input <- as.data.table(read.csv('input.txt', header = F, sep = "\n", skipNul = F, skip = F))
input <- readLines('input.txt')
fields <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid', 'cid')

input <- paste(input, collapse = ' ')
input <- data.table(newline.separated.data=strsplit(input, '  ')[[1]])

data <- lapply(1:nrow(input), function (thisrow) {
  split.row <- strsplit(input$newline.separated.data [thisrow], ' ') [[1]]
  matches <- sapply(fields, function(thisfield) {
    if (length(grep (paste0(thisfield, ':'), split.row))!=0) {
      return(strsplit(split.row [grep (paste0(thisfield, ':'), split.row)], ':') [[1]] [2])
    }
    else {
      return(NA)
    }
  })
})

data <- data.table(do.call(rbind, data))

### PART ONE
paste(sum(complete.cases(data[,.SD, .SDcols = !'cid'])), 'passports contain complete info.')

### PART TWO
data [,row.num := 1:nrow(data)]
data [, `:=` (hgt.unit = sapply(strsplit(hgt, '[0-9]'), 
                                function(z){ z[!is.na(z) & z != ""]}),
              hgt.value = sapply(strsplit(hgt, '\\D'),
                                 function(z){ z[!is.na(z) & z != ""]}))]

keep.those <- c(data [complete.cases(data[,.SD, .SDcols = !c('cid', 'hgt.unit', 'hgt.value', 'row.num')]), row.num], #cid (Country ID) - ignored, missing or not.
                data [nchar(byr)==4 & byr >= 1920 & byr <= 2002, row.num], #byr (Birth Year) - four digits; at least 1920 and at most 2002.
                data [nchar(iyr)==4 & iyr >= 2010 & iyr <= 2020, row.num], #iyr (Issue Year) - four digits; at least 2010 and at most 2020.
                data [nchar(eyr)==4 & eyr >= 2020 & eyr <= 2030, row.num], #eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
                data [!pid %like% '\\D' & nchar(pid)==9, row.num], #pid (Passport ID) - a nine-digit number, including leading zeroes.
                data [ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'), row.num], #ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
                data [hcl %like% '#[0-9|a-f]{6}', row.num], #hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
                unique(c(data [hgt.unit=='cm' & hgt.value >=150 & hgt.value <=193, row.num], #hgt (Height) - a number followed by either cm or in: If cm, the number must be at least 150 and at most 193.
                         data [hgt.unit=='in' & hgt.value >=59 & hgt.value <=76, row.num]))) #If in, the number must be at least 59 and at most 76.

paste(length(which(table(keep.those)==8)), 'passports contain correct information.')








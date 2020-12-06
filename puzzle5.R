# Puzzle 5 -- the most bizarre seating system
library(tidyverse)
library(testthat)
# Start by considering the whole range, rows 0 through 127.
# F means to take the lower half, keeping rows 0 through 63.
# B means to take the upper half, keeping rows 32 through 63.
# F means to take the lower half, keeping rows 32 through 47.
# B means to take the upper half, keeping rows 40 through 47.
# B keeps rows 44 through 47.
# F keeps rows 44 through 45.
# The final F keeps the lower of the two, row 44.

split_divide = function(range){
  mid = (range[2] - range[1])/2
  
  list(F = ceiling(c(range[1], range[1]+mid-1)), 
       B = ceiling(c(range[2]-mid, range[2]))
       )
}

get_seat <- function(code) {
  direction <- code %>% str_split("", simplify = T)
  n <- 1
  ranges <- list()
  ranges[[1]] <- c(0, 127)

  ### row!
  while (n < 8) {
    range <- split_divide(ranges[[n]])[direction[n]][[1]]
    n <- n + 1
    ranges[[n]] <- range
  }

  row <- unique(range)
  range <- c(0, 7)

  while (n < 11) {
    split <- split_divide(range)
    names(split) <- c("L", "R")

    range <- split[direction[n]][[1]]
    n <- n + 1
    ranges[[n]] <- range
  }

  column <- unique(range)

  seat <- row * 8 + column

  tibble(row, column, seat)
}

### tests

expect_equal(get_seat("BFFFBBFRRR")$seat, 567)
expect_equal(get_seat("FFFBBBFRRR")$seat, 119)
expect_equal(get_seat("BBFFBBFRLL")$seat, 820)

### input! 
input <- read_lines("puzzle5_input.txt")
seats <- map_df(input, get_seat)
max(seats)


#### part 2
## not elegant but works. 
library(magrittr)
ordered <- seats %>% 
  arrange(seat) %>% 
  mutate(diff = c(diff(seat), NA))

ordered[which(ordered$diff== 2),]$seat + 1

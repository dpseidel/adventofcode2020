library(tidyverse)

# proof
test <- c(
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"
)

# givens
start <- c(1, 1)
slope <- c(1, 3)

# Given the slope, I can tell you every coordinate you want based on the length of the input array

# Then the inefficient way would be to rep every line such that you have 3 times the "columns"
# than you have "rows"
# but i think using remainders, I can make thing "wrap"
# I think i can use the coords and modulus to identify the trees.

# Additionally, 1 approach would be to explode it out to a full matrix so that I can do a simple
# logical comparison
# but i think i could do the same with regexp

# test
coords <- tibble(
  pattern = test,
  x = seq(1, length.out = length(test), by = 1),
  y = seq(1, length.out = length(test), by = 3),
  idx = if_else(y %% length(test) == 0, length(test), y %% length(test)),
) %>%
  mutate(
    square = str_sub(pattern, start = idx, end = idx),
    tree = square == "#"
  )

sum(coords$tree)

tobogganing <- function(array, start, slope) {
  pattern_length <- str_count(array[1], ".|#")

  tibble(
    pattern = array,
    x = seq(start[1], length.out = length(array), by = slope[1]),
    y = seq(start[2], length.out = length(array), by = slope[2]),
    idx = ifelse(y %% pattern_length == 0, pattern_length, y %% pattern_length)
  ) %>%
    mutate(
      square = str_sub(pattern, start = idx, end = idx),
      tree = square == "#"
    )
}

tobogganing(test, start, slope)$tree %>% sum()

##
input <- read_lines("puzzle3_input.txt")
tobogganing(input, start, slope)$tree %>% sum()

## part 2
# check more slopes!

slopes <- list(
  c(1, 1),
  c(1, 3),
  c(1, 5),
  c(1, 7),
  c(2, 1)
)

# with proof
map(slopes, ~ tobogganing(test, start, .x))
map_dbl(slopes, ~ tobogganing(test, start, .x)$tree %>% sum()) %>% prod()
## seems to work here, but i'm suspicious

# with inputss
map_dbl(slopes, ~ tobogganing(input, start, .x)$tree %>% sum())

#### hmmm my function doesnt handle "steep" slopes -- those that go down more than 1
# each turn... because i'm not filtering down my pattern rows to those applicable...
# must add logic.

tobogganing_full_throttle <- function(array, start, slope) {
  
  pattern_length <- str_count(array[1], ".|#")
  x = seq(start[1], to = length(array), by = slope[1])

  tibble(
    pattern = array[x],
    x = x,
    y = seq(start[2], length.out = length(x), by = slope[2]),
    idx = ifelse(y %% pattern_length == 0, pattern_length, y %% pattern_length)
  ) %>%
    mutate(
      square = str_sub(pattern, start = idx, end = idx),
      tree = square == "#"
    )
}


# with proof
map(slopes, ~ tobogganing_full_throttle(test, start, .x))
map_dbl(slopes, ~ tobogganing_full_throttle(test, start, .x)$tree %>% sum()) %>% prod()
## works!

# with inputs
map_dbl(slopes, ~ tobogganing_full_throttle(input, start, .x)$tree %>% sum()) %>% prod()

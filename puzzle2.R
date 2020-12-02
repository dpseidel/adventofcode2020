library(tidyverse)
library(testthat)

### Part 1
test1 <- c(
  "1-3 a: abcde",
  "1-3 b: cdefg",
  "2-9 c: ccccccccc"
)

sled_password <- function(input) {
  input %>%
    str_split(" ", simplify = T) %>%
    as_tibble() %>%
    transmute(
      min = as.numeric(sub("(.*)-.*", "\\1", V1)),
      max = as.numeric(sub(".*-(.*)", "\\1", V1)),
      key = str_remove(V2, ":"),
      password = V3,
      key_count = map_dbl(str_match_all(password, key), nrow),
      is_legal = key_count >= min & key_count <= max
    )
}

sled_password(test1)
expect_equal(sled_password(test1)$is_legal, c(T, F, T))

input <- read_lines("puzzle2_input.txt")

sled_password(input) %>%
  pull(is_legal) %>%
  sum()

## Part2
toboggan_password <- function(input) {
  input %>%
    str_split(" ", simplify = T) %>%
    as_tibble() %>%
    transmute(
      pos1 = as.numeric(sub("(.*)-.*", "\\1", V1)),
      pos2 = as.numeric(sub(".*-(.*)", "\\1", V1)),
      key = str_remove(V2, ":"),
      password = V3,
      in_pos1 = str_sub(password, start = pos1, end = pos1) == key,
      in_pos2 = str_sub(password, start = pos2, end = pos2) == key,
      is_valid = (in_pos1 + in_pos2) == 1
    )
}

toboggan_password(input) %>%
  pull(is_valid) %>%
  sum()

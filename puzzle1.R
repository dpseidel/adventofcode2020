library(tidyverse)
library(testthat)

## proof
test1 <- c(1721, 979, 366, 299, 675, 1456)

parse_expense_report <- function(vector) {
  t <- combn(vector, m = 2) %>%
    t() %>%
    as_data_frame() %>%
    mutate(sum = V1 + V2)

  t[which(t$sum == 2020), ]$V1 * t[which(t$sum == 2020), ]$V2
}

expect_equal(parse_expense_report(test1), 514579)


## puzzle part 1

input <- read_table("puzzle1_input.txt", col_names = "v1")

parse_expense_report(input$v1)

## puzzle part 2

parse_general <- function(vector, m = 3) {
  t <- combn(vector, m = m) %>%
    t() %>%
    as_data_frame() %>%
    mutate(sum = rowSums(.))

  filter(t, sum == 2020) %>% 
    select(-sum) %>% 
    reduce(., `*`)
}

expect_equal(parse_general(input1, 2), 514579)
expect_equal(parse_general(input1, 3), 241861950)

parse_general(input$v1, 3)

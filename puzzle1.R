library(readr)
library(testthat)

## proof
test1 <- c(1721, 979, 366, 299, 675, 1456)

parse_expense_report <- function(vector, m = 2) {
  t <- combn(vector, m = m)
  prod(t[, which(colSums(t) == 2020)])
}

# test proof
expect_equal(parse_expense_report(test1), 514579)

## puzzle part 1
input <- read_table("puzzle1_input.txt", col_names = "v1")
parse_expense_report(input$v1)

## puzzle part 2
## test with new parameter `m`
expect_equal(parse_expense_report(test1, 2), 514579)
expect_equal(parse_expense_report(test1, 3), 241861950)

parse_expense_report(input$v1, 3)

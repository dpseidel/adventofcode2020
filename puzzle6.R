## puzzle 6

library(tidyverse)
library(testthat)

tester <- c("abc",
  NA,
  "a",
  "b",
  "c",
  NA,
  "ab",
  "ac",
  NA,
  "a",
  "a",
  "a",
  "a",
  NA,
  "b")

parsed <- tester %>%
  na_if("") %>%
  replace_na("/n") %>%
  paste(collapse = "") %>%
  str_split("/n", simplify = T) 

map_dbl(parsed, ~str_split(.x, "")[[1]] %>% unique %>% length) %>% sum() == 11

### input! 

input <- read_lines('puzzle6_input.txt')
parsed_input <- input %>%
  na_if("") %>%
  replace_na("/n") %>%
  paste(collapse = "") %>%
  str_split("/n", simplify = T)
map_dbl(parsed_input, ~str_split(.x, "")[[1]] %>% unique %>% length) %>% sum()

## part 2 -- not so fast! tricky tricky
group_parse <-  tester %>%
  na_if("") %>%
  replace_na("/n") %>%
  paste(collapse = " ") %>%
  str_split("/n")

## now I need to do the count of 
answer_check <- function(answer, n){
  
  counts <- str_split(answer, "")[[1]] %>% 
    na_if(" ") %>% na.omit() %>% 
    table()
  
  length(which(counts == n))
}

answers <- tibble(answer = trimws(group_parse[[1]])) %>% 
  mutate(n_members = str_count(answer, " ") + 1) %>% 
  rowwise() %>% 
  mutate(n_agreed = answer_check(answer, n_members))

expect_equal(sum(answers$n_agreed), 6)


### with input? 

group_parse <-  input %>%
  na_if("") %>%
  replace_na("/n") %>%
  paste(collapse = " ") %>%
  str_split("/n")

answers <- tibble(answer = trimws(group_parse[[1]])) %>% 
  mutate(n_members = str_count(answer, " ") + 1) %>% 
  rowwise() %>% 
  mutate(n_agreed = answer_check(answer, n_members))
sum(answers$n_agreed)

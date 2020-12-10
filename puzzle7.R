# puzzle 7 ---------------
library(tidyverse)
library(stringi)

test_rule_set <- c(
  "light red bags contain 1 bright white bag, 2 muted yellow bags.",
  "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
  "bright white bags contain 1 shiny gold bag.",
  "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
  "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
  "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
  "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
  "faded blue bags contain no other bags.",
  "dotted black bags contain no other bags."
)

# how many outer colors could contain a shiny gold bag?
str_detect(test_rule_set, "shiny gold")

what_contain_shiny_gold <- str_extract(
  test_rule_set,
  "\\w.*\\w+(?= bags.contain.*.shiny.gold.bag)"
)


### puzzle input 
p7 <- read_lines("input_files/puzzle7_input.txt")

p7[str_detect(p7, "shiny gold bag")]

what_shiny_gold_contain <- p7[str_detect(p7, "shiny gold bags contain")]

what_contain_shiny_gold <- str_extract(p7, "\\w.*\\w+(?= bags.contain.*.shiny.gold.bag)") %>%
  .[!is.na(.)]

can_contain <- function(color = "shiny gold", input = test_rule_set) {
  stringi::stri_extract_first_regex(input, paste0("\\w.*\\w+(?= bags.contain.*.", color, ".bag)")) %>%
    .[!is.na(.)]
}

can_contain()

n_can_contain <- function(color = "shiny gold", input = test_rule_set) {
  # init
  bags <- character(0)
  # loop
  while (!is_empty(color)) {
    color <- map(color, ~ can_contain(color = .x, input)) %>%
      flatten_chr() %>%
      unique()
    color <- color[!(color %in% bags)]
    bags <- c(bags, color)
  }

  return(length(bags))
}

testthat::expect_equal(n_can_contain(), 4)

n_can_contain(input = p7)
### the above is ridiculously slow.. but it does in fact work

### Part 2 -----------------------
bag_test <- str_extract_all(test_rule_set, "\\d{1,2} \\w* \\w*|no other bags")
names(bag_test) <- str_extract(test_rule_set, "^\\w* \\w*")

bag_test_df <- map_df(bag_test, function(x) {
  tibble(
    child_count = coalesce(as.numeric(str_extract(x, "\\d{1,2}")), 0),
    child_bag = str_extract(x, "[a-z]+.[a-z]+")
  )
}, .id = "parent_bag")

#### I took a peak @wcmbishop's solution because he is smarter than me
## and WOW I don't think I had ever built/seen a function that called itself.
# neat. Here's my adaptation

count_children <- function(df, bags, n_bags) {
  join_df <- tibble(parent_bag = bags, parent_count = n_bags)

  children <- df %>%
    filter(parent_bag %in% bags) %>%
    inner_join(join_df, by = "parent_bag") %>%
    mutate(n_children = parent_count * child_count)

  if (nrow(children) == 0) {
    return(NULL)
  } else {
    children <- bind_rows(
      children,
      count_children(df,
        bags = children$child_bag,
        n_bags = children$n_children
      )
    )
  }
  return(children)
}

sum <- count_children(bag_test_df, "shiny gold", 1)$n_children %>% sum
testthat::expect_equal(sum, 32)

#####

bags <- str_extract_all(p7, "\\d{1,2} \\w* \\w*|no other bags")
names(bags) <- str_extract(p7, "^\\w* \\w*")

df <- map_df(bags, function(x) {
  tibble(
    child_count = as.numeric(str_extract(x, "\\d{1,2}")),
    child_bag = str_extract(x, "[a-z]+.[a-z]+")
  )
}, .id = "parent_bag") %>%
  mutate(child_count = if_else(child_bag == "no other", 0, child_count))

count_children(df, "shiny gold", 1)$n_children %>% sum


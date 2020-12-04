library(tidyverse)

## Part 1
## proof!
passport_test <- c(
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
  "byr:1937 iyr:2017 cid:147 hgt:183cm",
  NA,
  "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
  "hcl:#cfa07d byr:1929",
  NA,
  "hcl:#ae17e1 iyr:2013",
  "eyr:2024",
  "ecl:brn pid:760753108 byr:1931",
  "hgt:179cm",
  NA,
  "hcl:#cfa07d eyr:2025 pid:166559648",
  "iyr:2011 ecl:brn hgt:59in"
)

req_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") # ignore that pesky "cid" field

parsed <- passport_test %>%
  na_if("") %>%
  replace_na("/n") %>%
  paste(collapse = " ") %>%
  str_split("/n")


testthat::expect_equal(map_lgl(
  parsed[[1]],
  function(x) {
    all(map_lgl(req_fields, ~ str_detect(x, .x)))
  }
), c(T, F, T, F))


### input!
input <- read_lines("puzzle4_input.txt") %>%
  na_if("") %>%
  replace_na("/n") %>%
  paste(collapse = " ") %>%
  str_split("/n")

map_lgl(
  input[[1]],
  function(x) {
    all(map_lgl(req_fields, ~ str_detect(x, .x)))
  }
) %>% sum()

#### Part 2 -- gotta add some data checks!
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

input[[1]] %>%
  map_df(~ str_split(.x, " ", simplify = T) %>%
    str_split(":", simplify = T) %>%
    as_tibble() %>%
    transmute(field = V1, value = V2) %>%
    filter(field != "") %>%
    pivot_wider(names_from = field, values_from = value),
  .id = "passport"
  ) %>%
  group_by(passport) %>%
  transmute(
    ecl_valid = ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    pid_valid = str_detect(pid, "^\\d{9}$"),
    eyr_valid = as.numeric(eyr) >= 2020 & as.numeric(eyr) <= 2030,
    hcl_valid = str_detect(hcl, "^#([a-f0-9]{6})$"),
    byr_valid = as.numeric(byr) >= 1920 & as.numeric(byr) <= 2002,
    iyr_valid = as.numeric(iyr) >= 2010 & as.numeric(iyr) <= 2020,
    hgt_valid =
      case_when(
        str_detect(hgt, "cm") &
          as.numeric(str_extract(hgt, "([0-9]+)")) >= 150 &
          as.numeric(str_extract(hgt, "([0-9]+)")) <= 193 ~ T,
        str_detect(hgt, "in") &
          as.numeric(str_extract(hgt, "([0-9]+)")) >= 59 &
          as.numeric(str_extract(hgt, "([0-9]+)")) <= 76 ~ T,
        TRUE ~ FALSE
      )
  ) %>% 
  mutate_at(., vars(-group_cols()), coalesce, FALSE) %>%
  mutate(valid = all(
    ecl_valid,
    pid_valid,
    eyr_valid,
    hcl_valid,
    byr_valid,
    iyr_valid,
    hgt_valid
  )) %>% 
  pull(valid) %>% sum

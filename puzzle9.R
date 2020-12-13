# puzzle 9
library(tidyverse)
preamble5 <- c(35L, 20L, 15L, 25L, 47L, 40L, 62L, 55L, 65L, 95L, 102L, 117L,
               150L, 182L, 127L, 219L, 299L, 277L, 309L, 576L)



check_XMAS <- function(code, preamble){

  for (i in (preamble+1):length(code)){
    
    check <- code[i] %in% (code[(i-preamble):(i-1)] %>% combn(., 2) %>% colSums())
    
    if(!any(check)) break 
      
  }

  return(code[i])
  
}

check_XMAS(preamble5, 5)
testthat::expect_equal(check_XMAS(preamble5, 5), 127)

input <- read_lines("input_files/puzzle9_input.txt") %>% as.numeric()

check_XMAS(input, 25)
# Answer --> 1398413738

## Part 2 -- find a contiguous series that adds to this. 

sum_XMAS <- function(code, check = 1398413738){
  for (start in seq_along(code)){
    s <- 0
    for (i in start:length(code)) {
       s <- s + code[i]
       if (s >= check) {break}
    }
    if ((s == check) & (i-start > 0)){
      return(min(code[start:i]) + max(code[start:i]))
    }
  } 
}

testthat::expect_equal(sum_XMAS(preamble5, 127), 62)

sum_XMAS(input)



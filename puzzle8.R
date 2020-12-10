### day 8
test <- c(
  "nop +0",
  "acc +1",
  "jmp +4",
  "acc +3",
  "jmp -3",
  "acc -99",
  "acc +1",
  "jmp -4",
  "acc +6"
) %>%
  str_split(., " ", simplify = T) %>%
  tibble(operation = .[, 1], argument = as.numeric(.[, 2])) %>%
  select(operation, argument) %>%
  mutate(id = 1:n(), order = NA)


#### Part 1 =====
handheld_debugger <- function(commands) {
  i <- 1
  acc <- 0

  while (all(i != nrow(commands)+1) & is.na(commands$order[i])) {
    commands$order[i] <- i

    op <- commands$operation[i]
    arg <- commands$argument[i]

    if (op == "nop") {
      i <- i + 1
    }

    if (op == "acc") {
      acc <- acc + arg
      i <- i + 1
    }

    if (op == "jmp") {
      i <- i + arg
    }
  }

  Crash = (i != nrow(commands)+1)
  return(list(acc = acc, crash = Crash))
}

testthat::expect_equal(handheld_debugger(test)$acc, 5)

input <- read_lines("input_files/puzzle8_input.txt") %>%
  str_split(., " ", simplify = T) %>%
  tibble(operation = .[, 1], argument = as.numeric(.[, 2])) %>%
  select(operation, argument) %>%
  mutate(id = 1:n(), order = NA)

handheld_debugger(input)


#### Part 2 =====
# hunting nops and jmps


bug_hunter <- function(x){
  
  suspicious <- filter(x, operation != 'acc')
  
  for (j in 1:nrow(suspicious)){
    
    test = x
    record <- suspicious[j,]
    op <- record$operation
    
    test[test$id == record$id,]$operation <- if_else(op == 'nop', 'jmp', 'nop')
    
    result <- handheld_debugger(test)
    
    if(result$crash == FALSE) break
  }
  
  return(result)
}

bug_hunter(test)
testthat::expect_equal(bug_hunter(test)$acc, 8)
testthat::expect_equal(bug_hunter(test)$crash, FALSE)

## this might be slow..... fingers crossed

bug_hunter(input)

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome1 <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  ## Check that state and outcome are valid
  all_state_name <- dimnames(table(outcome1$State))[[1]]
  outcome_choice = c('heart attack', 'heart failure', 'pneumonia')
  order_state <- all_state_name[order(all_state_name)]
  if (!outcome %in% outcome_choice) stop ('Error in rankall(outcome, num): invalid outcome')
  n <- charmatch(outcome, outcome_choice)
  choice <- c(11, 17, 23)
  outcome1[, choice[n]] <- as.numeric(outcome1[, choice[n]])
  split_by_state <- split(outcome1[, c(2, choice[n], 7)], as.factor(outcome1$State))
  result <- data.frame(54, 2)
  ## For each state, find the hospital of the given rank
  for ( st in 1:54){
    a <- split_by_state[[st]]
    a <- a[order(a[, 1]),]
    a <- a[order(a[, 2]),]
    a <- a[!is.na(a[, 2]),]
    if (num == 'best')
      number = 1
    else if (num == 'worst')
      number = length(a[, 2])
    else
      number = num
  result[st, 1] <- a[number, 1]
  result[st, 2] <- order_state[st]
  a <- NULL
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnames(result) <- c('hospital', 'state')
  rownames(result) <- order_state
  result
}
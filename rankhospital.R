rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome1 <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  ## Check that state and outcome are valid
  if (!state %in% outcome1$State) stop ('Error in rankhospital(state, outcome): invalid state')
  sta = which( outcome1$State == state)
  dease = c('heart attack', 'heart failure', 'pneumonia')
  num1 = c(11, 17, 23)
  names(num1) = dease
  if (!outcome %in% names(num1)) stop ('Error in rankhospital(state, outcome): invalid outcome')
  ## Return hospital name in that state with the given rank
  ill <- as.numeric(outcome1[, num1[outcome]][sta])
  names(ill) <- outcome1$Hospital.Name[sta]
  ## 30-day death rate
  ill = sort(ill)
  if (num == 'worst')
    tail(names(ill), 1)
  else if(num == 'best')
    names(ill[1])
  else
    names(ill[num])
}
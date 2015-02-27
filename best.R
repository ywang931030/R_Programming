best <- function(state, outcome) {
  ## Read outcome data
  outcome1 <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  ## Check that state and outcome are valid
  if (!state %in% outcome1$State) stop ('Error in best (state, outcome): invalid state')
  sta = which( outcome1$State == state)
  dease = c('heart attack', 'heart failure', 'pneumonia')
  num = c(11, 17, 23)
  names(num) = dease
  if (!outcome %in% names(num)) stop ('Error in best (state, outcome): invalid outcome')
  ## Return hospital name in that state with lowest 30-day death
  ill <- as.numeric(outcome1[, num[outcome]][sta])
  names(ill) <- outcome1$Hospital.Name[sta]
  ## rate
  ill = sort(ill)
  names(ill[1])
}
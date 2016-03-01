best <- function(state, outcome) {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid
  stateList <- unique(outcomeDF[, 7])
  if(!(state %in% stateList)){
    stop("Invalid State")
  }  
  ## Check that outcome is valid
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){
    stop("Invalid Outcome - must be either heart attack, heart failure, or pneumonia")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
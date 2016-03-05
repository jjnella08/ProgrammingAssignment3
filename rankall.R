rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## Check that outcome is valid - no states are being passed
  if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){
    stop("Invalid Outcome - must be either heart attack, heart failure, or pneumonia")
  }
  
  ## order the dataframe hospital names in order to address ties in ratings
  outcomeDF <- outcomeDF[order(outcomeDF[2]),]
  
  ## create a named vector for the required outcomes and their column numbers
  mortCol <- c(11, 17, 23)
  names(mortCol) <- c('heart attack', 'heart failure', 'pneumonia')
  col <- mortCol[[outcome]]
  
  ## make sure the columns are numeric
  for(i in mortCol) {
    suppressWarnings(outcomeDF[,i] <- as.numeric(as.character(outcomeDF[,i])))
  }
  
  ## sort 30-day mortality rates by rank then by state
  outcomeDF <- outcomeDF[order(outcomeDF[col]),]
  outcomeDF <- outcomeDF[order(outcomeDF$State),]
  
  ## create a new data frame with just the needed columns
  rankDF <- outcomeDF[,c(2, 7, col)]
  for(i in unique(rankDF$State)) {
    for(j in 1:nrow(rankDF[rankDF$State == i,])) {
      rankDF$Rank[rankDF$State == i][j] <- j
    }
  }
  
  ## Rename the last column, get the mas number of rows per state
  colnames(rankDF)[3] <- 'Rate'
  numStates <- length(unique(outcomeDF$State))
  n <- 0
  
  ## get rank for each state based on the value of num
  if(num == 'best') {
    n <- as.list(rep(1, numStates))
  } else if(num == 'worst') {
    j <- 1
    for(i in unique(rankDF$State)) {
      n[j] <- which.max(rankDF$Rate[rankDF$State == i])
      j = j + 1
    }
    
  } else {
    n <- as.list(rep(num, numStates))
  }
  
  ## find matching rank by state
  Hospname <- rankDF[1,]
  j <- 1
  for(i in unique(rankDF$State)) {
    if(length(rankDF[rankDF$Rank == n[j] & rankDF$State == i,'State']) > 0) {
      Hospname[j,] <- rankDF[rankDF$Rank == n[j] & rankDF$State == i,]
    } else {
      Hospname[j,] <- c("<NA>", i, "<NA>", "<NA>")
    }
    j <- j + 1
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## set row names in the new matrix to the state name
  rownames(Hospname) <- Hospname[,2]
  colnames(Hospname) <- c('hospital', 'state')
  Hospname <- Hospname[,1:2]
  return(Hospname)
}
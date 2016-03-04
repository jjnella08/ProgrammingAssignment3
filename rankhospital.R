rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with lowest 30-day death?
  ## rate
  
  ## set the column index based in the outcome value
  colIdx <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  
  ## create a subset of the data for the specified state
  outcomeDF.sub1 <- outcomeDF[outcomeDF[,7] == state, ]
  
  ## create a subset of the State subset data for the specified outcome
  outcomeDF.sub2 <- outcomeDF.sub1[,c(2,7,colIdx)]
  
  ## create a subset of the State/Outcome subset data of valid numbers
  outcomeDF.sub3 <- outcomeDF.sub2[outcomeDF.sub2[,3]!="Not Available",]
  
  ## change the data to be numeric
  outcomeDF.sub3[,3] <- as.numeric(outcomeDF.sub3[,3])
  
  ## Sort the matrix by Hospital name prior to ranking in order to
  ## handle duplicates
  Sub3Sort <- outcomeDF.sub3[ order(outcomeDF.sub3[,1]), ]
  
  ## Add a column for rankings using "first" to break the ties
  Sub3Sort["RankCol"] <- rank(Sub3Sort[,3], ties.method = "first")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death 
  if (num == "best"){
    rIdx <- 1
  } else if (num == "worst"){
    rIdx <- max(Sub3Sort[, 4])
  } else if (num <= max(Sub3Sort[, 4])){
    rIdx <= num
  } else {
    return(NA)
  }
  return(Sub3Sort$Hospital.Name[rIdx])
}
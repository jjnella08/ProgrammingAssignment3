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
  
  ## find the row with the minimum value in the outcome column
  minRow <- outcomeDF.sub3[outcomeDF.sub3[,3] == min(outcomeDF.sub3[, 3]),]
  
  ## get the Hospital name from the first column
  HospName <- minRow[, 1]

  ## In case of a tie, order the Hospital names and then retrieve the
  ## Hospital Name by lowest alphabet order
  if (NROW(HospName) > 1){
    bestRow <- order(HospName[,1])
    return(bestRow)
  } else {           ## if only one row is returned
    return(HospName)
  }
  
}
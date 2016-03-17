best <- function(state, outcome) {
  ## Read outcome data
  bestRawData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",stringsAsFactors=FALSE)
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day deathrate
  
  if (outcome=="heart attack"){
    print("ha")
  }
  
  if (outcome=="heart failure") {
    print("hf")
  }
    
  if (outcome=="pneumonia") {
    print("p")
  }

  print(head(bestRawData$Hospital.Name))
}
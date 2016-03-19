best <- function(state, outcome) {
  ## Read outcome data
  bestRawData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available",stringsAsFactors=FALSE)
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day deathrate
  
  if (outcome=="heart attack"){
    bestSubset<-subset(bestRawData,select= c(Hospital.Name,State,11))
    
  }
  
  if (outcome=="heart failure") {
    bestSubset<-subset(bestRawData,select= c(Hospital.Name,State,17))
  }
    
  if (outcome=="pneumonia") {
    bestSubset<-subset(bestRawData,select= c(Hospital.Name,State,23))
  }

  names(bestSubset)<-c("Name","DesiredState","DesiredOutcome")

  bestSubset<-as.data.frame(na.omit(bestSubset))
  bestSubset<-bestSubset[bestSubset[,"DesiredState"]==state,]
  str(bestSubset)
  sorted<-bestSubset[order(bestSubset$DesiredOutcome),]
  str(sorted)
  print(head(sorted,5))
}
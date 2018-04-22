# R-Prog-Assignment 3 - 2 Finding the best hospital in a state

## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings. 

best <- function(state,outcome) {
  
  ## Read outcome data into a separate data frame
  
  ofcm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ocm <- data.frame(cbind(ofcm[,2],   # hospital name
                          ofcm[,7],   # State
                          ofcm[,11],  # heart attack
                          ofcm[,17],  # heart failure
                          ofcm[,23])) # pneumonia
  
  
  colnames(ocm) <- c("hospital","State","heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  
  if (!(any(state == ocm$State))) {
  stop("invalid state")
  }
  else if (!(any(outcome == c("heart attack","heart failure","pneumonia")))){
  stop("invalid outcome")}
  
  else{ 
  
  ## Return hospital name in that state with lowest 30-day death rate  
  
  ocm1 <- subset(ocm,State == state)
  ocm1[] <- lapply(ocm1,as.character) 
  
  if(outcome == "heart attack") {i <-3}
  else if(outcome == "heart failure") {i <-4}
  else {i <-5}
  
  m1 <- as.numeric((ocm1[,i]))
  min1 <- min(m1,na.rm = TRUE)
  rowmin <- which(m1 == min1)
  h <- ocm1[rowmin,1]
  h <- sort(h)
  return(h[1])
  }
}
  
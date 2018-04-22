# R-Prog-Assignment 3 - 3 Ranking hospitals by outcome in a state

## Write a function called rankhospital() that takes THREE (3) arguments: (a) the TWO(2)-character abbreviated name of a state (state); (b) an outcome (outcome); and © the ranking of a hospital in that state for that outcome (num). The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
  
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
    
    else  { 
      
      ## Return hospital name in that state with the given rank 30-day death rate  
      
      ocm1 <- subset(ocm,State == state)
      ocm1[] <- lapply(ocm1,as.character) 
      
      if(outcome == "heart attack") {
      ocm1 <- ocm1[ocm1$`heart attack` != 'Not Available', ]
      ocm2 <- ocm1[order(as.numeric(ocm1$`heart attack`),ocm1$hospital),]
      }
      else if(outcome == "heart failure") {
      ocm1 <- ocm1[ocm1$`heart failure` != 'Not Available', ]
      ocm2 <- ocm1[order(as.numeric(ocm1$`heart failure`),ocm1$hospital),]
      }
      else {
      ocm1 <- ocm1[ocm1$pneumonia != 'Not Available', ]
      ocm2 <- ocm1[order(as.numeric(ocm1$pneumonia),ocm1$hospital),]
      }

      if (num == "best"){
        h <- ocm2[1,1]
        return(h[1])
      }
      else if(num == "worst"){
        print(nrow(ocm2))
        h <- ocm2[nrow(ocm2),1]
        return(h[1])}
      
      else if (num > nrow(ocm2)){
        return(NA)}
        
        else{
        h <- ocm2[num,1]
        return(h[1])}
      }
    }
  



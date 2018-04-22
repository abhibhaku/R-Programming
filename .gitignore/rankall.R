# R-Prog-Assingment 3 - 4 Ranking hospitals in all states

## Write a function called rankall() that takes TWO (2) arguments: (a) an outcome name (outcome); and (b) a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a TWO(2)-column data frame containing the hospital in EACH state that has the ranking specified in num. For example the function call rankall("heart attack", "best") would return a data frame containing the names of the hospitals that are the best in their respective states for THIRTY(30)-day heart attack death rates. The function should return a value for EVERY state (some may be NA). The FIRST (1st) column in the data frame is named hospital, which contains the hospital name, and the SECOND (2nd) column is named state, which contains the TWO(2)-character abbreviation for the state name.

rankall <- function(outcome, num = "best") {
  
    ## Read outcome data into a separate data frame
    
    ofcm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ocm <- data.frame(cbind(ofcm[,2],   # hospital name
                            ofcm[,7],   # State
                            ofcm[,11],  # heart attack
                            ofcm[,17],  # heart failure
                            ofcm[,23])) # pneumonia
    
    state_list <<- unique(ofcm[,7])
    state_list <- sort(state_list)
    i <-1
    result <<- data.frame(length(state_list),2)
    
    colnames(ocm) <- c("hospital","State","heart attack","heart failure","pneumonia")
    
    ## Check that outcome is valid
    
    if (!(any(outcome == c("heart attack","heart failure","pneumonia")))){
      stop("invalid outcome")}
    
    else  { 
      
      ## For each state, find the hospital of the given rank 
      
      for (state in state_list)      {
      
      ocm1 <- subset(ocm, State == state)
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
        result[i,1] <- ocm2[1,1]
        result[i,2] <- state
        i = i+1
        }
      else if(num == "worst"){
        result[i,1] <- ocm2[nrow(ocm2),1]
        result[i,2] <- state
        i = i+1
        }
     
      else{
        result[i,1] <- ocm2[num,1]
        result[i,2] <- state
        i<- i+1
      }
}
      colnames(result) <- c("hospital","State")
      return(result)
    }
}





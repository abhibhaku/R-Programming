# Part 2 - Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. 

complete <- function (directory, id = 1:332) {
  
  ## First will start by reading all 332 csv files into a vector  
  
  all_files <- dir() #storing all 332 csv files into a vector
  d <- data.frame()  #empty data frame of 0 rows & 0 columns 
  
  ## run loop based on the id argument value, read files accordingly, omit rows with NAs & return required result
  
  for (i in id){
    spec_df <- read.csv(all_files[i])
    spec_df1 <- na.omit(spec_df)
    nobs <- nrow(spec_df1)
    d <- rbind(d,data.frame(i,nobs))
    } 
    colnames(d) <- c("id","nobs")
    return(d)
}

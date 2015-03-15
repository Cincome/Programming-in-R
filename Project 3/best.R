best<- function(state, outcome){
 ##read data
  data<- read.csv("outcome-of-care-measures.csv", colClasses= "character")
  
 ##Filter col names
 data<- data[c(2,7,11,17,23)]
 names(data)[1]<- "name"
 names(data)[2]<- "state"
 names(data)[3]<- "heart attack"
 names(data)[4]<- "heart failure"
 names(data)[5]<- "pneumonia"
 
 ##validate outcomes
  outcomes =c("heart attack", "heart failure", "pneumonia")
    if (outcome %in% outcomes ==FALSE){
      stop("invalid outcome")
    }
 ## validate state
  states <- data[,2]
  states <- unique(states)
    if (state %in% states ==FALSE){
      stop("invalid state")
    }
 
  ## get rows with state value
  data <- data[data$state==state & data[outcome] != "Not Available", ]
  vals <- data[, outcome]
  rowNum <- which.min(vals)
 
  ##return name with lowest death rate
  data[rowNum, ]$name
 
 }
setwd("/Users/ke.huang/Documents/workspace")

rankhospital <- function(state, outcome, rank) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", header=T, colClasses = "character")
  #test if the state and outcome are valid
  if (! state %in% unique(outcome_data$State)) {
    stop("invalid state")
  }
  if (! outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  #this function returns the hospital with the lowest MR by outcome;
  order_mr<-function(d) {
    colnames(d)<-c("hn", "mr")    
    d<-transform(d, mr=as.numeric(mr))
    d<-d[complete.cases(d), ]
    d<-d[order(d$mr,d$hn), ]
    d$rank=c(1:nrow(d))
    if (rank=="best") {
      b_h<-d[d$rank==1, ][[1]]
    }
    else if (rank=="worst") {
      b_h<-d[d$rank==nrow(d), ][[1]]  
    }
    else if (rank>nrow(d)) {
      b_h<-NA
    }
    else {
      b_h<-d[d$rank==rank, ][[1]]
    }
    b_h
  }
  if (outcome == "heart attack") {
    d1<-outcome_data[outcome_data$State==state, c(2, 11)]
  } else if(outcome == "heart failure") {
    d1<-outcome_data[outcome_data$State==state, c(2, 17)]
  } else if(outcome == "pneumonia"){
    d1<-outcome_data[outcome_data$State==state, c(2, 23)]
  }
  order_mr(d1)
}

# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
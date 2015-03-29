setwd("/Users/ke.huang/Documents/workspace")

best <- function(state, outcome) {
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
    colnames(d)[2]<-"mr"
    d<-transform(d, mr=as.numeric(mr))
    lowest_mr<-min(d$mr, na.rm=T)
    d2<-d[d$mr==lowest_mr, ]
    if (nrow(d2)==1) {
      b_h<-d2[[1]]
    }
    else if (nrow(d2)>1) {
      b_h<-sort(d2[[1]])[1]  
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

# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")

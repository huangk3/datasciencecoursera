setwd("/Users/ke.huang/Documents/workspace")

rankall <- function(outcome, num) {
  ## Read outcome data
#   outcome<-"heart attack"
#   num<-20
  outcome_data <- read.csv("outcome-of-care-measures.csv", header=T, colClasses = "character")
  states<-unique(outcome_data[[7]])
  #test if the state and outcome are valid
  if (! state %in% unique(outcome_data$State)) {
    stop("invalid state")
  }
  if (! outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  #this function returns the hospital with the lowest MR by outcome;
  order_mr<-function(d, num) {
    colnames(d)<-c("hn", "s","mr")    
    d<-transform(d, mr=as.numeric(mr))
    d<-d[complete.cases(d), ]
    s<-c()
    h<-c()
    for (i in states) {
      d2<-d[d$s==i, ]
      d2<-d2[order(d2$mr,d2$hn), ]
      d2$rank=c(1:nrow(d2))
      if (num=="best") {
        h1<-d2[d2$rank==1, ]$hn
        s<-c(s, i)
        h<-c(h, h1)
      }
      else if (num=="worst") {
        h1<-d2[d2$rank==nrow(d2), ]$hn
        s<-c(s, i)
        h<-c(h, h1)
      }
      else if (num>nrow(d2)) {
        h1<-"NA"
        s<-c(s, i)
        h<-c(h, h1)
      }
      else {
        h1<-d2[d2$rank==num, ]$hn
        s<-c(s, i)
        h<-c(h, h1)
      }}
    out_put<-data.frame(hospital=h, state=s)
    out_put<-out_put[order(out_put$state), ]
    rownames(out_put)<-out_put$state
    out_put
  }
  if (outcome == "heart attack") {
    d1<-outcome_data[, c(2,7,11)]
  } else if(outcome == "heart failure") {
    d1<-outcome_data[, c(2,7,17)]
  } else if(outcome == "pneumonia"){
    d1<-outcome_data[, c(2,7,23)]
  }
  order_mr(d1, num)
}
head(rankall("heart attack", 20), 10)
# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
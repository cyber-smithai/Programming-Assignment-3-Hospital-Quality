
library(data.table)
rankall <- function(outcome, num = "best") {
  
  # Read outcome data
  ocm <- data_load('outcome-of-care-measures.csv')
  
  outcome <- tolower(outcome)
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  
  # Renaming Columns to be less verbose and lowercase
  setnames(ocm
           , tolower(sapply(colnames(ocm), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  
  # Columns indices to keep
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(ocm))
  
  # Filtering out unnessecary data 
  ocm <- ocm[, .SD ,.SDcols = col_indices]
  
  # Find out what class each column is 
  # sapply(ocm,class)
  
  # Change outcome column class
  ocm[, outcome] <- ocm[,  as.numeric(get(outcome))]
  
  if (num == "best"){
    return(ocm[order(state, get(outcome), `hospital name`)
                  , .(hospital = head(`hospital name`, 1))
                  , by = state])
  }
  
  if (num == "worst"){
    return(ocm[order(get(outcome), `hospital name`)
                  , .(hospital = tail(`hospital name`, 1))
                  , by = state])
  }
  
  return(ocm[order(state, get(outcome), `hospital name`)
                , head(.SD,num)
                , by = state, .SDcols = c("hospital name") ])
  
}
data_load <- function(file){
  ocm <- data.table::fread('outcome-of-care-measures.csv')
  #print("data is loaded")
  return(ocm)
}
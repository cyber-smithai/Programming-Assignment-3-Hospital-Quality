
library(data.table)
rankhospital <- function(state, outcome, num = "best") {
  
  # Read outcome data
  
  ocm <- data_load('outcome-of-care-measures.csv')
  
  outcome <- tolower(outcome)
  
  # Column name is same as variable so changing it 
  chosen_state <- state 
  
  # Check that state and outcome are valid
  if (!chosen_state %in% unique(ocm[["State"]])) {
    stop('invalid state')
  }
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  
  # Renaming Columns to be less verbose and lowercase
  setnames(ocm
           , tolower(sapply(colnames(ocm), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  
  #Filter by state
  ocm <- ocm[state == chosen_state]
  
  # Columns indices to keep
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(ocm))
  
  # Filtering out unnessecary data 
  ocm <- ocm[, .SD ,.SDcols = col_indices]
  
  # Find out what class each column is 
  # sapply(ocm,class)
  ocm[, outcome] <- ocm[,  as.numeric(get(outcome))]
  
  
  # Removing Missing Values for numerical datatype (outcome column)
  ocm <- ocm[complete.cases(ocm),]
  
  # Order Column to Top 
  ocm <- ocm[order(get(outcome), `hospital name`)]
  
  ocm <- ocm[,  .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
  
  if (num == "best"){
    return(ocm[1,`hospital name`])
  }
  
  if (num == "worst"){
    return(ocm[.N,`hospital name`])
  }
  
  return(ocm[num,`hospital name`])
  
}

data_load <- function(file){
  ocm <- data.table::fread('outcome-of-care-measures.csv')
  #print("data is loaded")
  return(ocm)
}

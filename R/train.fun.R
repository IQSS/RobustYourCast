extract.test.fun <- function(cs,year.max,year.maxf,response) {

  # Save data from test period
  test <- cs[as.character((year.max+1):year.maxf),paste(response)]

  return(test)
}

train.fun <- function(cs,year.min,year.max,year.maxf,response) {
  
  # Trim any extra NA at the end if there
  cs <- cs[as.character(year.min:year.maxf),]

  # Save data from test period
  test <- cs[as.character((year.max+1):year.maxf),paste(response)]

  # Set test data as NA in dataobj
  cs[as.character((year.max+1):year.maxf),paste(response)] <- NA
  
  return(cs)
  
}


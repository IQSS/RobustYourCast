# Function to create a validation object

createvalidobj <- function(block.row,dataobj,response) {
  dataobj$data <- lapply(dataobj$data,function(x){
                  x[as.character(block.row),response] <- NA 
                  return(x)})
  return(dataobj)
}


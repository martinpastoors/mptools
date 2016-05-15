no.emphasis.table <- function(df){
  the.row.names <- rownames(df) 
  
  # For some reason, when 'pandoc' writes the markdown 
  # table to LaTeX, it doesn't make the first column 
  # wide enough unless some padding is added to the row 
  # names
  add.space <- function(x){
    return(paste0(x, ""))
  }
  the.row.names.m <- as.vector(sapply(the.row.names, add.space))
  rownames(df) <- NULL
  df <- cbind(the.row.names.m, df)
  colnames(df)[1] <- "  " 
  
  # Set horizontal justification for columns
  v.justify <- vector()
  v.justify[seq(1, length(df))] <- 'center'
  v.justify[1] <- 'left'
  set.alignment(v.justify) 
  return(df)
}
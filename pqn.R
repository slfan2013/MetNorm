cat("<!--------- pqn --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  reference <- apply(e_raw, 1, median, na.rm = TRUE)
  reference[reference==0] = 1
  quotient <- e_raw/reference
  quotient.median <- apply(quotient, 2, median, na.rm = TRUE)
  e_norm <- t(t(e_raw)/quotient.median)
  
  cat("<!--------- pqn done.--------->\n")
  
}else{
  cat("<!--------- pqn skipped.--------->\n")
}


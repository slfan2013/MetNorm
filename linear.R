cat("<!--------- linear --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  normalize.linear <- get("normalize.quantiles",en=asNamespace("affy"))
  e_norm <- normalize.linear(data.matrix(e_raw))
  rownames(e_norm) <- rownames(e_raw)
  colnames(e_norm) <- colnames(e_raw)
  
  cat("<!--------- linear done.--------->\n")
  
}else{
  cat("<!--------- linear skipped.--------->\n")
}


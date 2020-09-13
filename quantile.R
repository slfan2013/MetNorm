cat("<!--------- quantile --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  normalize.quantile <- get("normalize.quantiles",en=asNamespace("affy"))
  e_norm <- normalize.quantile(data.matrix(e_raw))
  rownames(e_norm) <- rownames(e_raw)
  colnames(e_norm) <- colnames(e_raw)
  
  cat("<!--------- quantile done.--------->\n")
  
}else{
  cat("<!--------- quantile skipped.--------->\n")
}


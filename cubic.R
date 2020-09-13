cat("<!--------- cubic --------->\n")
norm_skip = FALSE
if(!norm_skip){
  pacman::p_load(affy)
  start = Sys.time()
  e_norm <- normalize.qspline(e_raw,samples=0.02,target=apply(e_raw,1,mean,na.rm = TRUE),verbose = FALSE)
  rownames(e_norm) <- rownames(e_raw)
  colnames(e_norm) <- colnames(e_raw)
  cat("<!--------- cubic done.--------->\n")
  
}else{
  cat("<!--------- cubic skipped.--------->\n")
}


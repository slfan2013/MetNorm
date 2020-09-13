cat("<!--------- median --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  index = 1:nrow(e_raw)
  medians = apply(e_raw[index,], 2, median, na.rm=T)
  mean_medians = mean(medians, na.rm = TRUE)
  e_norm = t(t(e_raw)/(medians/mean_medians))
  
  cat("<!--------- median done.--------->\n")
  
}else{
  cat("<!--------- median skipped.--------->\n")
}


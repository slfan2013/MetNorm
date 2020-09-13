cat("<!--------- sum --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  index = 1:nrow(e_raw)
  sums = apply(e_raw[index,], 2, sum, na.rm=T)
  mean_sums = mean(sums, na.rm = TRUE)
  e_norm = t(t(e_raw)/(sums/mean_sums))

  cat("<!--------- sum done.--------->\n")

}else{
  cat("<!--------- sum skipped.--------->\n")
}


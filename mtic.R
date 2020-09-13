cat("<!--------- mTIC --------->\n")
norm_skip = FALSE
if(!'compoundType' %in% colnames(f)){
  cat(paste0("warning: 'compountType' is not in the dataset. mTIC skipped.\n"))
  norm_skip = TRUE
}else{
  mTIC_column = f[['compoundType']]
  if(!'known' %in% unique(f[['compoundType']])){
    cat(paste0("'known' (case-sensitive) is not found in the 'compoundType'. mTIC skipped.\n"))
    norm_skip = TRUE
  }
}
if(!norm_skip){
  start = Sys.time()
  index = mTIC_column %in% "known"
  sums = apply(e_raw[index,], 2, sum, na.rm=T)
  mean_sums = mean(sums, na.rm = TRUE)
  e_norm = t(t(e_raw)/(sums/mean_sums))

  cat("<!--------- mTIC done.--------->\n")

}else{
  cat("<!--------- mTIC skipped.--------->\n")
}


cat("<!--------- batchratio --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  e_norm = matrix(,nrow=nrow(e_raw),ncol=ncol(e_raw))
  QC.index = p[["sampleType"]]
  batch = p[["batch"]]
  for(i in 1:nrow(f)){
    means = by(as.numeric(e_raw[i,QC.index=='qc']),batch[QC.index=='qc'], mean, na.rm=T)
    mean_means = mean(means, na.rm = TRUE)
    for(b in 1:length(unique(batch))){
      e_norm[i,batch%in%unique(batch)[b]] = as.numeric(e_raw[i,batch%in%unique(batch)[b]])/(means[[unique(batch)[b]]]/mean_means)
    }
  }
  rownames(e_norm) = rownames(e_raw)
  colnames(e_norm) = colnames(e_raw)
  
  cat("<!--------- batchratio done.--------->\n")
  
}else{
  cat("<!--------- batchratio skipped.--------->\n")
}


cat("<!--------- nomis --------->\n")
norm_skip = FALSE
pacman::p_load(metabolomics)
if(!'compoundType' %in% colnames(f)){
  cat(paste0("warning: 'compoundType' is not found. NOMIS is skipped.\n"))
  norm_skip = TRUE
}else{
  IS_column = f[['compoundType']]
  if(!'istd' %in% unique(f[['compoundType']])){
    cat(paste0("'istd' (case-sensitive) is not found in the 'compoundType'. NOMIS is skipped.\n"))
    norm_skip = TRUE
  }
  
}
if(!norm_skip){

    IS_column = f[['compoundType']]
    istd_index =  which(IS_column%in%'istd')
    
    inputdata = data.frame(Group = "A", t(log((e_raw + sqrt(e_raw^2 + 4)) * 0.5, base  = exp(1))))
    colnames(inputdata) = c("Group",f$label)
    rownames(inputdata) = paste0("S",1:nrow(inputdata))
    normed = exp(t(Normalise(inputdata,method = 'nomis',nc = istd_index)$output[,-1]))
    e_norm = rbind(e_raw[istd_index,],normed)
  
  cat("<!--------- nomis done.--------->\n")
  
}else{
  cat("<!--------- nomis skipped.--------->\n")
}



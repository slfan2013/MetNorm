cat("<!--------- contrast --------->\n")
norm_skip = FALSE
if(!norm_skip){
  pacman::p_load(affy)
  start = Sys.time()
  threshold=1e-11
  e_raw[e_raw <= 0] <- threshold
  maffy.data <- maffy.normalize(data.matrix(e_raw),
                                subset=1:nrow(e_raw),
                                span=0.75,
                                verbose=FALSE,
                                family="gaussian",
                                log.it=FALSE)
  subtract <- function(x){
    t(t(x)-apply(x,2,quantile,0.1,na.rm = TRUE))
  }
  e_norm <- subtract(maffy.data)
  rownames(e_norm) = rownames(e_raw)
  colnames(e_norm) = colnames(e_raw)
  
  cat("<!--------- contrast done.--------->\n")
  
}else{
  cat("<!--------- contrast skipped.--------->\n")
}


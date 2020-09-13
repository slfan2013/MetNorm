cat("<!--------- liwong --------->\n")
norm_skip = FALSE
if(!norm_skip){
  start = Sys.time()
  average.intensity <- apply(e_raw,2,mean,na.rm = TRUE)
  median.number <- round(ncol(e_raw)/2 + 0.1)
  ordering <- order(average.intensity)
  median.sample.number <- ordering[median.number]
  median.sample <- e_raw[,median.sample.number]
  e_norm <- vector()
  for(i in 1:ncol(e_raw)){
    tryCatch({
      liwong.model <- normalize.invariantset(data=e_raw[,i],
                                             ref=median.sample,
                                             prd.td=c(0.003,0.007))
      liwong.sample <- predict(liwong.model$n.curve$fit, e_raw[,i])
    },error = function(er){
      liwong.sample = list();liwong.sample$y = e_raw[,i]
    })
    
    e_norm <- cbind(e_norm,liwong.sample$y)
  }
  colnames(e_norm) = colnames(e_raw)
  cat("<!--------- liwong done.--------->\n")
  
}else{
  cat("<!--------- liwong skipped.--------->\n")
}


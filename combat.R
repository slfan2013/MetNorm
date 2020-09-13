cat("<!--------- combat --------->\n")
norm_skip = FALSE
pacman::p_load(sva)

if(!norm_skip){
  
  e_norm = ComBat(e_raw, batch = p$batch)
  
  cat("<!--------- combat done.--------->\n")
  
}else{
  cat("<!--------- combat skipped.--------->\n")
}



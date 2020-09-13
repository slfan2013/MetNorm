robust_sd = function(x){
  x = remove_outlier(x)[[1]]
  sd(x,na.rm = TRUE)
}

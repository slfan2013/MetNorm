
RSD = function(data){
  remove_outlier = function(v){
    out = boxplot.stats(v)$out
    return(list(value = v[!v%in%out],index = which(v%in%out)))
  }
  return(apply(data,1,function(x){
    x = remove_outlier(x)[[1]]
    return(sd(x,na.rm=T)/mean(x,na.rm=T))
  }))
}

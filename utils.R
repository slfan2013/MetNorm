remove_outlier = function(v){
  out = boxplot.stats(v)$out
  return(list(value = v[!v%in%out],index = which(v%in%out)))
}
RSD = function(data){
  return(apply(data,1,function(x){
    x = remove_outlier(x)[[1]]
    return(sd(x,na.rm=T)/mean(x,na.rm=T))
  }))
}
robust_sd = function(x){
  x = remove_outlier(x)[[1]]
  sd(x,na.rm = TRUE)
}
rm_batch = function(d, batch){ # d=e
  return(t(apply(t(d),2,function(x){
    x/(rep(by(x,batch,mean), table(batch))/mean(x))
  })))
}
transform = function(e, forward = TRUE, y0 = NULL, lambda = NULL, regular_log = FALSE){


  if(is.null(y0)){y0=0}

  if(class(e) == 'numeric'){

    if(forward){


    }else{
      if(is.null(lambda)){
        stop("You forgot to input lambda.")
      }
      e_after = 0.5 * (2*y0 - lambda * exp(-e) + exp(e))

    }

  }else{

    if(forward){
      if(regular_log){
        e_after = log(e)
      }else{
        e_after = e
        lambda = rowMeans(e)^2
        for(i in 1:nrow(e)){
          e_after[i,] = log(e[i,] - y0 + sqrt((e[i,]-y0)^2 + lambda[i]))
        }
      }
    }else{
      if(regular_log){
        e_after = exp(e)
      }else{
        if(is.null(lambda)){
          stop("You forgot to input lambda.")
        }
        e_after = e
        for(i in 1:nrow(e)){
          e_after[i,] = 0.5 * (2*y0 - lambda[i] * exp(-e[i,]) + exp(e[i,]))
        }
      }
    }

  }


  return(list(e_after, lambda = lambda))
}
scale_data = function(d){# d = e.
  sds = apply(d, 1, sd, na.rm = TRUE)

  # !!! how to deal with zero sds.
  sds[sds == 0] = 1


  means = apply(d, 1, mean, na.rm = TRUE)
  data_scale = d
  for(i in 1:nrow(d)){
    data_scale[i,] = (d[i,] - means[i])/sds[i]
  }
  return(list(
    data_scale = data_scale,
    means = means,
    sds = sds
  ))
}

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

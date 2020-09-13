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

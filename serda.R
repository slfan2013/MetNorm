cat("<!--------- SERDA --------->\n")
norm_skip = FALSE
# stop("test error")


remove_outlier = function(v){
  out = boxplot.stats(v)$out
  return(list(value = v[!v%in%out],index = which(v%in%out)))
}
rm_batch = function(d, batch){ # d=e

  return(t(apply(t(d),2,function(x){
    x/(rep(by(x,batch,mean), table(batch))/mean(x))
  })))

}
library(keras)
seed = 42

pacman::p_load(GGally, freeknotsplines, lattice, plyr, locpol, splines, ranger, spm, plotly, pdist,pheatmap, DiscriMiner, pROC, caret, wesanderson)


if(exists("e")){
  need_to_remove_e_after_normalization = F
}else{
  need_to_remove_e_after_normalization = T
  e = e_raw

  if(class(data) == 'function'){

  }else{
    if(exists("f")){
      if(nrow(e) == nrow(f)){

      }else{
        f = data$f
        p = data$p
      }
    }else{
      f = data$f
      p = data$p
    }


  }


}






for(i in 1:nrow(e)){

  e[i,is.na(e[i,])] = rnorm(sum(is.na(e[i,])), mean = 0.5 * min(e[i,!is.na(e[i,])]), sd = 0.05 * min(e[i,!is.na(e[i,])]))

}


original_sampleType = p$sampleType

sampleType_NA_index = is.na(p$sampleType)


e_na = e[,is.na(p$sampleType)]
p_na = p[is.na(p$sampleType),]

e = e[,!is.na(p$sampleType)]
p = p[!is.na(p$sampleType),]

original_time = as.numeric(p$time)

# LOG TRANSFORMATION
e = transform(e)
lambda = e$lambda
e = e[[1]]

number_of_cross_validation = 5

qc_index = which(p$sampleType == 'qc')
sample_index = which(p$sampleType == 'sample')
batch_index = p$batch


with_validates = any(!p$sampleType %in% c("qc","sample"))
if(with_validates){
  validates = unique(p$sampleType)
  validates = validates[!validates %in% c("qc",'sample')]
  validates_indexes = list()
  for(i in 1:length(validates)){
    validates_indexes[[i]] = which(p$sampleType %in% validates[i])
  }
  names(validates_indexes) = validates
}else{
  validates = NULL
}

e_qc = e[,qc_index]
p_qc = p[qc_index,]
if(with_validates){
  e_validates = list()
  for(i in 1:length(validates)){
    e_validates[[i]] = e[,which(p$sampleType %in% validates[i])]
  }
}


# Even without validate, having this for cross-validation for selecting best hyperparameters.
validates_RSDs = list()
if(with_validates){
  e_validates_scale = list()
  for(i in 1:length(validates)){

    e_validates_scale[[i]] = scale_data(e_validates[[i]])

  }
  validates_RSDs[[i]] = 1
}

# AUTO-SCALING TRANSFORMATION
x_sample_scale = scale_data(e[,sample_index])
x_sample_scale_d = t(x_sample_scale$data_scale)
x_sample_scale_sd = x_sample_scale$sds
x_sample_scale_mean = x_sample_scale$means
x_qc_scale = scale_data(e[,qc_index])
x_qc_scale_d = t(x_qc_scale$data_scale)
x_qc_scale_sd = x_qc_scale$sds
x_qc_scale_mean = x_qc_scale$means


e_none = t(transform(e, forward = FALSE, lambda = lambda)[[1]])
# calculation time
calculation_time = c()

ae_model = function(x_train_input, x_test_input,x_train_output, x_test_output, patience = 50, layer_units = c(1400), batch_size = 128, epochs = 2000, verbose = 0,activations = c("elu"), drop_out_rate = 0.05, optimizer = "adam",s_index = NULL, t_index = NULL # , customize_performance_evaluation_set
){
  # seed = 42
  # reticulate::py_config()
  # reticulate::py_set_seed(seed)
  # set.seed(seed)

  # Another way of scripting denoising autoencoder.
  # middles = list()
  # tensorflow::tf$random$set_seed(42)
  # middles[[length(layer_units)+1]] = keras_model_sequential() # here is the final layer.
  # middles[[length(layer_units)+1]] %>% layer_dense(input_shape = layer_units[length(layer_units)], units = nrow(f),use_bias = TRUE)
  #
  # for(layer_index in length(layer_units):1){
  #   tensorflow::tf$random$set_seed(42)
  #   middles[[layer_index]] = keras_model_sequential(name = paste0('middle',layer_index))
  #   if(layer_index==1){
  #     middles[[layer_index]] %>% layer_dense(input_shape = nrow(f), units = layer_units[layer_index], activation = activations[layer_index],use_bias = TRUE) # this is the first mid-layer
  #   }else{
  #     middles[[layer_index]] %>% layer_dense(input_shape = layer_units[layer_index-1], units = layer_units[layer_index], activation = activations[layer_index],use_bias = TRUE) # this is other mid-layer
  #   }
  # }
  # model = middles[[1]]%>%layer_dropout(drop_out_rate)
  # for(i in 2:length(middles)){
  #   model_temp = middles[[i]]
  #   model %>% model_temp
  # }

  early_stopping <- callback_early_stopping(patience = patience)
  tensorflow::tf$random$set_seed(seed)
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = layer_units, activation =  activations, input_shape = nrow(f)) %>%
    layer_dropout(rate = drop_out_rate) %>%
    layer_dense(units = nrow(f))

  model %>% compile(
    loss = "mean_absolute_error",
    optimizer = optimizer
  )
  model %>% save_model_hdf5("model.h5")


  model %>% fit(
    x = x_train_input,
    y = x_train_output,
    epochs = epochs,
    verbose = verbose,
    batch_size = batch_size,
    validation_data = list(x_test_input, x_test_output),
    callbacks = list(early_stopping),
    view_metrics = FALSE
  )

  epoch_n = length(model$history$epoch)#-patience
  # epoch_n



  x_all_input = rbind(x_train_input,x_test_input)
  x_all_output = rbind(x_train_output,x_test_output)

  final_model <- load_model_hdf5("model.h5")
  final_model %>% fit(
    x = x_all_input,
    y = x_all_output,
    epochs = epoch_n,
    verbose = verbose,
    batch_size = batch_size,
    view_metrics = FALSE
  )


  if(is.null(s_index)){
    return(list(
      final_model = final_model,
      model = model
    ))
  }else{
    x_all = x_all_input
    # pred_train = x_all_output
    #
    # model2 <- load_model_hdf5("model.h5")
    # model2 %>% fit(
    #   x = x_all_input,
    #   y = pred_train,
    #   epochs = epoch_n,
    #   verbose = verbose,
    #   batch_size = batch_size
    # )

    pred = predict(final_model,  x_all)

    x_all[s_index,] = pred[1:nrow(x_current_train_output),]
    x_all[t_index,] = pred[(nrow(x_current_train_output)+1):nrow(pred),]

    return(list(
      final_model = final_model,
      pred_train = x_all,
      model = model
    ))

  }


}

# HYPERPARAMETERS
layer_units_options = expand.grid(
  # x = ceiling(seq(min(nrow(e),400), min(nrow(e)*3,1600), length.out = 10)),
  x  = round(nrow(f)*c(seq(1.2, 2, by = 0.2))),
  activation_function = c("elu"),
  # drop_rates = c(3:6)/100,
  drop_rates = 5/100,
  patience = c(25), many_training = c(TRUE
                                                                                                                                                                                                         # ,FALSE
),many_training_n = c(500,1500), noise_weight = 1, use_qc_sd_when_final_correct = c(T
                                                                            # ,F
)
)
layer_units_options = layer_units_options[1,]
# layer_units_options$x = round(nrow(f)*c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2))
# layer_units_options$drop_rates = 0.05
e_norms = list()

l = 1

warning_index = list()


QC_cv_RSDs = list()
rsd_each_comp = list()
# START
start = Sys.time()
warning_index[[l]] = c("")
qc_left_pred = list()
for(l in 1:nrow(layer_units_options)){
  rsd_each_comp[[l]] = list()
  # rsd for each of the fold.
  x_left_normalize = x_left_predict = x_qc_scale_d
  sample_index_temp = 1:nrow(x_left_normalize)
  x_lefts = split(sample_index_temp, sample_index_temp%%number_of_cross_validation)

  rsds = c()
  qc_left_pred[[l]] = list()
  for(i in 1:length(x_lefts)){
    print(paste0(i,'th cv'))

    x_train = t(e_qc[,-x_lefts[[i]]])
    x_left = t(e_qc[,x_lefts[[i]]])
    # add noise to input data according to train and target.
    if(layer_units_options$many_training[l]){
      index = (1:layer_units_options$many_training_n[l])%%nrow(x_train)
      index[index==0] = nrow(x_train)
      x_train_output = x_train_input = x_train[index,] #sample(size = many_training_n, 1:nrow(t(e_qc)), replace = TRUE)
    }else{
      x_train_output = x_train
      x_train_input = x_train
    }

    x_train_var = apply(x_train_input,2,robust_sd)^2
    target_var = apply(x_left,2,robust_sd)^2
    for(j in 1:ncol(x_train_input)){
      if(target_var[j] > x_train_var[j]){
        set.seed(j)
        x_train_input[,j] = x_train_input[,j]+rnorm(length(x_train_input[,j]), sd = sqrt(target_var[j] - x_train_var[j])*layer_units_options$noise_weight[l])
      }
    }

    x_train_input = t(scale_data(t(x_train_input))[[1]])
    x_train_output = t(scale_data(t(x_train_output))[[1]])

    # 8/2 split
    set.seed(42)
    s_index = sample(1:nrow(x_train_input), size = nrow(x_train_input) * 0.8)
    t_index = (1:nrow(x_train_input))[!(1:nrow(x_train_input)) %in% s_index]
    x_test_input = x_train_input[t_index,]
    x_train_input = x_train_input[s_index,]
    x_test_output = x_train_output[t_index,]
    x_train_output = x_train_output[s_index,]

    ae = ae_model(x_train_input, x_test_input,x_train_output, x_test_output, layer_units = as.numeric(layer_units_options$x[l]), verbose = 0, patience = layer_units_options$patience[l], activations = as.character(layer_units_options$activation_function[l]), drop_out_rate = layer_units_options$drop_rates[l], optimizer = "adam")

    final_model = ae$final_model
    x_left_scale = t(scale_data(t(x_left))[[1]])

    x_left_predict = predict(final_model, x_left_scale)

    # normalize;
    x_left_normalize = x_left_scale
    x_left_predict_o = x_left_predict
    for(j in 1:ncol(x_left_normalize)){
      x_left_normalize[,j] = x_left_scale[,j] - (x_left_predict[,j] - mean(x_left_predict[,j]))
      x_left_normalize[,j] = x_left_normalize[,j] * x_qc_scale_sd[j] + x_qc_scale_mean[j]
    }
    # normalize batch effect
    x_left_normalize = t(rm_batch(t(x_left_normalize),batch = p_qc$batch[x_lefts[[i]]]))

    # EXP
    # x_left_normalize_exp_current = exp(x_left_normalize)
    x_left_normalize_exp_current = t(transform(t(x_left_normalize), forward = FALSE, lambda = lambda)[[1]])

    # put normalized dataset to the original scale.
    raw_means = apply(transform(e_qc, forward = FALSE, lambda = lambda)[[1]],1, mean)
    norm_means = apply(x_left_normalize_exp_current,2, mean)
    tran_temp = transform(e_qc, forward = FALSE, lambda = lambda)[[1]][,x_lefts[[i]]]
    for(k in 1:ncol(x_left_normalize_exp_current)){
      mean_adj = x_left_normalize_exp_current[,k] - (norm_means[k] - raw_means[k])
      if(any(mean_adj<0) | is.na(any(mean_adj<0))){
        # cat(k,": ",sum(mean_adj<0),"\n")
        if(sum(mean_adj<0)>length(mean_adj)/2 | is.na(sum(mean_adj<0)>length(mean_adj)/2)){
          # warning_index[[l]] = c(warning_index[[l]], paste0("qc",k))
          # mean_adj = exp(e_qc)[k,]
          mean_adj = tran_temp[k,]
        }else{
          mean_adj[mean_adj<0] = rnorm(sum(mean_adj<0), mean = min(mean_adj[mean_adj>0], na.rm = TRUE)/2, sd = min(mean_adj[mean_adj>0], na.rm = TRUE)/20)
        }
      }
      x_left_normalize_exp_current[,k] = mean_adj
    }
    qc_left_pred[[l]][[i]] = t(x_left_normalize_exp_current)
    rsd_each_comp[[l]][[i]] = RSD(t(x_left_normalize_exp_current))
    rsds[i] = median(rsd_each_comp[[l]][[i]], na.rm = TRUE)
  }

  QC_cv_RSDs[[l]] = mean(rsds)
  rsd_each_comp[[l]] = apply(do.call('cbind',rsd_each_comp[[l]]),1,mean)
  # print(median(rsd_each_comp[[l]]))


  # How is the algorithm perform on validates?
  if(with_validates){
    print("Working on validates.")
    current_method_validate_RSDs = list()

    x_target_normalize_exps = list()

    for(i in 1:length(validates)){

      # add noise to input data according to train and target.
      if(layer_units_options$many_training[l]){
        index = (1:layer_units_options$many_training_n[l])%%nrow(t(e_qc))
        index[index==0] = nrow(t(e_qc))
        x_train_output = x_train_input = t(e_qc)[index,] #sample(size = many_training_n, 1:nrow(t(e_qc)), replace = TRUE)
      }else{
        x_train_input = t(e_qc)
        x_train_output = t(e_qc)
      }

      # add noise to input data according to train and target.
      x_train_var = apply(x_train_input,2,robust_sd)^2
      target_var = apply(t(e_validates[[i]]),2,robust_sd)^2

      for(j in 1:ncol(x_train_input)){
        if(target_var[j] > x_train_var[j]){
          set.seed(j)
          x_train_input[,j] = x_train_input[,j]+rnorm(length(x_train_input[,j]), sd = sqrt(target_var[j] - x_train_var[j])*layer_units_options$noise_weight[l])

        }
      }

      set.seed(seed)
      s_index = sample(1:nrow(x_train_input), size = nrow(x_train_input) * 0.8)
      t_index = (1:nrow(x_train_input))[!(1:nrow(x_train_input)) %in% s_index]

      x_train_input = t(scale_data(t(x_train_input))[[1]])
      x_train_output = t(scale_data(t(x_train_output))[[1]])

      # x_current_test = x_train[t_index,]
      # x_current_train = x_train[s_index,]
      x_current_train_input = x_train_input[s_index,]
      x_current_test_input = x_train_input[t_index,]
      x_current_train_output = x_train_output[s_index,]
      x_current_test_output = x_train_output[t_index,]


      ae = ae_model(x_current_train_input,x_current_test_input,x_current_train_output,x_current_test_output, layer_units = as.numeric(layer_units_options$x[l]), verbose = 0, patience = layer_units_options$patience[l],
                    activations = as.character(layer_units_options$activation_function[l]),
                    drop_out_rate = layer_units_options$drop_rates[l],
                    optimizer = tolower("Adam"),
                    t_index = NULL,
                    s_index = NULL
      )


      x_target_d = t(e_validates_scale[[i]]$data_scale)
      x_target_sd = e_validates_scale[[i]]$sds
      x_target_mean = e_validates_scale[[i]]$means


      x_target_predict = predict(ae$final_model, x_target_d)

      # visualize with plot_ly
      # plot_ly(x = c(1:nrow(x_target_normalize_exp),1:nrow(x_target_normalize_exp)),y = c(x_target_predict[,101],x_target_d[,101]),text = c(1:nrow(x_target_normalize_exp),1:nrow(x_target_normalize_exp)), color = rep(c("pred","true"),each = nrow(x_target_normalize_exp)))

      x_target_normalize = x_target_d


      # plot(x_train_input[qc_index,2])
      # plot(x = 1:length(x_target_d[,2]), (x_target_d[,2]* x_target_sd[2] + x_target_mean[2]), ylim = c(8,10))
      # points(x = 1:length(x_target_d[,2]),x_target_predict[,2] * min(x_target_sd[2], x_qc_scale_sd[2]) + x_qc_scale_mean[2], col = 'red')
      # points(x = 1:length(x_target_d[,2]),x_target_predict[,2] * x_target_sd[2] + x_target_mean[2], col = 'blue')
      # abline(h = x_target_mean[2], col = 'blue')


      # normalize
      for(j in 1:ncol(x_target_normalize)){
        if(layer_units_options$use_qc_sd_when_final_correct[l]){
          x_target_predict[,j] = x_target_predict[,j] * min(x_target_sd[j], x_qc_scale_sd[j]) + x_qc_scale_mean[j]
          # x_target_predict[,j] = x_target_predict[,j] * x_qc_scale_sd[j] + x_qc_scale_mean[j]
        }else{
          x_target_predict[,j] = x_target_predict[,j] * x_target_sd[j] + x_target_mean[j]
        }
        x_target_normalize[,j] = (x_target_normalize[,j]* x_target_sd[j] + x_target_mean[j]) - (x_target_predict[,j] - mean(x_target_predict[,j]))
      }

      # deal with leftover batch effect
      x_target_normalize = t(rm_batch(t(x_target_normalize), p[p$sampleType %in% validates[i],]$batch))
      # points(x = 1:length(x_target_d[,2]),x_target_normalize[,2], col = 'green')

      # EXP
      # x_target_normalize_exp = exp(x_target_normalize)
      x_target_normalize_exp = t(transform(t(x_target_normalize), forward = FALSE, lambda = lambda)[[1]])

      # raw_means = apply(exp(e_validates[[i]]),1, mean)
      raw_means = apply(transform(e_validates[[i]], forward = FALSE, lambda = lambda)[[1]],1, mean)
      norm_means = apply(x_target_normalize_exp,2, mean)

      for(k in 1:ncol(x_target_normalize_exp)){
        mean_adj = x_target_normalize_exp[,k] - (norm_means[k] - raw_means[k])
        if(any(mean_adj<0) | is.na(any(mean_adj<0))){
          #cat(k,": ",sum(mean_adj<0),"\n")
          if(sum(mean_adj<0)>length(mean_adj)/2 | is.na(sum(mean_adj<0)>length(mean_adj)/2)){
            # warning_index[[l]] = c(warning_index[[l]], paste0(validates[i],k))
            # mean_adj = exp(e_validates[[i]])[k,]
            mean_adj = transform(e_validates[[i]], forward = FALSE, lambda = lambda)[[1]][k,]
          }else{
            mean_adj[mean_adj<0] = rnorm(sum(mean_adj<0), mean = min(mean_adj[mean_adj>0], na.rm = TRUE)/2, sd = min(mean_adj[mean_adj>0], na.rm = TRUE)/20)
          }
        }
        x_target_normalize_exp[,k] = mean_adj
      }

      # plot(x = 1:length(p$time[validates_indexes[[1]]]),e_none[validates_indexes[[1]],2])
      # points(x_target_normalize_exp[,2], col = 'red')

      current_validate_RSD = RSD(t(x_target_normalize_exp))
      current_method_validate_RSDs[[validates[i]]] = current_validate_RSD
      validates_RSDs[[i]][l] = median(current_validate_RSD, na.rm = TRUE)
      if(l == 1){
        validates_RSDs[[i]][l] = paste0(validates_RSDs[[i]][l], "; (raw: ",median(RSD(transform(e[,validates_indexes[[i]]], forward = FALSE, lambda = lambda)[[1]]), na.rm = TRUE),")")
      }
      x_target_normalize_exps[[i]] = x_target_normalize_exp
    }
    names(validates_RSDs) = validates
    names(x_target_normalize_exps) = validates


  }




  # add noise to input data according to train and target.
  if(layer_units_options$many_training[l]){
    index = (1:layer_units_options$many_training_n[l])%%nrow(t(e_qc))
    index[index==0] = nrow(t(e_qc))
    x_train_output = x_train_input = t(e_qc)[index,]
  }else{
    x_train_input = t(e_qc)
    x_train_output = t(e_qc)
  }



  x_train_var = apply(x_train_input,2,robust_sd)^2
  target_var = apply(t(e[,sample_index]),2,robust_sd)^2

  # cat("here: ",sum(target_var>x_train_var)/length(target_var))

  for(j in 1:ncol(x_train_input)){
    if(target_var[j] > x_train_var[j]){
      set.seed(j)
      x_train_input[,j] = x_train_input[,j]+rnorm(length(x_train_input[,j]), sd = sqrt(target_var[j] - x_train_var[j])*layer_units_options$noise_weight[l])
    }
  }

  # WORKING ON SAMPLES
  print("Working on samples.")
  set.seed(seed)
  s_index = sample(1:nrow(x_train_input), size = nrow(x_train_input) * 0.8)
  t_index = (1:nrow(x_train_input))[!(1:nrow(x_train_input)) %in% s_index]

  x_train_input = t(scale_data(t(x_train_input))[[1]])
  x_train_output = t(scale_data(t(x_train_output))[[1]])

  x_current_train_input = x_train_input[s_index,]
  x_current_test_input = x_train_input[t_index,]
  x_current_train_output = x_train_output[s_index,]
  x_current_test_output = x_train_output[t_index,]


  ae = ae_model(x_current_train_input,x_current_test_input,x_current_train_output,x_current_test_output, layer_units = as.numeric(layer_units_options$x[l]), verbose = 0, patience = layer_units_options$patience[l],
                activations = as.character(layer_units_options$activation_function[l]), #0.1873987
                drop_out_rate = layer_units_options$drop_rates[l],
                optimizer = tolower("Adam"),
                t_index = t_index,
                s_index = s_index
  )

  x_sample_predict = predict(ae$final_model, x_sample_scale_d)
  x_sample_normalize = x_sample_scale_d

  for(j in 1:ncol(x_sample_normalize)){
    if(layer_units_options$use_qc_sd_when_final_correct[l]){
      x_sample_predict[,j] = x_sample_predict[,j] * min(x_sample_scale_sd[j], x_qc_scale_sd[j]) +
        x_sample_scale_mean[j]
    }else{
      x_sample_predict[,j] = x_sample_predict[,j] * x_sample_scale_sd[j] + x_sample_scale_mean[j]
    }
  }
  for(j in 1:ncol(x_sample_normalize)){
    x_sample_normalize[,j] = (x_sample_normalize[,j]* x_sample_scale_sd[j] + x_sample_scale_mean[j]) - (x_sample_predict[,j] - mean(x_sample_predict[,j]))
  }
  # deal with leftover batch effect
  x_sample_normalize = t(rm_batch(t(x_sample_normalize), p$batch[sample_index]))

  # EXP
  # x_sample_normalize_exp = exp(x_sample_normalize)
  x_sample_normalize_exp = t(transform(t(x_sample_normalize), forward = FALSE, lambda = lambda)[[1]])

  # put data to original scale
  raw_means = apply(transform(e[,sample_index], forward = FALSE, lambda = lambda)[[1]],1, mean)
  norm_means = apply(x_sample_normalize_exp,2, mean)
  transform_temp = transform(e[,sample_index], forward = FALSE, lambda = lambda)[[1]]
  for(k in 1:ncol(x_sample_normalize_exp)){
    mean_adj = x_sample_normalize_exp[,k] - (norm_means[k] - raw_means[k])
    if(any(mean_adj<0) | is.na(any(mean_adj<0))){
      # cat(k,": ",sum(mean_adj<0),"\n")
      if(sum(mean_adj<0)>length(mean_adj)/2 | is.na(sum(mean_adj<0)>length(mean_adj)/2)){
        # warning_index[[l]] = c(warning_index[[l]], paste0("sample",k))
        # mean_adj = exp(e[,sample_index])[k,]
        mean_adj = transform_temp[k,]
      }else{
        mean_adj[mean_adj<0] = rnorm(sum(mean_adj<0), mean = min(mean_adj[mean_adj>0], na.rm = TRUE)/2, sd = min(mean_adj[mean_adj>0], na.rm = TRUE)/20)
      }
    }
    x_sample_normalize_exp[,k] = mean_adj
  }

  # put qc, validates and samples together.
  e_norm = transform(e, forward = FALSE, lambda = lambda)[[1]]
  e_norm[,sample_index] = t(x_sample_normalize_exp)
  if(with_validates){
    for(i in 1:length(validates)){
      e_norm[,p$sampleType %in% validates[i]] = t(x_target_normalize_exps[[validates[i]]])
    }
  }

  pred_train = ae$pred_train
  if(layer_units_options$many_training[l]){
    pred_train = pred_train[1:length(qc_index),]
  }

  for(j in 1:ncol(x_qc_scale_d)){
    old = x_qc_scale_d[,j] * x_qc_scale_sd[j] + x_qc_scale_mean[j]
    pred_temp = pred_train[,j] * x_qc_scale_sd[j] + x_qc_scale_mean[j]
    new  = old - (pred_temp - mean(pred_temp))
    e_norm[,qc_index][j,] = transform(new, forward = FALSE, lambda = lambda[j])[[1]]
  }
  raw_means = apply(transform(e_qc, forward = FALSE, lambda = lambda)[[1]],1, mean)
  norm_means = apply(e_norm[,qc_index],1, mean)
  for(k in 1:nrow(e_norm[,qc_index])){
    mean_adj = e_norm[,qc_index][k,] - (norm_means[k] - raw_means[k])
    if(any(mean_adj<0) | is.na(any(mean_adj<0))){
      # cat(k,": ",sum(mean_adj<0),"\n")
      if(sum(mean_adj<0)>length(mean_adj)/2 | is.na(sum(mean_adj<0)>length(mean_adj)/2)){
        warning_index[[l]] = c(warning_index[[l]], paste0("aggregating",k))
        # mean_adj = exp(e_qc)[k,]
        mean_adj = transform(e_qc, forward = FALSE, lambda = lambda)[[1]][k,]
      }else{
        mean_adj[mean_adj<0] = rnorm(sum(mean_adj<0), mean = min(mean_adj[mean_adj>0], na.rm = TRUE)/2, sd = min(mean_adj[mean_adj>0], na.rm = TRUE)/20)
      }
      e_norm[,qc_index][k,] = mean_adj
    }
    e_norm[,qc_index][k,] = mean_adj
  }

  e_exp = t(e_none)
  sds = apply(e_exp,1,function(x){
    # x = remove_outlier(x)$value
    sd(x)
  })

  if(with_validates){
    for(i in 1:length(validates)){
      validate_index = p$sampleType %in% validates[i]
      for(j in 1:nrow(e_norm)){
        e_norm[j,validate_index] = ((mean(e_exp[j,validate_index]) - mean(e_exp[j,sample_index]))/sds[j] * sd(e_norm[j,]) + mean(e_norm[j,sample_index]))/mean(e_norm[j,validate_index]) * e_norm[j,validate_index]
      }
    }
  }

  for(i in 1:nrow(e_norm)){
    e_norm[i,qc_index] = ((mean(e_exp[i,qc_index]) - mean(e_exp[i,sample_index]))/sds[i] * sd(e_norm[i,]) + mean(e_norm[i,sample_index]))/mean(e_norm[i,qc_index]) * e_norm[i,qc_index]
  }
  qc_left_pred_dfs = list()
  for(l in 1:length(qc_left_pred)){
    qc_left_pred_dfs[[l]] = do.call('cbind',qc_left_pred[[l]])
  }
  # qc_left_pred_df = do.call('cbind',qc_left_pred[[1]])
  #
  # biorec_serda = qc_left_pred_df
  # biorec_serda = biorec_serda[,c(1:ncol(biorec_serda))[c(1:ncol(biorec_serda)) %% 20]==1]
  # biorec_serda_cor = cor(biorec_serda, method = 'pearson', use = "complete.obs")
  # breaksList = seq(0.5,1,by=0.001)
  # pheatmap(biorec_serda_cor, color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)),breaks = breaksList,show_rownames = F, show_colnames = F)


  e_norm2 = cbind(e_norm, e_na)
  p2 = rbind(p, p_na)

  e_norm2[,!is.na(original_sampleType)] = e_norm
  e_norm2[,is.na(original_sampleType)] = e_na

  e_norm2[e_raw==0] = 0
  e_norm2[is.na(e_raw)] = NA

  e_norm = e_norm2
  p2[!is.na(original_sampleType),] = p
  p2[is.na(original_sampleType),] = p_na
  p = p2

  e_norms[[l]] = e_norm
  if(need_to_remove_e_after_normalization){
    rm(e)
  }

  gc()
  # write(l,'test.csv')
}


cat("<!--------- SERDA done.--------->\n")

# cat("<!--------- SERDA skipped.--------->\n")

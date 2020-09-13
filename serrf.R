cat("<!--------- SERRF --------->\n")
norm_skip = FALSE

for(i in 1:nrow(e)){

  e[i,is.na(e[i,])] = rnorm(sum(is.na(e[i,])), mean = 0.5 * min(e[i,!is.na(e[i,])]), sd = 0.05 * min(e[i,!is.na(e[i,])]))

}

pacman::p_load(ranger, parallel)
detectcores_ratio = 1

p$sample_index = paste0('p',1:nrow(p))

original_colnames_e = colnames(e)
colnames(e) = p$sample_index


p_empty_sampleType = p[is.na(p$sampleType), ]
e_empty_sampleType = e[,is.na(p$sampleType)]

if(class(e_empty_sampleType) == 'numeric'){
  e_empty_sampleType = matrix(e_empty_sampleType, ncol = 1)
}
colnames(e_empty_sampleType) = p_empty_sampleType$sample_index
e = e[, !is.na(p$sampleType)]
p = p[!is.na(p$sampleType), ]


infinite_index = which(apply(e, 1, function(x){
  sum(is.infinite(x)) == length(x)
}))
if(!length(infinite_index)==0){
  e_infinite = e[infinite_index,]
  f_infinite = f[infinite_index,]

  e = e[-infinite_index,]
  f = f[-infinite_index,]
}

with_validate = any(!p$sampleType %in% c('qc','sample'))



# split e, and p to different sample type.
e_qc = e[, p$sampleType == 'qc']
e_sample = e[, p$sampleType == 'sample']

p_qc = p[p$sampleType == 'qc',]
p_sample = p[p$sampleType == 'sample',]


e_validates = list()

p_validates = list()
if(with_validate){


  val_RSDs = list()

  validate_types = unique(p$sampleType[!p$sampleType %in% c('qc','sample')])

  for(validate_type in validate_types){
    e_validates[[validate_type]] = e[, p$sampleType %in% validate_type]
    p_validates[[validate_type]] = p[p$sampleType %in% validate_type, ]

    val_RSDs[[validate_type]] = list()
  }
}else{
  validate_types = NULL
}


aggregate_e = function(e_qc,e_sample,e_validates){
  e = do.call('cbind',c(list(e_qc, e_sample), e_validates))
  e = e[,order(as.numeric(gsub("p","",colnames(e))))]
  return(e)
}






e_norm = matrix(,nrow=nrow(e),ncol=ncol(e))
QC.index = p[["sampleType"]]
batch = p[["batch"]]
time = p[["time"]]
batch = factor(batch)
num = 10
start = Sys.time();

cl = makeCluster(detectCores() * detectcores_ratio)

serrfR = function(train = e[,p$sampleType == 'qc'],
                  target = e[,p$sampleType == 'sample'],
                  num = 10,
                  batch. = factor(c(batch[p$sampleType=='qc'],batch[p$sampleType=='sample'])),
                  time. = c(time[p$sampleType=='qc'],time[p$sampleType=='sample']),
                  sampleType. = c(p$sampleType[p$sampleType=='qc'],p$sampleType[p$sampleType=='sample']),cl){


  all = cbind(train, target)
  normalized = rep(0, ncol(all))
  for(j in 1:nrow(all)){
    for(b in 1:length(unique(batch.))){
      current_batch = levels(batch.)[b]
      all[j,batch.%in%current_batch][all[j,batch.%in%current_batch] %in% 0] = rnorm(length(all[j,batch.%in%current_batch][all[j,batch.%in%current_batch] %in% 0]),mean = min(all[j,batch.%in%current_batch][!is.na(all[j,batch.%in%current_batch])])+1,sd = 0.1*(min(all[j,batch.%in%current_batch][!is.na(all[j,batch.%in%current_batch])])+.1))
      all[j,batch.%in%current_batch][is.na(all[j,batch.%in%current_batch])] = rnorm(length(all[j,batch.%in%current_batch][is.na(all[j,batch.%in%current_batch])]),mean = 0.5*min(all[j,batch.%in%current_batch][!is.na(all[j,batch.%in%current_batch])])+1,sd = 0.1*(min(all[j,batch.%in%current_batch][!is.na(all[j,batch.%in%current_batch])])+.1))
    }
  }

  corrs_train = list()
  corrs_target = list()
  for(b in 1:length(unique(batch.))){

    current_batch = levels(batch.)[b]

    train_scale = t(apply(train[,batch.[sampleType.=='qc']%in%current_batch],1,scale))
    if(is.null(target[,batch.[!sampleType.=='qc']%in%current_batch])){
      target_scale = t(apply(target[,batch.[!sampleType.=='qc']%in%current_batch],1,scale))
    }else{
      target_scale = scale(target[,batch.[!sampleType.=='qc']%in%current_batch])
    }

    # all_scale = cbind(train_scale, target_scale)

    # e_current_batch = all_scale
    corrs_train[[current_batch]] = cor(t(train_scale), method = "spearman")
    corrs_target[[current_batch]] = cor(t(target_scale), method = "spearman")
    # corrs[[current_batch]][is.na(corrs[[current_batch]])] = 0
  }




  pred = parSapply(cl, X = 1:nrow(all), function(j,all,batch.,ranger, sampleType., time., num,corrs_train,corrs_target){
    # for(j in 1:nrow(all)){
    # j = j+1
    print(j)
    normalized  = rep(0, ncol(all))
    qc_train_value = list()
    qc_predict_value = list()
    sample_value = list()
    sample_predict_value = list()

    for(b in 1:length(levels(batch.))){
      current_batch = levels(batch.)[b]
      e_current_batch = all[,batch.%in%current_batch]
      corr_train = corrs_train[[current_batch]]
      corr_target = corrs_target[[current_batch]]


      corr_train_order = order(abs(corr_train[,j]),decreasing = TRUE)
      corr_target_order = order(abs(corr_target[,j]),decreasing = TRUE)

      sel_var = c()
      l = num
      while(length(sel_var)<(num)){
        sel_var = intersect(corr_train_order[1:l], corr_target_order[1:l])
        sel_var = sel_var[!sel_var == j]
        l = l+1
      }



      train.index_current_batch = sampleType.[batch.%in%current_batch]
      train_data_y = scale(e_current_batch[j, train.index_current_batch=='qc'],scale=F)
      train_data_x = apply(e_current_batch[sel_var, train.index_current_batch=='qc'],1,scale)

      if(is.null(dim(e_current_batch[sel_var, !train.index_current_batch=='qc']))){
        test_data_x = t(scale(e_current_batch[sel_var, !train.index_current_batch=='qc']))
      }else{
        test_data_x = apply(e_current_batch[sel_var, !train.index_current_batch=='qc'],1,scale)
      }

      train_NA_index  = apply(train_data_x,2,function(x){
        sum(is.na(x))>0
      })

      train_data_x = train_data_x[,!train_NA_index]
      test_data_x = test_data_x[,!train_NA_index]

      if(!class(test_data_x)=="matrix"){
        test_data_x = t(test_data_x)
      }

      good_column = apply(train_data_x,2,function(x){sum(is.na(x))%in%0}) & apply(test_data_x,2,function(x){sum(is.na(x))%in%0})
      train_data_x = train_data_x[,good_column]
      test_data_x = test_data_x[,good_column]
      if(!class(test_data_x)=="matrix"){
        test_data_x = t(test_data_x)
      }
      train_data = data.frame(y = train_data_y,train_data_x )

      if(ncol(train_data)==1){# some samples have all QC constent.
        norm = e_current_batch[j,]
        normalized[batch.%in%current_batch] = norm
      }else{
        colnames(train_data) = c("y", paste0("V",1:(ncol(train_data)-1)))
        model = ranger(y~., data = train_data)

        test_data = data.frame(test_data_x)
        colnames(test_data) = colnames(train_data)[-1]

        norm = e_current_batch[j,]



        norm[train.index_current_batch=='qc'] = e_current_batch[j, train.index_current_batch=='qc']/((predict(model, data = train_data)$prediction+mean(e_current_batch[j,train.index_current_batch=='qc'],na.rm=TRUE))/mean(all[j,sampleType.=='qc'],na.rm=TRUE))

        norm[!train.index_current_batch=='qc'] =(e_current_batch[j,!train.index_current_batch=='qc'])/((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))/(median(all[j,!sampleType.=='qc'],na.rm = TRUE)))
        norm[!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]=e_current_batch[j,!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]




        norm[train.index_current_batch=='qc'] = norm[train.index_current_batch=='qc']/(median(norm[train.index_current_batch=='qc'],na.rm=TRUE)/median(all[j,sampleType.=='qc'],na.rm=TRUE))
        norm[!train.index_current_batch=='qc'] = norm[!train.index_current_batch=='qc']/(median(norm[!train.index_current_batch=='qc'],na.rm=TRUE)/median(all[j,!sampleType.=='qc'],na.rm=TRUE))
        norm[!is.finite(norm)] = rnorm(length(norm[!is.finite(norm)]),sd = sd(norm[is.finite(norm)],na.rm=TRUE)*0.01)




        out = boxplot.stats(norm, coef = 3)$out
        norm[!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']%in%out] = ((e_current_batch[j,!train.index_current_batch=='qc'])-((predict(model,data = test_data)$predictions  + mean(e_current_batch[j, !train.index_current_batch=='qc'],na.rm=TRUE))-(median(all[j,!sampleType.=='qc'],na.rm = TRUE))))[norm[!train.index_current_batch=='qc']%in%out];
        norm[!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]=e_current_batch[j,!train.index_current_batch=='qc'][norm[!train.index_current_batch=='qc']<0]
        normalized[batch.%in%current_batch] = norm

      }






    }







    return(normalized)
  },all,batch.,ranger, sampleType., time., num,corrs_train,corrs_target)




  normed = t(pred)

  normed_target = normed[,!sampleType.=='qc']


  for(i in 1:nrow(normed_target)){
    normed_target[i,is.na(normed_target[i,])] = rnorm(sum(is.na(normed_target[i,])), mean = min(normed_target[i,!is.na(normed_target[i,])], na.rm = TRUE), sd = sd(normed_target[i,!is.na(normed_target[i,])])*0.1)
  }
  for(i in 1:nrow(normed_target)){
    normed_target[i,normed_target[i,]<0] = runif(1) * min(normed_target[i,normed_target[i,]>0], na.rm = TRUE)
  }


  normed_train = normed[,sampleType.=='qc']


  for(i in 1:nrow(normed_train)){
    normed_train[i,is.na(normed_train[i,])] = rnorm(sum(is.na(normed_train[i,])), mean = min(normed_train[i,!is.na(normed_train[i,])], na.rm = TRUE), sd = sd(normed_train[i,!is.na(normed_train[i,])])*0.1)
  }
  for(i in 1:nrow(normed_train)){
    normed_train[i,normed_train[i,]<0] = runif(1) * min(normed_train[i,normed_train[i,]>0], na.rm = TRUE)
  }
  return(list(normed_train=normed_train,normed_target=normed_target))
}

serrf_normalized = e
serrf_normalized_modeled = serrfR(train = e_qc, target = e_sample, num = num,batch. = factor(c(p_qc$batch, p_sample$batch)),time. = c(p_qc$time, p_sample$time),sampleType. = c(p_qc$sampleType, p_sample$sampleType),cl)

serrf_qc = serrf_normalized_modeled$normed_train
colnames(serrf_qc) = colnames(e_qc)
serrf_sample = serrf_normalized_modeled$normed_target
colnames(serrf_sample) = colnames(e_sample)


serrf_cross_validated_qc = e_qc

cv = 5
RSDs = list()
if(any(table(p_qc$batch)<7)){
  ratio = 0.7
}else{
  ratio = 0.8
}

test_indexes = split(1L:nrow(p_qc), c(1L:nrow(p_qc))%%cv)
qc_left_pred = list()
for(k in 1:cv){

  print(paste0(k,'th cv'))
  test_index = test_indexes[[k]]
  train_index = c(1L:nrow(p_qc))[-test_index]

  train_index = sample(1L:sum(p$sampleType=='qc'),round(sum(p$sampleType=='qc')*ratio))
  test_index = c(1L:sum(p$sampleType=='qc'))[!(c(1L:sum(p$sampleType=='qc'))%in%train_index)]


  while(length(unique(p_qc$batch[test_index]))<length(unique(batch))){
    train_index = sample(1L:nrow(p_qc),round(nrow(p_qc)*ratio))
    test_index = c(1L:nrow(p_qc))[!(c(1L:nrow(p_qc))%in%train_index)]
  }
  serrf_normalized_on_cross_validate = serrfR(train = e_qc[,train_index], target = e_qc[,test_index], num = num,batch. = factor(c(p_qc$batch[train_index],p_qc$batch[test_index])),time. = c(p_qc$time[train_index],p_qc$time[test_index]),sampleType. = rep(c("qc","sample"),c(length(train_index),length(test_index))),cl)

  serrf_cross_validated_qc[,test_index] = serrf_normalized_on_cross_validate$normed_target

  qc_left_pred[[k]] = serrf_normalized_on_cross_validate$normed_target

  RSDs[[k]] = RSD(serrf_normalized_on_cross_validate$normed_target)
}
qc_left_pred_dfs = do.call('cbind', qc_left_pred)









rsd_each_comp = apply(do.call("cbind",RSDs),1,mean)



serrf_validates = list()
current_method_validate_RSDs = list()
if(with_validate){


  for(validate_type in validate_types){

    serrf_validates[[validate_type]] = serrfR(train = e_qc, target = e_validates[[validate_type]], num = num,batch. = factor(c(p_qc$batch, p_validates[[validate_type]]$batch)),time. = c(p_qc$time, p_validates[[validate_type]]$time),sampleType. = rep(c("qc","sample"),c(nrow(p_qc),nrow(p_validates[[validate_type]]))),cl)$normed_target

    colnames(serrf_validates[[validate_type]]) = colnames(e_validates[[validate_type]])


    current_method_validate_RSDs[[validate_type]] = RSD(serrf_validates[[validate_type]])



  }
  e_norm = aggregate_e(serrf_qc,serrf_sample,serrf_validates)
}else{
  e_norm = aggregate_e(serrf_qc,serrf_sample,NULL)
}

if(!length(infinite_index)==0){
  e_full = rbind(e_infinite, e)
  f_full = rbind(f_infinite, f)

  e_full[infinite_index,] = NA
  e_full[-infinite_index,] = e
  e = e_full

  f_full[infinite_index,] = f_infinite
  f_full[-infinite_index,] = f
  f = f_full

}


comb_p = rbind(p_empty_sampleType, p)
comb_e = cbind(e_empty_sampleType, e)

comb_p$sample_index = as.numeric(substring(comb_p$sample_index,2))

comb_e = comb_e[,order(comb_p$sample_index)]


if(!length(infinite_index)==0){
  for(i in 1:length(normalized_dataset)){

    e_full = rbind(e_infinite, normalized_dataset[[i]])
    f_full = rbind(f_infinite, f)

    e_full[infinite_index,] = NA
    e_full[-infinite_index,] = normalized_dataset[[i]]
    normalized_dataset[[i]] = e_full


  }
}

e_norm = cbind(e_empty_sampleType, e_norm)
e_norm = e_norm[,order(comb_p$sample_index)]

comb_p = comb_p[order(sample_index),]

stopCluster(cl)
gc()

cat("<!--------- SERRF done.--------->\n")









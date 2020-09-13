pacman::p_load(parallel)
detectcores_ratio = 1
e_norm = matrix(,nrow=nrow(e_raw),ncol=ncol(e_raw))
QC.index = p[["sampleType"]]
batch = p[["batch"]]
time = as.numeric(p[["time"]])
n_CV = 5
QC.index.train = QC.index.test = list()
n_CV = 5
seed = 1
set.seed(seed)
QC.index.train = QC.index.test = list()
e_qc_only = e_raw[,p$sampleType=='qc']
if(any(table(p$batch[p$sampleType=='qc']))<7){
  ratio = 0.7
}else{
  ratio = 0.8
}
for(j in 1:n_CV){
  QC.index.train.temp = sample(1L:ncol(e_qc_only),round(ncol(e_qc_only)*ratio))
  QC.index.test.temp = c(1L:ncol(e_qc_only))[!c(1L:ncol(e_qc_only))%in%QC.index.train.temp]
  QC.index.train. = rep('sample',ncol(e_qc_only))
  QC.index.test. = rep('sample',ncol(e_qc_only))
  QC.index.train.[QC.index.train.temp] = 'qc'
  QC.index.test.[QC.index.test.temp] = 'qc'
  QC.index.train[[j]] = QC.index.train.
  QC.index.test[[j]] = QC.index.test.
}
svm_fun_cv = function(e_raw,train.index = QC.index,test.index=NULL,batch,time){
  cl = makeCluster(detectCores() * detectcores_ratio)
  e_norm = parSapply(cl, X=1:nrow(e_raw), function(i,e_raw,train.index,batch,time,remove_outlier,trainControl,train){
    # for(i in 1:nrow(e_raw)){
    tryCatch({
      line = e_raw[i,]
      for(b in 1:length(unique(batch))){
        outlier_remove = remove_outlier(e_raw[i,(batch %in% unique(batch)[b]) & train.index=='qc'])
        if(length(outlier_remove$index) == 0){
          dta = data.frame(x = time[(batch %in% unique(batch)[b]) & train.index=='qc'], y = e_raw[i,(batch %in% unique(batch)[b]) & train.index=='qc'])
          lm = train(y~., data=dta, method = "svmLinear", trControl = trainControl(method = "cv", savePred=T))
        }else{
          dta = data.frame(x = time[(batch %in% unique(batch)[b]) & train.index=='qc'][-outlier_remove$index], y = e_raw[i,(batch %in% unique(batch)[b]) & train.index=='qc'][-outlier_remove$index])
          lm = train(y~., data=dta, method = "svmLinear", trControl = trainControl(method = "cv", savePred=T))
        }
        line[batch %in% unique(batch)[b]] = predict(lm,newdata  = data.frame(x = time[batch %in% unique(batch)[b]]))
      }
      if(length(which(is.na(line)))>0){
        for(j in which(is.na(line))){
          time_notNA = time[-which(is.na(line))]
          closest_time = time_notNA[which.min(abs(time_notNA - time[j]))]
          line[j] = line[which(time==closest_time)]
        }
      }
      e_norm = e_raw[i,] / (line / median(e_raw[i,], na.rm = TRUE))
    },error = function(er){
      e_raw[i,]
    })
    
    # }
    
  },e_raw,train.index,batch,time,remove_outlier,trainControl,train)
  stopCluster(cl)
  e_norm = t(e_norm)
  if(!is.null(test.index)){
    rsd = RSD(e_norm[,test.index=='qc'])
  }else(
    rsd = NULL
  )
  return(list(data = e_norm,rsd = rsd))
}
rsds = matrix(,nrow = nrow(e_raw), ncol = n_CV)
for(i in 1:n_CV){
  rsds[,i] = svm_fun_cv(e_raw=e_qc_only,train.index=QC.index.train[[i]],test.index = QC.index.test[[i]],batch = batch[p$sampleType=='qc'],time=time[p$sampleType=='qc'])[[2]]
}
rsd_each_comp = qc_RSD = apply(rsds,1,mean,na.rm = TRUE)
e_norm = svm_fun_cv(e_raw,train.index = QC.index,test.index=NULL,batch=batch,time=time)[[1]]
rownames(e_norm) = rownames(e_raw)
colnames(e_norm) = colnames(e_raw)
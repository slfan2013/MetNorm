pacman::p_load(parallel)

e_norm = matrix(,nrow=nrow(e),ncol=ncol(e))
QC.index = p[["sampleType"]]
batch = p[["batch"]]
time = as.numeric(p[["time"]])
n_CV = 5
QC.index.train = QC.index.test = list()
n_CV = 5
seed = 42
set.seed(seed)
QC.index.train = QC.index.test = list()
e_qc_only = e[,p$sampleType=='qc']
if(any(table(p$batch[p$sampleType=='qc'])<7)){
  ratio = 0.7
}else{
  ratio = 0.8
}




for(j in 1:n_CV){
  set.seed(j)
  QC.index.train.temp = sample(1L:ncol(e_qc_only),round(ncol(e_qc_only)*ratio))
  QC.index.test.temp = c(1L:ncol(e_qc_only))[!c(1L:ncol(e_qc_only))%in%QC.index.train.temp]
  QC.index.train. = rep('sample',ncol(e_qc_only))
  QC.index.test. = rep('sample',ncol(e_qc_only))
  QC.index.train.[QC.index.train.temp] = 'qc'
  QC.index.test.[QC.index.test.temp] = 'qc'

  QC.index.train[[j]] = QC.index.train.
  QC.index.test[[j]] = QC.index.test.
}
detectcores_ratio = 1
loess_fun_cv = function(e,train.index = QC.index,test.index=NULL,batch,time){
  # whether remove outlier or not.
  remove_outlier = function(v){
    out = boxplot.stats(1)$out
    return(list(value = v[!v%in%out],index = which(v%in%out)))
  }
  loess_wrapper_extrapolate <- function (x, y, span.vals = seq(0.25, 1, by = 0.05), folds = 5){
    # Do model selection using mean absolute error, which is more robust than squared error.
    mean.abs.error <- numeric(length(span.vals))
    
    # Quantify error for each span, using CV
    loess.model <- function(x, y, span){
      loess(y ~ x, span = span, control=loess.control(surface="interpolate",statistics='exact'),family = "gaussian")
    }
    
    loess.predict <- function(fit, newdata) {
      predict(fit, newdata = newdata)
    }
    
    span.index <- 0
    
    for (each.span in span.vals) {
      span.index <- span.index + 1
      mean.abs.error[span.index] = tryCatch({
        y.hat.cv <- bootstrap::crossval(x, y, theta.fit = loess.model, theta.predict = loess.predict, span = each.span, ngroup = folds)$cv.fit
        non.empty.indices <- !is.na(y.hat.cv)
        diff = (y[non.empty.indices] / y.hat.cv[non.empty.indices]) * mean(y[non.empty.indices])
        sd(diff)/mean(diff)
      },error = function(er){
        NA
      })
    }
    best.span <- span.vals[which.min(mean.abs.error)]
    if(length(best.span)==0){
      best.span = 0.75
    }
    
    best.model <- loess(y ~ x, span = best.span, control=loess.control(surface="interpolate",statistics='exact'),family = "gaussian")
    
    return(list(best.model, min(mean.abs.error, na.rm = TRUE),best.span))
  }
  cl = makeCluster(detectCores() * detectcores_ratio)
  e_norm = parSapply(cl, X=1:nrow(e), function(i,e,train.index,batch,time,remove_outlier,loess_wrapper_extrapolate){
    
    e_norm = tryCatch({
      # for(i in 1:nrow(e)){
      line = e[i,]
      for(b in 1:length(unique(batch))){
        outlier_remove = remove_outlier(e[i,(batch %in% unique(batch)[b]) & (train.index=='qc')])
        if(length(outlier_remove$index) == 0){
          lm = loess_wrapper_extrapolate(x=time[(batch %in% unique(batch)[b]) & (train.index=='qc')], y = e[i,(batch %in% unique(batch)[b]) & (train.index=='qc')])[[1]]
        }else{
          lm = loess_wrapper_extrapolate(x=time[(batch %in% unique(batch)[b]) & (train.index=='qc')][-outlier_remove$index], y = e[i,(batch %in% unique(batch)[b]) & (train.index=='qc')][-outlier_remove$index])[[1]]
        }
        line[batch %in% unique(batch)[b]] = predict(lm,newdata  = time[batch %in% unique(batch)[b]])
        
        
        if(length(which(is.na(line[batch %in% unique(batch)[b]])))>0){
          for(j in which(is.na(line[batch %in% unique(batch)[b]]))){
            time_notNA = time[batch %in% unique(batch)[b]][-which(is.na(line[batch %in% unique(batch)[b]]))]
            closest_time = time_notNA[which.min(abs(time_notNA - time[batch %in% unique(batch)[b]][j]))]
            line[batch %in% unique(batch)[b]][j] = line[batch %in% unique(batch)[b]][which(time[batch %in% unique(batch)[b]]==closest_time)]
          }
        }
        
      }
      
      if(sum(line<0)>(length(line)/5)){
        stop("too many negative value. LOESS failed.")
      }else{
        line[line<0] = runif(sum(line<0), min = max(c(median(e[i,], na.rm = TRUE) -  0.1 * sd(e[i,], na.rm = TRUE),0)), max = max(c(median(e[i,], na.rm = TRUE) +  0.1 * sd(e[i,], na.rm = TRUE),1)))
      }
      
      # if(length(which(is.na(line)))>0){
      #   for(j in which(is.na(line))){
      #     time_notNA = time[-which(is.na(line))]
      #     closest_time = time_notNA[which.min(abs(time_notNA - time[j]))]
      #     line[j] = line[which(time==closest_time)]
      #   }
      # }
      e[i,] / (line / median(e[i,], na.rm = TRUE))
      # if(sum((e[i,] / (line / median(e[i,], na.rm = TRUE)))<0)){
      #   stop(i)
      # }
      # }
      
      
    },error = function(er){
      e[i,]
    })
    return(e_norm)
  },e,train.index,batch,time,remove_outlier,loess_wrapper_extrapolate)
  stopCluster(cl)
  e_norm = t(e_norm)
  if(!is.null(test.index)){
    rsd = RSD(e_norm[,test.index=='qc'])
  }else(
    rsd = NULL
  )
  return(list(data = e_norm,rsd = rsd,left_out = e_norm[,test.index=='qc']))
}







rsds = matrix(,nrow = nrow(e), ncol = n_CV)
# loess_fun_cvs = list()
qc_left_pred = list()
for(i in 1:n_CV){
  temp = loess_fun_cv(e=e_qc_only,train.index=QC.index.train[[i]],test.index = QC.index.test[[i]],batch = batch[p$sampleType=='qc'],time=time[p$sampleType=='qc'])
  # loess_fun_cvs[[i]] = temp[[1]]
  rsds[,i] = temp[[2]]
  qc_left_pred[[i]] = temp[[3]]
}
qc_left_pred_dfs = do.call('cbind', qc_left_pred)
rsd_each_comp = qc_RSD = apply(rsds, 1, median, na.rm =TRUE)
# loess_fun_cvs2 =list()

# for(i in 1:length(loess_fun_cvs)){
#   
#   loess_fun_cvs2[[i]] = loess_fun_cvs[[i]][,QC.index.test[[i]]%in%'qc']
#   
# }


e_norm = loess_fun_cv(e,train.index = QC.index,test.index=NULL,batch=batch,time=time)[[1]]



# qc_left_pred_df = do.call('cbind',qc_left_pred)
# 
# biorec_serda = qc_left_pred_df
# biorec_serda = biorec_serda[,c(1:ncol(biorec_serda))[c(1:ncol(biorec_serda)) %% 20]==1]
# biorec_serda_cor = cor(biorec_serda, method = 'pearson', use = "complete.obs")
# breaksList = seq(0.5,1,by=0.001)
# pheatmap(biorec_serda_cor, color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)),breaks = breaksList,show_rownames = F, show_colnames = F)



rownames(e_norm) = rownames(e)
colnames(e_norm) = colnames(e)

gc()




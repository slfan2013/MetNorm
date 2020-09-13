
input_file <- function(file = "P20 positive mode FULL.csv") {
  # file = "P20 positive mode CUT.csv"
  source("https://raw.githubusercontent.com/slfan2013/rcodes/master/read_data.R")
  data = read_data(file)

  pacman::p_load(paws.database, jsonlite)



  task_id = as.character(as.integer(Sys.time()))
  # Put bins to a collection.
  data_collection_id = "5f0de8dcc1edc4661756e681"
  # Posting multipart
  library(curl)
  library(magrittr)
  new_bin_id_matching = list()
  cb <- function(req){rrr <<- req; new_bin_id_matching[[length(new_bin_id_matching)+1]] <<- fromJSON(rawToChar(rrr$content))[c("id","binName")];cat("done:", req$url, ": HTTP:", req$status, "\n")}
  pool <- new_pool()
  data$e_matrix = cbind(1:nrow(data$e_matrix),data$e_matrix[1:nrow(data$e_matrix),]) # Will try if the system will block later once the Process went through; Check how long it blocks. Check if changing IP will prevent blocking.

  handels = list()
  for(i in 1:nrow(data$e_matrix)){
      if(i %% 10 == 1){
        if(i+9 < nrow(data$e_matrix)){
          print(i)

          handels[[length(handels)+1]] = new_handle(
            copypostfields = jsonlite::toJSON(data$e_matrix[i:(i+9),],auto_unbox = TRUE,force = TRUE)
          )%>%
            handle_setheaders(
              'Content-Type'='application/json',
              'secret-key'=get_key(),
              "name" = paste0(task_id,"-",i),
              "collection-id" = data_collection_id
            )%>%handle_setopt(customrequest = "POST")
        }else{
          print(i)
          handels[[length(handels)+1]] = new_handle(
            copypostfields = jsonlite::toJSON(matrix(data$e_matrix[i:nrow(data$e_matrix),], ncol = ncol(data$e_matrix)),auto_unbox = TRUE,force = TRUE)
          )%>%
            handle_setheaders(
              'Content-Type'='application/json',
              'secret-key'=get_key(),
              "name" = paste0(task_id,"-",i),
              "collection-id" = data_collection_id
            )%>%handle_setopt(customrequest = "POST")
        }

        curl_fetch_multi('https://api.jsonbin.io/b', done = cb, pool = pool, handle  = handels[[length(handels)]])
      }
  }
  out <- multi_run(pool = pool)

  # also post the f and p to the bin.
  post_f = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id,'-f'),"collection-id" = data_collection_id), postfields = jsonlite::toJSON(data$f,auto_unbox = TRUE,force = TRUE)))
  post_f_id = post_f$id
  new_bin_id_matching[[length(new_bin_id_matching)+1]] = list(id = post_f_id, binName = paste0(task_id,'-f'))

  post_p = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id,'-p'),"collection-id" = data_collection_id), postfields = jsonlite::toJSON(data$p,auto_unbox = TRUE,force = TRUE)))
  post_p_id = post_p$id
  new_bin_id_matching[[length(new_bin_id_matching)+1]] = list(id = post_p_id, binName = paste0(task_id,'-p'))


  # also provide PCA data.
  e_raw = data$e_matrix
  sds = apply(e_raw,1,sd)
  e_raw_pca = e_raw[!is.na(sds),]
  pca_raw = prcomp(t(e_raw_pca), scale. = TRUE)
  pca_raw_score = pca_raw$x[,c(1,2)]
  pca_raw_var_exp = format_number((pca_raw$sdev^2)[1:2]/sum(pca_raw$sdev^2))
  # post raw PCA to bin.
  raw_info = list(PCA = list(scores = pca_raw_score, var_exp = pca_raw_var_exp))
  if('qc' %in% data$p$sampleType){
    has_qc = TRUE
  }
  if(length(unique(data$p$sampleType)[!unique(data$p$sampleType) %in% c("qc",'sample',"")])>0){
    has_validate = TRUE
  }
  if(has_qc){
    raw_info$raw_qc_RSD = format_number(median(RSD(data$e_matrix[,data$p$sampleType == 'qc']), na.rm = TRUE))
  }
  if(has_validate){
    validates = unique(data$p$sampleType)
    validates = validates[!validates %in% c("qc",'sample','')]
    for(i in 1:length(validates)){
      raw_info[[paste0("raw_",validates[i],"_RSD")]] = format_number(median(RSD(data$e_matrix[,data$p$sampleType == validates[i]]), na.rm = TRUE))
    }
  }

  post_raw = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id,'-raw'),"collection-id" = data_collection_id), postfields = jsonlite::toJSON(raw_info,auto_unbox = TRUE,force = TRUE)))
  post_raw_id = post_raw$id
  new_bin_id_matching[[length(new_bin_id_matching)+1]] = list(id = post_raw_id, binName = paste0(task_id,'-raw'))



  # put the job_id relation to collection id.
  if(out$error == 0){# if there is no error, put the job_id information to the collection.

    # new_bin_id_matching = do.call("rbind",new_bin_id_matching)

    # print(length(new_bin_id_matching))
    return(list(new_bin_id_matching=new_bin_id_matching,task_id = task_id))

  }else{
    stop("Error happened when uploading dataset.")
  }










}

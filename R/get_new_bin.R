get_new_bin = function(new_record_ids = c("5f39bed8b88c04101cf543d0")){
  pacman::p_load('jsonlite')


  current_new_records = list()
  for(i in 1:length(new_record_ids)){
    current_new_record_id = new_record_ids[i]
    current_new_record = fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",current_new_record_id,"/latest"), customrequest='GET', httpheader=c('secret-key'=get_key())))
    current_new_records[[i]] = current_new_record
  }

  return(current_new_records)
}

# fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/","5f1a046fc58dc34bf5d978fb","/latest"), customrequest='GET', httpheader=c('secret-key'=get_key())))$method


# object = jsonlite::toJSON(data$e_matrix[sample(1:25, 100, replace = TRUE),],auto_unbox = TRUE,force = TRUE)
# print(format(object.size(object),"MB"))
#
#
# test = RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id,'-f'),"collection-id" = data_collection_id), postfields = object)
# fromJSON(test)$success

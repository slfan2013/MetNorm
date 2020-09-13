
submit_job <- function(task_id = "1595268547", normalization_method =  c("raw","mtic",'sum'),new_bin_id_matching) {
  # normalization_method =  c("raw","mtic",'sum')
  pacman::p_load(paws.database, jsonlite)
  task_id = as.character(task_id)
  queue_collection_id = "5f0de9c79180616628417e7c"


  # Add a new collection first. Return new_collection_id.
  status_create_collection = fromJSON(RCurl::getURL("https://api.jsonbin.io/c", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key()),postfields = jsonlite::toJSON(list(name = paste0("task-",task_id,"-collection")),auto_unbox = TRUE,force = TRUE)))
  if(status_create_collection$success){
    new_collection_id = status_create_collection$id
  }else{
    return(status_create_collection$message)
  }


  # Then, Add new bin containing  new_bin_id_matching, method, task_id and new_collection_id to queue_collection
  new_bin_content = list(
    new_bin_id_matching = new_bin_id_matching,
    method = normalization_method,
    task_id = task_id,
    new_collection_id = new_collection_id
  )

  new_queue_job = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0("task-",task_id), "collection-id" = queue_collection_id), postfields = jsonlite::toJSON(new_bin_content,auto_unbox = TRUE,force = TRUE)))


  # Third, return p and f.
  ids = sapply(new_bin_id_matching[,"id"], function(x){x})
  raw_info_index = sapply(new_bin_id_matching[,"binName"],function(x){grepl("raw",x)})
  p_index = sapply(new_bin_id_matching[,"binName"],function(x){grepl("p",x)})
  f_index = sapply(new_bin_id_matching[,"binName"],function(x){grepl("f",x)})
  headers=c('secret-key'=get_key())
  raw_info = jsonlite::fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",ids[raw_info_index]), customrequest='GET', httpheader=headers))
  p = jsonlite::fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",ids[p_index]), customrequest='GET', httpheader=headers))
  f = jsonlite::fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",ids[f_index]), customrequest='GET', httpheader=headers))


#
#
#
#
#
#   # First, add method to the bin by: create a bin in the collection ...ce6 with name task_id + method
#   status_method = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id, "-method" )), postfields = jsonlite::toJSON(list(paste0(normalization_method,collapse = ",")),auto_unbox = TRUE,force = TRUE)))
#
#   # Second, add status to be "waiting_in_queue".
#   status_status = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id, "-status" )), postfields = jsonlite::toJSON(list("waiting_in_queue"),auto_unbox = TRUE,force = TRUE)))
#
#
#   # Third, add new jobs to the bin_id_matching
#   # 1. Get the bin.
#   bin_id_matching = fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",queue_collection_id), customrequest='GET', httpheader=c('secret-key'=get_key())))
#   # 2. Update the bin.
#   updated_bin_id_matching = rbind(bin_id_matching, new_bin_id_matching)
#   updated_bin_id_matching = rbind(updated_bin_id_matching,data.frame(id = status_method$id,binName = paste0(task_id, "-method")))
#   updated_bin_id_matching = rbind(updated_bin_id_matching,data.frame(id = status_status$id,binName = paste0(task_id, "-status")))
#
#
#   status_update = fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",queue_collection_id), customrequest='PUT', httpheader=c('Content-Type'='application/json','secret-key'=get_key()), postfields = jsonlite::toJSON(updated_bin_id_matching,auto_unbox = TRUE,force = TRUE)))
#
#   # Fourth, create a new collection-id, so that we can know when the normalization is finished.
#   status_create_collection = fromJSON(RCurl::getURL("https://api.jsonbin.io/c", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key()),postfields = jsonlite::toJSON(list(name = paste0(task_id,"-collection")),auto_unbox = TRUE,force = TRUE)))




  return(list(status = ifelse(new_queue_job$success,"success","failed"), new_collection_id = new_collection_id,p = p,f = f,raw_info = raw_info))

}

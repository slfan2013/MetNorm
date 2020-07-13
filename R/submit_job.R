
submit_job <- function(task_id = "1594672239", normalization_method = "serda",new_bin_id_matching) {
  # normalization_method = "serda"
  pacman::p_load(paws.database, jsonlite)
  task_id = as.character(task_id)
  bin_id_matching_id = "5f0ca06aa62f9b4b276448ed"
  collection_id = "5f07e238a62f9b4b27621ce6"


  # First, add method to the bin by: create a bin in the collection ...ce6 with name task_id + method
  status_method = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id, "-method" )), postfields = jsonlite::toJSON(list(paste0(normalization_method,collapse = ",")),auto_unbox = TRUE,force = TRUE)))

  # Second, add status to be "waiting_in_queue".
  status_status = fromJSON(RCurl::getURL("https://api.jsonbin.io/b", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id, "-status" )), postfields = jsonlite::toJSON(list("waiting_in_queue"),auto_unbox = TRUE,force = TRUE)))


  # Third, add new jobs to the bin_id_matching
  # 1. Get the bin.
  bin_id_matching = fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",bin_id_matching_id), customrequest='GET', httpheader=c('secret-key'=get_key())))
  # 2. Update the bin.
  updated_bin_id_matching = rbind(bin_id_matching, new_bin_id_matching)
  updated_bin_id_matching = rbind(updated_bin_id_matching,data.frame(id = status_method$id,binName = paste0(task_id, "-method")))
  updated_bin_id_matching = rbind(updated_bin_id_matching,data.frame(id = status_status$id,binName = paste0(task_id, "-status")))


  status_update = fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",bin_id_matching_id), customrequest='PUT', httpheader=c('Content-Type'='application/json','secret-key'=get_key()), postfields = jsonlite::toJSON(updated_bin_id_matching,auto_unbox = TRUE,force = TRUE)))

  # Fourth, create a new collection-id, so that we can know when the normalization is finished.
  status_create_collection = fromJSON(RCurl::getURL("https://api.jsonbin.io/c", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key()),postfields = jsonlite::toJSON(list(name = paste0(task_id,"-collection")),auto_unbox = TRUE,force = TRUE)))



  return(list(status = ifelse(status_update$success,"success","failed"), collection_id = status_create_collection$id))

}

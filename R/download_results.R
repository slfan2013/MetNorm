download_results = function(normalized_datasets_bin_id_matching, RSDs_df, sample_label){
  pacman::p_load(data.table, curl, magrittr, jsonlite)
  # print(RSDs_df)
  # save(normalized_datasets_bin_id_matching, RSDs_df, sample_label, file = "result.RData")

  Sys.sleep(2)
  filenames = "RSDs.csv"
  fwrite(RSDs_df,filenames)

  for(m in 1:length(normalized_datasets_bin_id_matching)){
    e_list = list()
    dataset_ids = normalized_datasets_bin_id_matching[[m]]$id

    pool <- new_pool()
    cb <- function(req){
      rrr = req
      json_result = fromJSON(rawToChar(req$content))
      e_list[[length(e_list)+1]] <<- json_result
    }
    for(i in 1:length(dataset_ids)){

      curl_fetch_multi(paste0("https://api.jsonbin.io/b/",dataset_ids[i]), done = cb, pool = pool,handle = new_handle()%>%handle_setheaders('secret-key'=get_key()))


    }
    out <- multi_run(pool = pool)
    e_unordered = do.call('rbind',e_list)
    e_ordered = e_unordered[order(e_unordered[,1], decreasing = FALSE),]
    e = e_ordered[,-1]
    colnames(e) = sample_label

    filenames[length(filenames)+1] = paste0("normalized_data_",names(normalized_datasets_bin_id_matching)[m],".csv")
    fwrite(data.table(label = RSDs_df$label,e),filenames[length(filenames)])

  }

  return(list(status = TRUE, filenames = filenames))
}

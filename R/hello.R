# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
  # restdb
  # Server API-key (full access): 2071940744066a0e786115d88f3bbae1fe4d9
  # create job (data) under a collection
  # url = 'https://testtest-ca74.restdb.io/rest/testcollection'
  # headers = c("content-type"="application/json",
  #             "x-apikey"="2071940744066a0e786115d88f3bbae1fe4d9",
  #             "cache-control"="no-cache")
  # init_data = list(e = data$e_matrix[1,]); #1114.262 milliseconds
  # postfields = jsonlite::toJSON(init_data,auto_unbox = TRUE,force = TRUE)
  # microbenchmark::microbenchmark({
  #   result = RCurl::getURL(url, customrequest='POST', httpheader=headers,postfields=postfields)
  # })
  #
  # result

  # data_id = as.integer(Sys.time())




  #   # Put data to cloud.
  # svc <- dynamodb(
  #   config = list(
  #     credentials = list(
  #       creds = get_key()
  #       # ,profile = "string"
  #     ),
  #     endpoint = "https://dynamodb.us-west-1.amazonaws.com",
  #     region = "us-west-1"
  #   )
  # )

  # create table.
  # svc$create_table(
  #   AttributeDefinitions = list(
  #     list(
  #       AttributeName = "job_id",
  #       AttributeType = "S"
  #     ),
  #     list(
  #       AttributeName = "data_id",
  #       AttributeType = "S"
  #     )
  #   ),
  #   KeySchema = list(
  #     list(
  #       AttributeName = "job_id",
  #       KeyType = "HASH"
  #     ),
  #     list(
  #       AttributeName = "data_id",
  #       KeyType = "RANGE"
  #     )
  #   ),
  #   ProvisionedThroughput = list(
  #     ReadCapacityUnits = 500L,
  #     WriteCapacityUnits = 500L
  #   ),
  #   TableName = "SERDA"
  # )


  # put_item
  # 124.1012 milliseconds
  #  start = Sys.time()
  #  for(i in 1:nrow(data$e_matrix)){
  #    if(i %% 10 == 1){
  #      print(i)
  #      tryCatch({
  #        a=svc$put_item(
  #          Item = list(
  #            job_id = list(S = paste0(data_id,"_",i,"_",i+9)),
  #            data_id = list(S = data_id),
  #            data = list(S = jsonlite::toJSON(data$e_matrix[i:(i+9),],auto_unbox = TRUE,force = TRUE))
  #          ),
  #          ReturnConsumedCapacity = "TOTAL",
  #          TableName = "SERDA"
  #        )
  #      }, error = function(e){
  #
  #      })
  #      # microbenchmark::microbenchmark({
  #
  #      # })
  #
  #    }
  #  }
  #
  # i = tail(which(1:nrow(data$e_matrix) %% 10==1), n = 1)
  #
  #
  #   svc$put_item(
  #     Item = list(
  #       job_id = list(S = paste0(data_id,"_",i,"_",i+9)),
  #       data_id = list(S = data_id),
  #       data = list(S = jsonlite::toJSON(data$e_matrix[i:nrow(data$e_matrix),],auto_unbox = TRUE,force = TRUE))
  #     ),
  #     ReturnConsumedCapacity = "TOTAL",
  #     TableName = "SERDA"
  #   )
  #   time_consumed = as.numeric(Sys.time() - start)
  #
  #   svc$put_item(
  #     Item = list(
  #       job_id = list(S = paste0(data_id,"_f")),
  #       data_id = list(S = data_id),
  #       data = list(S = jsonlite::toJSON(data$f,auto_unbox = TRUE,force = TRUE))
  #     ),
  #     ReturnConsumedCapacity = "TOTAL",
  #     TableName = "SERDA"
  #   )
  #   svc$put_item(
  #     Item = list(
  #       job_id = list(S = paste0(data_id,"_p")),
  #       data_id = list(S = data_id),
  #       data = list(S = jsonlite::toJSON(data$p,auto_unbox = TRUE,force = TRUE))
  #     ),
  #     ReturnConsumedCapacity = "TOTAL",
  #     TableName = "SERDA"
  #   )






  # get_item
  # item = svc$get_item(
  #   Key = list(
  #     job_id = list(
  #       S = "1593110039"
  #     )
  #   ),
  #   TableName = "SERDA_jobs"
  # )


  # delete table.
  # svc$delete_table(
  #   TableName = "Music"
  # )
  # var req=ocpu.call("input_file",{
  #
  # },function(session){
  #   s = session
  #
  # }).done(function(){
  #
  # }).fail(function(){
  #
  # })



  # jsonbin
  # url = "https://api.jsonbin.io/b"
  # headers=c('Content-Type'='application/json','secret-key'=get_key(),"collection-id" = "5f07e238a62f9b4b27621ce6")
  #
  # start = Sys.time()
  #
  #   for(i in 1:nrow(data$e_matrix)){
  #     if(i %% 10 == 1){
  #       print(i)
  #     postfields = jsonlite::toJSON(data$e_matrix[i:(i+9),],auto_unbox = TRUE,force = TRUE)
  #   }
  #   a = RCurl::getURL(url, customrequest='POST', httpheader=headers,postfields=postfields)
  #   }
  #
  #
  # library(curl)
  # pool <- new_pool()
  # cb <- function(req){cat("done:", req$url, ": HTTP:", req$status, "\n")}
  # curl_fetch_multi('https://www.google.com', done = cb, pool = pool)
  # curl_fetch_multi('https://cloud.r-project.org', done = cb, pool = pool)
  # curl_fetch_multi('https://httpbin.org/blabla', done = cb, pool = pool)
  #
  # out <- multi_run(pool = pool)
  # 0. Create the bin.
  # bin_id_matching_id = "5f0ca06aa62f9b4b276448ed"
  # done = function(req){
  #   print(fromJSON(rawToChar(req$content))[["id"]])
  # }
  # pool = new_pool()
  # h = new_handle(
  #   copypostfields = jsonlite::toJSON(list(list(id = "1", binName = "1")),auto_unbox = TRUE,force = TRUE)
  # )%>%
  #   handle_setheaders(
  #     'Content-Type'='application/json',
  #     'secret-key'=get_key(),
  #     name = "bin_id_matching"
  #   )%>%handle_setopt(customrequest = "POST")
  # curl_fetch_multi('https://api.jsonbin.io/b', done = done, pool = pool, handle  = h)
  # out <- multi_run(pool = pool)

  # 1. Get the bin.
  # bin_id_matching = fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",bin_id_matching_id), customrequest='GET', httpheader=c('secret-key'=get_key())))
  #
  # updated_bin_id_matching = rbind(bin_id_matching, do.call("rbind",new_bin_id_matching))
  #
  # # 2. Update the bin.
  # fromJSON(RCurl::getURL(paste0("https://api.jsonbin.io/b/",bin_id_matching_id), customrequest='PUT', httpheader=c('Content-Type'='application/json','secret-key'=get_key()), postfields = jsonlite::toJSON(updated_bin_id_matching,auto_unbox = TRUE,force = TRUE)))




  # get all bin from a collection
  # while(TRUE){
  #   start = Sys.time()
    url = paste0("https://api.jsonbin.io/e/collection/",collection_id,"/all-bins")
    headers=c('secret-key'=get_key())
    all_bin_result = fromJSON(RCurl::getURL(url, customrequest='GET', httpheader=headers))
    print(Sys.time() - start)
  #
  #   # check if number of bins increased. If so, then it means a new job is submitted.
  #   new_bin_added = FALSE
  #   if(new_bin_added){
  #
  #
  #
  #
  #   }
  #
  # }

  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #   Sys.time() - start
  #
  #   # data = list("Sample" = "Hello World")
  #   # data = d
  #   postfields = toJSON(data$e_matrix[1,],auto_unbox = TRUE,force = TRUE)
  #   # format(object.size(data), units = "MB")
  #   # start = Sys.time()
  #   microbenchmark::microbenchmark({
  #     a = RCurl::getURL(url, customrequest='POST', httpheader=headers,postfields=postfields)
  #   })
  #   result = RCurl::getURL(url, customrequest='POST', httpheader=headers,postfields=postfields)
  #
  #
  #
  #   for(i in 1:nrow(data$e_matrix)){
  #     if(i %% 10 == 1){
  #       print(i)
  #       tryCatch({
  #         a=svc$put_item(
  #           Item = list(
  #             job_id = list(S = paste0(data_id,"_",i,"_",i+9)),
  #             data_id = list(S = data_id),
  #             data = list(S = jsonlite::toJSON(data$e_matrix[i:(i+9),],auto_unbox = TRUE,force = TRUE))
  #           ),
  #           ReturnConsumedCapacity = "TOTAL",
  #           TableName = "SERDA"
  #         )
  #       }, error = function(e){
  #
  #       })
  #       # microbenchmark::microbenchmark({
  #
  #       # })
  #
  #     }
  #   }



  # if there is no error, save which methods use wanted.
  # if(out$error == 0){
  #   fromJSON(RCurl::getURL("https://api.jsonbin.io/b/", customrequest='POST', httpheader=c('Content-Type'='application/json','secret-key'=get_key(),name = paste0(task_id,"-methods"),"collection-id" = collection_id), postfields = jsonlite::toJSON(list(methods = ""),auto_unbox = TRUE,force = TRUE)))
  # }
}

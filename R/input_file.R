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

input_file <- function(file = "P20 positive mode FULL.csv") {
  # file = "P20 positive mode FULL.csv"
  source("https://raw.githubusercontent.com/slfan2013/rcodes/master/read_data.R")
  data = read_data(file)

  pacman::p_load(paws.database, jsonlite)

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

  data_id = as.integer(Sys.time())





#   # Put data to cloud.
  svc <- dynamodb(
    config = list(
      credentials = list(
        creds = get_key()
        # ,profile = "string"
      ),
      endpoint = "https://dynamodb.us-west-1.amazonaws.com",
      region = "us-west-1"
    )
  )

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
  start = Sys.time()
  for(i in 1:nrow(data$e_matrix)){
    if(i %% 10 == 1){
      print(i)
      tryCatch({
        a=svc$put_item(
          Item = list(
            job_id = list(S = paste0(data_id,"_",i,"_",i+9)),
            data_id = list(S = data_id),
            data = list(S = jsonlite::toJSON(data$e_matrix[i:(i+9),],auto_unbox = TRUE,force = TRUE))
          ),
          ReturnConsumedCapacity = "TOTAL",
          TableName = "SERDA"
        )
      }, error = function(e){

      })
      # microbenchmark::microbenchmark({

      # })

    }
  }

 i = tail(which(1:nrow(data$e_matrix) %% 10==1), n = 1)


  svc$put_item(
    Item = list(
      job_id = list(S = paste0(data_id,"_",i,"_",i+9)),
      data_id = list(S = data_id),
      data = list(S = jsonlite::toJSON(data$e_matrix[i:nrow(data$e_matrix),],auto_unbox = TRUE,force = TRUE))
    ),
    ReturnConsumedCapacity = "TOTAL",
    TableName = "SERDA"
  )
  time_consumed = as.numeric(Sys.time() - start)

  svc$put_item(
    Item = list(
      job_id = list(S = paste0(data_id,"_f")),
      data_id = list(S = data_id),
      data = list(S = jsonlite::toJSON(data$f,auto_unbox = TRUE,force = TRUE))
    ),
    ReturnConsumedCapacity = "TOTAL",
    TableName = "SERDA"
  )
  svc$put_item(
    Item = list(
      job_id = list(S = paste0(data_id,"_p")),
      data_id = list(S = data_id),
      data = list(S = jsonlite::toJSON(data$p,auto_unbox = TRUE,force = TRUE))
    ),
    ReturnConsumedCapacity = "TOTAL",
    TableName = "SERDA"
  )






  # get_item
  # item = svc$get_item(
  #   Key = list(
  #     job_id = list(
  #       S = "test1"
  #     )
  #   ),
  #   TableName = "SERDA"
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
  # headers=c('Content-Type'='application/json','secret-key'='$2b$10$5bk8vcqs/lgNmnBLLGmSauJJfQkfmgBkqnerEd0EaD6xiCf0BLh8u',"collection-id" = "5ee94ed3ccc9877ac37d1724")
  # # data = list("Sample" = "Hello World")
  # # data = d
  # postfields = toJSON(data$e_matrix[1,],auto_unbox = TRUE,force = TRUE)
  # # format(object.size(data), units = "MB")
  # # start = Sys.time()
  # microbenchmark::microbenchmark({
  #   a = RCurl::getURL(url, customrequest='POST', httpheader=headers,postfields=postfields)
  # })
  # result = RCurl::getURL(url, customrequest='POST', httpheader=headers,postfields=postfields)












  return(list(time_consumed=time_consumed,data_id = data_id))
}

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
  # source("https://raw.githubusercontent.com/slfan2013/rcodes/master/read_data.R")
  # data = read_data(file)

  pacman::p_load(paws.database, jsonlite)
#
#   # Put data to cloud.
  svc <- dynamodb(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = "AKIASFOZFH2CKOCHASTU",
          secret_access_key = "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"
          # ,session_token = "string"
        )
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
    #     )
    #   ),
    #   KeySchema = list(
    #     list(
    #       AttributeName = "job_id",
    #       KeyType = "HASH"
    #     )
    #   ),
    #   ProvisionedThroughput = list(
    #     ReadCapacityUnits = 500L,
    #     WriteCapacityUnits = 500L
    #   ),
    #   TableName = "SERDA"
    # )


  # put_item
  o = microbenchmark::microbenchmark({
    svc$put_item(
      Item = list(
        job_id = list(
          S = "test1"
        ),
        data_e = list(
          S = toJSON(iris)
        ),
        data_f = list(
          S = toJSON(data$f[1,])
        ),
        data_p = list(
          S = toJSON(data$p[1,])
        )
      ),
      ReturnConsumedCapacity = "TOTAL",
      TableName = "SERDA"
    )
  })


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



  # https://console.aws.amazon.com/iam/home#/users/slfan?section=security_credentials
  # T5s1VMB5lwVPDTBZbT6TPA1Zl7ArcXIglKn1SExR
  # AKIASFOZFH2CKOCHASTU
  # us-west-1
  #  https://dynamodb.us-west-1.amazonaws.com
  return(o)
}

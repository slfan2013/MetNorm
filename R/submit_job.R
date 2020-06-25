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

submit_job <- function(data_id = "1592676776", normalization_method = "serda") {
  # normalization_method = "serda"
  pacman::p_load(paws.database, jsonlite)
  data_id = as.character(data_id)

  job_id = as.integer(Sys.time())

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
  #   TableName = "SERDA_jobs"
  # )



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


  svc$put_item(
    Item = list(
      job_id = list(S = job_id),
      normalization_method = list(S = normalization_method),
      status = list(S = "waiting_to_be_normalized"),
      data_id = list(S = data_id)
    ),
    ReturnConsumedCapacity = "TOTAL",
    TableName = "SERDA_jobs"
  )






  return(list(status = "success"))
}

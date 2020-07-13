
input_file <- function(file = "P20 positive mode FULL.csv") {
  # file = "P20 positive mode FULL.csv"
  source("https://raw.githubusercontent.com/slfan2013/rcodes/master/read_data.R")
  data = read_data(file)

  pacman::p_load(paws.database, jsonlite)



  task_id = as.character(as.integer(Sys.time()))
  # Put bins to a collection.
  collection_id = "5f07e238a62f9b4b27621ce6"
  # Posting multipart
  library(curl)
  library(magrittr)
  new_bin_id_matching = list()
  cb <- function(req){rrr <<- req; new_bin_id_matching[[length(new_bin_id_matching)+1]] <<- fromJSON(rawToChar(rrr$content))[c("id","binName")];cat("done:", req$url, ": HTTP:", req$status, "\n")}

  pool <- new_pool()

  data$e_matrix = data$e_matrix[1:25,] # Will try if the system will block later once the Process went through; Check how long it blocks. Check if changing IP will prevent blocking.

  handels = list()
  for(i in 1:nrow(data$e_matrix)){
      if(i %% 10 == 1){
        if(i+9 < nrow(data$e_matrix)){
          print(i)

          handels[[length(handels)+1]] = new_handle(
            copypostfields = jsonlite::toJSON(data$e_matrix[1:(1+9),],auto_unbox = TRUE,force = TRUE)
          )%>%
            handle_setheaders(
              'Content-Type'='application/json',
              'secret-key'=get_key(),
              "name" = paste0(task_id,"-",i),
              "collection-id" = collection_id
            )%>%handle_setopt(customrequest = "POST")
        }else{
          print(i)
          handels[[length(handels)+1]] = new_handle(
            copypostfields = jsonlite::toJSON(data$e_matrix[i:nrow(data$e_matrix),],auto_unbox = TRUE,force = TRUE)
          )%>%
            handle_setheaders(
              'Content-Type'='application/json',
              'secret-key'=get_key(),
              "name" = paste0(task_id,"-",i),
              "collection-id" = collection_id
            )%>%handle_setopt(customrequest = "POST")
        }

        curl_fetch_multi('https://api.jsonbin.io/b', done = cb, pool = pool, handle  = handels[[length(handels)]])
      }
  }
  out <- multi_run(pool = pool)


  # put the job_id relation to collection id.
  if(out$error == 0){# if there is no error, put the job_id information to the collection.

    print(length(new_bin_id_matching))
    return(list(new_bin_id_matching=new_bin_id_matching,task_id = task_id))

  }else{
    stop("Error happened when uploading dataset.")
  }










}

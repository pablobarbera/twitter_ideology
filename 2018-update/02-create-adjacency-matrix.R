#==============================================================================
# 02-create-adjacency-matrix.R
# Purpose: create list of users who follow 3 or more Congress accounts, and 
# create adjacency matrix based on what political accounts they follow
# Author: Pablo Barbera
#==============================================================================

# params
folder <- 'followers-lists-201807'
tabname <- 'followers201807'

# preparing to export files to Google Cloud Storage
Sys.setenv("GCS_AUTH_FILE" = "~/Dropbox/credentials/bigquery-token.json")
library(googleCloudStorageR)
gcs_get_bucket("tweetscores")

# function to upload files to GCloudStorage
write_upload <- function(input, output){
  write.table(input, file = output, row.names=FALSE, col.names=FALSE, sep=",")
}

# reading files and uploading to GCloudStorage
outfolder <- paste0('~/Dropbox/tweetscores/', folder)
fls <- list.files(outfolder, full.names=TRUE)

# checking those not uploaded yet
done <- gcs_list_objects("tweetscores")$name
done <- gsub(".csv", ".rdata", done)
fls <- fls[fls %in% paste0("/Users/pablobarbera/Dropbox/tweetscores/", done) == FALSE]

# uploading new files
for (f in fls){
  message(f)
  message(which(fls==f), '/', length(fls))
  load(f)
  account <- gsub(".*201807/(.*).rdata", f, repl="\\1")
  df <- data.frame(
    account = account,
    id_str = followers,
    stringsAsFactors = FALSE
  )
  gcs_upload(df, bucket="tweetscores", name=paste0(folder, '/', account, '.csv'),
           predefinedAcl="bucketOwnerFullControl", object_function = write_upload)
}

# open files and upload to Google BigQuery table
library(bigrquery)
project <- "usc-barbera"
set_service_token("~/Dropbox/credentials/bigquery-token.json")

# creating dataset
if (!bq_dataset_exists("usc-barbera.tweetscores")){
  bq_dataset_create("usc-barbera.tweetscores")
}

tab <- bq_table("usc-barbera", "tweetscores", tabname)

# loading files to BigQuery table
fields <- as_bq_fields(list(
  bq_field("account", "string"),
  bq_field("id_str", "string")
))
job <- bq_perform_load(tab, source_uris=paste0("gs://tweetscores/", folder, "/*.csv"),
                fields=fields, source_format = "CSV")

# wait if not completed
while (bq_job_status(job)$state!="DONE"){
  Sys.sleep(5)
}

# uploading Congress table





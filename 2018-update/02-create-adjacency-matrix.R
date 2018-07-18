#==============================================================================
# 02-create-adjacency-matrix.R
# Purpose: create list of users who follow 3 or more Congress accounts, and 
# create adjacency matrix based on what political accounts they follow
# Author: Pablo Barbera
#==============================================================================

# params
folder <- 'followers-lists-201807'
tabname <- 'followers201807'
rstabname <- 'edgelist201807'
usermatrix <- 'adjmatrix201807'
dropbox <- "~/Dropbox (Personal)/"

######################################################################
## upload follower files to Google Cloud Storage
######################################################################

# preparing to export files to Google Cloud Storage
Sys.setenv("GCS_AUTH_FILE" = paste0(dropbox, "credentials/bigquery-token.json"))
library(googleCloudStorageR)
gcs_get_bucket("tweetscores")

# function to upload files to GCloudStorage
write_upload <- function(input, output){
  write.table(input, file = output, row.names=FALSE, col.names=FALSE, sep=",")
}

# reading files and uploading to GCloudStorage
fls <- paste0(folder, '/', list.files(outfolder))

# checking those not uploaded yet
done <- gcs_list_objects("tweetscores")$name
done <- gsub(".csv", ".rdata", done)
fls <- fls[fls %in% done == FALSE]

# uploading new files
for (f in fls){
  message(f)
  message(which(fls==f), '/', length(fls))
  load(paste0(dropbox, 'tweetscores/', f))
  account <- gsub(".*201807/(.*).rdata", f, repl="\\1")
  df <- data.frame(
    account = account,
    id_str = followers,
    stringsAsFactors = FALSE
  )
  gcs_upload(df, bucket="tweetscores", name=paste0(folder, '/', account, '.csv'),
           predefinedAcl="bucketOwnerFullControl", object_function = write_upload)
}

######################################################################
## exporting follower files to Google BigQuery
######################################################################

# open files and upload to Google BigQuery table
library(bigrquery)
project <- "usc-barbera"
set_service_token(paste0(dropbox, "credentials/bigquery-token.json"))

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
  message(bq_job_status(job)$state)
  Sys.sleep(5)
}

######################################################################
## descriptive analysis of follower counts
######################################################################

# counting number of users that meet different conditions
query_exec(
  paste0("SELECT COUNT(DISTINCT(id_str)) AS user_count
  FROM [usc-barbera:tweetscores.", tabname, "]"),
  project = project)
# 157,052,143

query_exec(paste0("
  WITH user_counts AS (
    SELECT COUNT(1) as accounts_followed
    FROM `usc-barbera.tweetscores.", tabname, "` 
     GROUP BY id_str 
  )
  SELECT '3+' AS user_group, 0 AS accounts_followed, COUNT(1) as freq
  FROM user_counts WHERE accounts_followed>=3
  UNION ALL
  SELECT '5+' AS user_group, 0 AS accounts_followed, COUNT(1) as freq
                  FROM user_counts WHERE accounts_followed>=5
  UNION ALL
  SELECT '10+' AS user_group, 0 AS accounts_followed, COUNT(1) as freq
  FROM user_counts WHERE accounts_followed>=10
  UNION ALL
  SELECT 'follows:' AS user_group, accounts_followed, COUNT(1) AS freq
  FROM user_counts GROUP by accounts_followed ORDER by accounts_followed"),
       project=project, use_legacy_sql=FALSE)

#user_group accounts_followed     freq
#1          10+                 0  7635171
#2           3+                 0 48746802
#3           5+                 0 25122269
#4     follows:                 1 75764070
#5     follows:                 2 26167563
#6     follows:                 3 14724199

######################################################################
## taking random sample for estimation
######################################################################

# uploading Congress table
congress <- readr::read_csv(paste0(dropbox, "tweetscores/accounts-twitter-data-2018-07.csv"),
                            col_types = "ccccciiiccccccccccc")
tab <- bq_table("usc-barbera", "tweetscores", "polaccounts")
bq_table_upload(tab, congress)

# checking that JOIN will work
query_exec(paste0(
  "SELECT COUNT(DISTINCT(screen_name)) FROM `usc-barbera.tweetscores.polaccounts` x
  JOIN `usc-barbera.tweetscores.", tabname, "` y
  ON LOWER(x.screen_name) = LOWER(y.account)
  WHERE x.type = 'Congress'"), project=project, use_legacy_sql=FALSE) #516

query_exec(
  "SELECT COUNT(DISTINCT(screen_name)) FROM `usc-barbera.tweetscores.polaccounts`
  WHERE type='Congress'",
  project=project, use_legacy_sql=FALSE
) #516

# selecting users who follow 10+ accounts total and 3+ Members of Congress
df <- query_exec(paste0(
  "WITH congress_counts AS (
    SELECT x.id_str, COUNT(1) as congress_followed
    FROM `usc-barbera.tweetscores.", tabname, "` x 
    JOIN `usc-barbera.tweetscores.polaccounts` y
    ON LOWER(x.account) = LOWER(y.screen_name)
    WHERE y.type='Congress'
    GROUP BY x.id_str
    HAVING COUNT(1)>=3
  ),
  all_counts AS (
    SELECT id_str, COUNT(1) as accounts_followed
    FROM `usc-barbera.tweetscores.", tabname, "` 
    GROUP BY id_str
    HAVING COUNT(1)>=10
  ),
  user_sample AS (
    SELECT x.id_str
    FROM congress_counts x
    JOIN all_counts y
    ON x.id_str = y.id_str
    ORDER BY RAND()
    LIMIT 500000
  )
    SELECT id_str, account
    FROM `usc-barbera.tweetscores.", tabname, "` 
    WHERE id_str IN (SELECT id_str FROM user_sample)"), 
  project=project, use_legacy_sql=FALSE, 
  destination_table=list(project_id=project, dataset_id='tweetscores', table_id=rstabname))

# saving to Google Cloud Storage
tab <- bq_table("usc-barbera", "tweetscores", rstabname)
gcsname <- paste0("gs://tweetscores/", rstabname, ".csv")
bq_table_save(tab, destination_uris=gcsname)
# downloading from Google Cloud Storage
tmp <- tempfile()
gcs_get_object(object_name=gcsname, bucket="tweetscores",
           saveToDisk=tmp)

######################################################################
## creating matrix
######################################################################

edges <- ndjson::stream_in(tmp)
users <- unique(edges$id_str)
n <- length(users)
accounts <- unique(edges$account)
m <- length(accounts)

rows <- list()
columns <- list()
pb <- txtProgressBar(min=1, max=m, style=3)
for (j in 1:m){
    followers <- edges$id_str[edges$account==accounts[j]]
    to_add <- which(users %in% followers)
    rows[[j]] <- to_add
    columns[[j]] <- rep(j, length(to_add))
    setTxtProgressBar(pb, j)
}

rows <- unlist(rows)
columns <- unlist(columns)

library(Matrix)
y <- sparseMatrix(i=rows, j=columns)
rownames(y) <- users[1:nrow(y)]
colnames(y) <- accounts

save(y, file=paste0(dropbox, 'tweetscores/', usermatrix, '.rdata'))





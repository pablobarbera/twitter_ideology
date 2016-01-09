#==============================================================================
# 02-create-adjacency-matrix.R
# Purpose: create list of users who follow 3 or more political accounts, and 
# create adjacency matrix based on what political accounts they follow
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
options(stringsAsFactors=F)
setwd("~/git/twitter_ideology/2016-election")
outfolder <- 'data/followers-lists'
oauth_folder <- '~/Dropbox/credentials/twitter'
userfile <- 'data/userlist-US.rdata'
matrixfile <- 'data/adj-matrix-US.rdata'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## creating user list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

users <- read.csv("data/accounts-twitter-data.csv")
accounts <- users$screen_name[users$followers_count>1000]
fls <- paste0(outfolder, "/", accounts, ".rdata")
fls <- fls[tolower(fls) %in% tolower(list.files(outfolder, full.names=TRUE))]

followers.list <- list(NULL)
for (i in 1:length(fls)){
	load(fls[i])
	followers.list[[i]] <- followers
	cat(i, "of", length(fls), "\n")
}

all <- unlist(followers.list)
library(data.table)
dt <- data.table(all)
utab <- dt[, .N , by = all]
#utab <- table(all) # aggregating at user level

# counting number of unique users
cat(nrow(utab))
# 71,497,051

# keeping list of users who follow 3+ accounts
userlist <- utab$all[utab$N>=3]
cat(length(userlist)) # 20,584,164
save(userlist, file=userfile)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## creating matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load(userfile)
n <- length(userlist) # users in dataset
users <- read.csv("data/accounts-twitter-data.csv")
accounts <- users$screen_name[users$followers_count>1000]
fls <- paste0(outfolder, "/", accounts, ".rdata")
fls <- fls[tolower(fls) %in% tolower(list.files(outfolder, full.names=TRUE))]
m <- length(accounts) # political accounts
rows <- list()
columns <- list()

pb <- txtProgressBar(min=1,max=m, style=3)
for (j in 1:m){
    load(fls[j])
    to_add <- which(userlist %in% followers)
    rows[[j]] <- to_add
    columns[[j]] <- rep(j, length(to_add))
    setTxtProgressBar(pb, j)
}

rows <- unlist(rows)
columns <- unlist(columns)

# preparing sparse Matrix
library(Matrix)
y <- sparseMatrix(i=rows, j=columns)
rownames(y) <- userlist
colnames(y) <- accounts

save(y, file=matrixfile)



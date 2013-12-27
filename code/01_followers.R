#==============================================================================
# 01_followers.R
# Purpose: download list of Twitter followers of politicians
# Details: follower lists are stored in the "data/followers_lists" folder
# as .Rdata files
# Author: Pablo Barbera
#==============================================================================

setwd("~/Dropbox/git/twitter_ideology")
source("code/functions.R")

# loading list of twitter accounts for members of US Congress
user.data <- read.csv(
    "data/congress_social_media_data.csv", 
    stringsAsFactors=F)

# removing MoCs without Twitter account
accounts <- user.data$congress_account[user.data$congress_account_id!=""]

# removing those that we already did (downloaded to "data/followers_lists/")
accounts.done <- list.files("followers_lists")
accounts.left <- accounts[accounts %in% gsub(".rdata", "", accounts.done) == FALSE]
accounts.left <- accounts.left[!is.na(accounts.left)]


# loop over each account
while (length(accounts.left) > 0){
    # sample randomly one account to get followers
    new.user <- sample(accounts.left, 1)
    cat(new.user, " -- ", length(accounts.left), " accounts left!\n")   
    # download followers (with some exception handling...) 
    error <- tryCatch(followers <- getFollowers(screen_name=new.user,
        oauth_folder="~/Dropbox/credentials"), error=function(e) e)
    if (inherits(error, 'error')) {
        cat("Error! On to the next one...")
        next
    }
    # save to file and remove from lists of "accounts.left"
    file.name <- paste0("followers_lists/", new.user, ".rdata")
    save(followers, file=file.name)
    accounts.left <- accounts.left[-which(accounts.left %in% new.user)]
}





#==============================================================================
# 02-get-followers.R
# Purpose: download list of Twitter followers of politicians from Twitter API
# Details: follower lists are stored in 'outfolder' as .Rdata files
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
dropbox <- "~/Dropbox/"
outfolder <- 'followers-lists-202008/'
polsfile <- "accounts-twitter-data-2020-08.csv"
oauth_folder <- paste0(dropbox, 'credentials/twitter')

# reading list of accounts
users <- read.csv(paste0(dropbox, "data/tweetscores/", polsfile),
                  stringsAsFactors = FALSE)
accounts <- users$screen_name

# first check if there's any list of followers already downloaded to 'outfolder'
accounts.done <- gsub(".rdata", "", 
                      list.files(paste0(dropbox, 'data/tweetscores/', outfolder)))
accounts.left <- accounts[tolower(accounts) %in% tolower(accounts.done) == FALSE]
accounts.left <- accounts.left[!is.na(accounts.left)]

# excluding accounts with 10MM+ followers for now:
length(accounts.left)
accounts.left <- accounts.left[tolower(accounts.left) %in% 
           tolower(users$screen_name[users$followers_count>=10000000]) == FALSE]
length(accounts.left)

# loop over the rest of accounts, downloading follower lists from API
while (length(accounts.left) > 0){

    # sample randomly one account to get followers
    new.user <- sample(accounts.left, 1)
    #new.user <- accounts.left[1]
    cat(new.user, "---", users$followers_count[users$screen_name==new.user], 
        " followers --- ", length(accounts.left), " accounts left!\n")    
    
    # download followers (with some exception handling...) 
    error <- tryCatch(followers <- getFollowers(screen_name=new.user,
        oauth=oauth_folder, sleep=3, verbose=FALSE), error=function(e) e)
    if (inherits(error, 'error')) {
        cat("Error! On to the next one...")
        next
    }
    
    # save to file and remove from lists of "accounts.left"
    file.name <- paste0(dropbox, 'data/tweetscores/', 
                        outfolder, new.user, ".rdata")
    save(followers, file=file.name)
    accounts.left <- accounts.left[-which(accounts.left %in% new.user)]

}

# and now the rest...
accounts.left <- users$screen_name[users$followers_count>=10000000]

# loop over the rest of accounts, downloading follower lists from API
while (length(accounts.left) > 0){

    # sample randomly one account to get followers
    new.user <- sample(accounts.left, 1)
    #new.user <- accounts.left[1]
    cat(new.user, "---", users$followers_count[users$screen_name==new.user], 
        " followers --- ", length(accounts.left), " accounts left!\n")    
    outfile <- paste0(dropbox, 'data/tweetscores/', 
                      outfolder, new.user, '.txt')
    
    # download followers (with some exception handling...) 
    error <- tryCatch(getFollowers(screen_name=new.user,
        oauth=oauth_folder, cursor='-1', 
        sleep=3, verbose=FALSE,
        file=outfile), error=function(e) e)
    if (inherits(error, 'error')) {
        cat("Error! On to the next one...")
        next
    }
    
    # read from file and then save to .rdata;
    # also remove from lists of "accounts.left"
    followers <- unique(scan(outfile, what="character"))
    file.name <- paste0(dropbox, 'data/tweetscores/', 
                        outfolder, new.user, ".rdata")
    save(followers, file=file.name)
    accounts.left <- accounts.left[-which(accounts.left %in% new.user)]

}

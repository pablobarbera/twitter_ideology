#==============================================================================
# 02_preparing_user_list.R
# Purpose: compile followers lists and extract list of users who follow 3 or
# more political accounts.
# Runtime: depending on number of politicians, ~1 hour
# Author: Pablo Barbera
#==============================================================================

setwd("~/Dropbox/git/twitter_ideology")
source("code/functions.R")

filesList <- list.files("followers_lists", full.names=TRUE)
followers_m <- list(NULL)
for (i in 1:length(filesList)){
    load(filesList[i])
    followers_m[[i]] <- followers
    cat(i, "of", length(filesList), "\n")
}

all_users_list <- unlist(followers_m)
cat(length(unique(all_users_list)))

## how many users follow at least 1 MC?
## 5,377,629

userlistfollowers <- table(all_users_list)
options(scipen=30)
names(userlistfollowers) <- as.numeric(names(userlistfollowers))
userlistfollowers <- userlistfollowers[userlistfollowers>=3]

## how many users follow 3 or more political accounts?
## 813,203

userlist <- names(userlistfollowers)
save(userlist, file="temp/userlist.Rdata")

## in paper, subset also users who tweeted at least 3 times about Obama/Romney






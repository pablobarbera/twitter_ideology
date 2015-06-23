#==============================================================================
# 02-get-users-data.R
# Purpose: create list of users who follow 3 or more political accounts, and
# then download their data from Twitter API to apply spam and geography filter
# Author: Pablo Barbera
#==============================================================================

source('functions.R')

## change the following two lines to run this Rscript for other countries
outfolder <- 'temp/US/followers_lists/'
userfile <- 'output/userlist-US.rdata'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## creating user list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# opening all follower lists and merging them into a single list

fls <- list.files(outfolder, full.names=TRUE)
followers.list <- list(NULL)
for (i in 1:length(filesList)){
	load(fls[i])
	followers.list[[i]] <- followers
	cat(i, "of", length(fls), "\n")
}

all_users_list <- unlist(followers.list)
cat(length(unique(all_users_list))) # counting number of unique users
userlistfollowers <- table(all_users_list) # aggregating at user level

# keeping list of users who follow 3+ politicians
userlistfollowers <- userlistfollowers[userlistfollowers>=3]

# U.S. only: subsetting only those who published at least three
# tweet mentioning Obama or Romney in the 2012 campaign
load("data/tweet-counts.rdata")
users.tweets$total <- users.tweets$obama + users.tweets$romney
userlisttweets <- users.tweets$id[users.tweets$total>=3]

userlist <- intersect(names(userlisttweets), names(userlistfollowers))
save(userlist, file=userfile)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## download user information from Twitter API
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load(userfile)

# Note: there are multiple ways of capturing and storing user data. Here
# I use a Mongo database to store and query the data to make the process
# slightly more efficient, but the data could be saved in plain .csv files.

# Connecting to MongoDB in local machine
library(rmongodb)
mongo <- mongo.create(host='127.0.0.1', username='', 
    password='', db='users')
ns <- 'users.US'

# getting list of IDs already downloaded (if any)
done.ids <- getDoneIDs(mongo, ns)

# list of IDs to download now
left.ids <- userlist[userlist %in% done.ids == FALSE]

# loop over users
while (length(left.ids)>0){

    # take random sample of 100 users
    new.users <- sample(left.ids, 100)  
    # get user information from Twitter API
    error <- tryCatch(j <- getUsers(id=new.users, 
    	oauth_folder="~/credentials/twitter"),
            error=function(e) e)
    if (inherits(error, 'error')) {
        cat("Error! On to the next one...")
        Sys.sleep(1)
        next
    }

    # insert into MongoDB
    for (i in 1:length(j)){ 

        cat("\n", i, " ", j[[i]][['screen_name']])
        # indexing by id / adding date of last tweet
        user <- prepareForMongo(j[[i]])
        # adding users data to MongoDB
        mongo.insert(mongo=mongo, ns=ns, user)

    }

    # removing done IDs from list
    left.ids <- left.ids[-which(left.ids %in% new.users)]

    # information on console
    cat("\n ", length(left.ids), " accounts left!\n")

    rm(j)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## applying simple spam classifier
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## selecting only users that meet the following conditions:
min_followers = 25
min_lasttweet = as.numeric(as.POSIXct("2012-04-01 00:00:00"))
min_statuses = 100

users <- getSubsetIDs(mongo, ns, min_followers=min_followers,
	min_lasttweet=min_lasttweet, min_statuses=min_statuses)

## subsetting
load(userfile)
userlist <- userlist[uselist %in% users$`_id`]
save(userlist, file=userfile)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## collecting user location information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## downloading location information to subset only users in the US
## (in the paper I used Yahoo Geo API, but that's not available anymore.
## Instead, I recommend using http://www.datasciencetoolkit.org/
## The code below assumes that DSTK is installed locally, running
## as a virtual machine


for (i in 1:length(users$location)){

	user_id <- users$`_id`[i]
    cat("User", i, "of", length(users$location), ":", user_id, "\n")

    if (users$location==""){ next }

    location <- users$location[i]
    cat(location, "\n")

    geo <- getGeo(location, rdstk = "localhost:8080")
    if (length(geo)==0 | is.null(geo$state)){
        cat("No state location found!!\n")
        next
    }

    cat(geo$lat, " ", geo$lng, " ", geo$state, "\n")

    df <- data.frame(
    	id = user_id, location=location, lat=geo$lat,
    	lng = geo$lng, country=geo$country, 
    	constituency=ifelse(!is.null(geo$constituency), geo$constituency, NA),
    	state = geo$state, 
    	county=ifelse(!is.null(geo$county), geo$country, NA),
    	city=ifelse(!is.null(geo$city), geo$city, NA),
    	stringsAsFactors=F)
	write.table(df, file="users-geo.csv", append=TRUE,
    	row.names=FALSE, col.names=FALSE, sep=",")

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## applying geography classifier
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df <- read.csv("users-geo.csv", stringsAsFactors=F)
users_in_us <- df$id[df$country=="United States",]

userlist <- userlist[userlist %in% users_in_us]

save(userlist, file=userfile)







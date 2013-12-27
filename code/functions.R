#==============================================================================
# functions.R
# Purpose: additional functions programmed for paper
# Author: Pablo Barbera
#==============================================================================

#==============================================================================
# getFollowers
#
# Description: 
# \code{getFollowers} connects to the REST API of Twitter and returns the
# list of followers of a given user. Note that this function allows the
# use of multiple OAuth token to make the process more efficient.
#
# Arguments
# screen_name: user name of the Twitter user for which their followers
# will be downloaded
#
# oauth_folder: folder where OAuth tokens are stored. These tokens need to
# be created using the tutorial on https://github.com/pablobarbera/workshop
# and saved as an object named 'my_oauth'
#
# cursor: See \url{https://dev.twitter.com/docs/api/1.1/get/followers/ids}
#
# Example:
# ## Download list of followers of user "p_barbera"
#  followers <- getFollowers(screen_name="p_barbera", oauth_folder="oauth")
#
#==============================================================================

getFollowers <- function(screen_name, oauth_folder, cursor=-1){

    require(rjson); require(ROAuth)

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    cat(cr, "\n")
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitFollowers(my_oauth)
    cat(limit, " API calls left\n")
    while (limit==0){
        cr <- sample(creds, 1)
        cat(cr, "\n")
        load(cr)
        Sys.sleep(1)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitFollowers(my_oauth)
        cat(limit, " API calls left\n")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/followers/ids.json"
    ## empty list for followers
    followers <- c()
    ## while there's more data to download...
    while (cursor!=0){
        ## making API call
        params <- list(screen_name = screen_name, cursor = cursor)
        url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
        Sys.sleep(1)
        ## one API call less
        limit <- limit - 1
        ## trying to parse JSON data
        json.data <- fromJSON(url.data, unexpected.escape = "skip")
        if (length(json.data$error)!=0){
            cat(url.data)
            stop("error! Last cursor: ", cursor)
        }
        ## adding new IDS
        followers <- c(followers, as.character(json.data$ids))

        ## previous cursor
        prev_cursor <- json.data$previous_cursor_str
        ## next cursor
        cursor <- json.data$next_cursor_str
        ## giving info
        cat(length(followers), "followers. Next cursor: ", cursor, "\n")

        ## changing oauth token if we hit the limit
        cat(limit, " API calls left\n")
        while (limit==0){
            cr <- sample(creds, 1)
            cat(cr, "\n")
            load(cr)
            Sys.sleep(1)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitFollowers(my_oauth)
            cat(limit, " API calls left\n")
        }
    }
    return(followers)
}

#==============================================================================
# getLimitRate
# getLimitFollowers
#
# Description: functions used in getFollowers to monitor API rate limits
#==============================================================================


getLimitRate <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "followers,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}

getLimitFollowers <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "followers,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))
}


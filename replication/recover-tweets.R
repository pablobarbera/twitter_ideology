#==============================================================================
# recover-tweets.R
# Purpose: function to recover tweets from tweet IDS
# Author: Pablo Barbera
#==============================================================================

#==============================================================================
## FUNCTIONS (and see below for examples)
#==============================================================================

#' @rdname getStatuses
#' @export
#'
#' @title 
#' Downloads tweets by their ID from REST API and saves to a json file
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param ids list of tweet IDs to be downloaded
#'
#' @param file Name of file where json tweets will be stored
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#'

getStatuses <- function(ids=NULL, filename, oauth_folder, verbose=TRUE, sleep=1){

    require(rjson); require(ROAuth)

    ## create list of credentials
    creds <- list.files(oauth_folder, full.names=T)
    ## open a random credential
    cr <- sample(creds, 1)
    if (verbose) {cat(cr, "\n")}
    load(cr)
    ## while rate limit is 0, open a new one
    limit <- getLimitStatuses(my_oauth)
    if (verbose) {cat(limit, " API calls left\n")}
    while (limit==0){
        cr <- sample(creds, 1)
        if (verbose){cat(cr, "\n")}
        load(cr)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitStatuses(my_oauth)
        if (verbose){cat(limit, " API calls left\n")}
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/statuses/lookup.json"
    ids.left <- ids

    ## while there's more data to download...
    while (length(ids.left)>0){
        ## making API call
        params <- list(id = paste(ids.left[1:100], collapse=","))
        url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1
        
        # parsing JSON
        json.data <- RJSONIO::fromJSON(url.data)
        if (length(json.data$error)!=0){
            cat(url.data)
            stop("error downloading IDs! First ID not downloaded", ids[1])
        }

        ## writing to disk
        conn <- file(filename, "a")
        invisible(lapply(json.data, function(x) writeLines(rjson::toJSON(x), con=conn)))
        close(conn)
    
        # removing IDs done
        ids.left <- ids.left[-(1:100)]

        ## changing oauth token if we hit the limit
        if (verbose){cat(limit, " API calls left\n")}
        while (limit==0){
            cr <- sample(creds, 1)
            if (verbose){cat(cr, "\n")}
            load(cr)
            Sys.sleep(sleep)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitStatuses(my_oauth)
            if (verbose){cat(limit, " API calls left\n")}
        }
    }
}

getLimitRate <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "followers,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}



getLimitStatuses <- function(my_oauth){
    require(rjson); require(ROAuth)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "statuses,application")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$statuses$`/statuses/lookup`[['remaining']]))

}


#==============================================================================
## EXAMPLES 
#==============================================================================

## reading first 100 tweets
ids <- scan("data/romney-tweets.txt", n=100, what="character")

## downloading statuses
getStatuses(ids=ids, filename='romney-tweets.json',
    oauth_folder = "~/credentials/twitter")

## reading tweets in R
library(streamR)
tweets <- parseTweets("romney-tweets.json")










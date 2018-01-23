#' @rdname getStatuses
#' @export
#'
#' @title
#' Downloads tweets by their ID from REST API and saves to a json file
#'
#' @author
#' Pablo Barbera \email{pbarbera@@usc.edu}
#'
#' @param ids list of tweet IDs to be downloaded
#'
#' @param filename Name of file where json tweets will be stored
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#'

getStatuses <- function(ids=NULL, filename, oauth, verbose=TRUE, sleep=1){

    ## loading credentials
    my_oauth <- getOAuth(oauth, verbose=verbose)

    ## while rate limit is 0, open a new one
    limit <- getLimitStatuses(my_oauth)
    if (verbose) {message(limit, " API calls left\n")}
    while (limit==0){
        my_oauth <- getOAuth(oauth, verbose=verbose)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitStatuses(my_oauth)
        if (verbose){message(limit, " API calls left\n")}
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/statuses/lookup.json"
    ids.left <- ids

    # preparing OAuth token for httr
    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
        secret = my_oauth$consumerSecret)
    credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
    twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE),
        app = app, credentials = credentials)

    ## while there's more data to download...
    while (length(ids.left)>0){
        ## making API call
        query <- list(id = paste(ids.left[1:100], collapse=","))
        url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1

        # parsing JSON
        json.data <- httr::content(url.data)
        if (length(json.data$error)!=0){
            message(url.data)
            stop("error downloading IDs! First ID not downloaded", ids[1])
        }

        ## writing to disk
        conn <- file(filename, "a")
        invisible(lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn, useBytes=TRUE)))
        close(conn)

        # removing IDs done
        ids.left <- ids.left[-(1:100)]

        ## changing oauth token if we hit the limit
        if (verbose){message(limit, " API calls left\n")}
        cr_old <- cr
        while (limit==0){
            my_oauth <- getOAuth(oauth, verbose=verbose)
            Sys.sleep(sleep)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitStatuses(my_oauth)
            if (verbose){message(limit, " API calls left\n")}
        }
        if (cr != cr_old) {
            app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
                secret = my_oauth$consumerSecret)
            credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
            twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE),
                app = app, credentials = credentials)
        }
    }
}

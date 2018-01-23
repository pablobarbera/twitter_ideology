#' @rdname searchTweets
#' @export
#'
#' @title
#' Search recent tweets
#'
#' @description
#' \code{searchTweets} connects to the REST API of Twitter and returns
#' recent tweets (usually no more than 5-7 days old) based on a supplied
#' search string. Note that this function will not return all tweets. As
#' explained in the documentation (\url{https://dev.twitter.com/rest/public/search}),
#' the goal of this API endpoint is relevance and not completeness.
#'
#' @author
#' Pablo Barbera \email{P.Barbera@@lse.ac.uk}
#'
#' @param q search query to issue to Twitter. It can contain Boolean operators.

#' @param filename file where tweets will be stored (in json format)
#'
#' @param n maximum number of tweets to be downloaded
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param until limit for the most recent tweet. The function will then
#' returns tweets created before this date. Date should be formatted as
#' YYYY-MM-DD. Keep in mind that the search index has a 7-day limit
#'
#' @param since_id id of the oldest tweet to be downloaded. Useful if, for
#' example, we're only interested in getting tweets sent after a certain
#' date.
#'
#' @param result_type specifies the type of results you would prefer to
#' receive: "recent" (default), "popular" or "mixed".
#'
#' @param lang restricts tweets to the given language (e.g. "es")
#'
#' @param sleep numeric, number of seconds between API calls. Higher number
#' will increase reliability of API calls; lower number will increase speed.
#'
#' @param verbose If TRUE, provides additional output in console about API
#' rate limits
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## Download recent tweets by user "p_barbera"
#'  searchTweets(q="twitter", filename="twitter-tweets.json",
#'    n=200, oauth=my_oauth)
#' }
#'

searchTweets <- function(q, filename, n=200, oauth="~/credentials",
    until=NULL, since_id=NULL, result_type="recent", lang=NULL, sleep=.5, verbose=FALSE){

    ## loading credentials
    my_oauth <- getOAuth(oauth, verbose=verbose)

    ## while rate limit is 0, open a new one
    limit <- getLimitSearch(my_oauth)
    if (verbose) message(limit, " hits left")
    while (limit==0){
        my_oauth <- getOAuth(oauth, verbose=verbose)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitSearch(my_oauth)
        if (verbose) message(limit, " hits left")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/search/tweets.json"

    ## first API call
    params <- list(q=q, count=200, result_type=result_type)
    if (!is.null(since_id)){
        params[["since_id"]] <- since_id
    }
    if (!is.null(until)){
        params[["until"]] <- until
    }
    if (!is.null(lang)){
        params[["lang"]] <- lang
    }
    query <- lapply(params, function(x) URLencode(as.character(x)))

    # preparing OAuth token for httr
    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
        secret = my_oauth$consumerSecret)
    credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
    twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE),
        app = app, credentials = credentials)

    # first query
    url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## changing oauth token if we hit the limit
    if (verbose) message(limit, " hits left")
    while (limit==0){
        my_oauth <- getOAuth(oauth, verbose=verbose)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitSearch(my_oauth)
        if (verbose) message(limit, " hits left")
    }
    ## trying to parse JSON data
    json.data <- httr::content(url.data)
    if (length(json.data$error)!=0){
        message(url.data)
        stop("error! Last cursor: ", cursor)
    }
    ## writing to disk
    conn <- file(filename, "a")
    ret <- lapply(json.data[[1]], function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
    close(conn)
    ## max_id
    tweets <- length(json.data[[1]])
    max_id <- json.data[[2]]$max_id
    message(tweets, " tweets. Max id: ", max_id)

    while (tweets < n && !is.null(json.data[[2]]$next_results)){

        next_url <- paste0(url, json.data[[2]]$next_results)
        url.data <- httr::GET(next_url, httr::config(token = twitter_token))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1
        ## changing oauth token if we hit the limit
        message(limit, " hits left")
        while (limit==0){
            my_oauth <- getOAuth(oauth, verbose=verbose)
            Sys.sleep(sleep)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitSearch(my_oauth)
            message(limit, " hits left")
            # preparing OAuth token for httr
            options("httr_oauth_cache"=FALSE)
            app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
            secret = my_oauth$consumerSecret)
            credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
                twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE),
                app = app, credentials = credentials)
        }
        ## trying to parse JSON data
        ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
        json.data <- httr::content(url.data)
        if (length(json.data$error)!=0){
            message(url.data)
            stop("error! Last cursor: ", cursor)
        }
        ## writing to disk
        conn <- file(filename, "a")
        ret <- lapply(json.data[[1]], function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
        close(conn)
        ## max_id
        tweets <- tweets + length(json.data[[1]])
        max_id <- json.data[[2]]$max_id
        message(tweets, " tweets. Max id: ", max_id)

    }
}


getLimitSearch <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "search")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$search$`/search/tweets`[['remaining']]))

}







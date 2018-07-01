#' @rdname getTimeline
#' @export
#'
#' @title
#' Returns up to 3,200 recent tweets from a given user
#'
#' @description
#' \code{getTimeline} connects to the REST API of Twitter and returns up to
#' 3,200 recent tweets sent by these user. If the total number of tweets sent
#' by this user is less than 3,200 tweets, it will return all tweets.
#'
#' @author
#' Pablo Barbera \email{P.Barbera@@lse.ac.uk}
#'
#' @param filename file where tweets will be stored (in json format)
#'
#' @param n number of tweets to be downloaded (maximum is 3,200)
#'
#' @param screen_name user name of the Twitter user for which his/her tweets
#' will be downloaded
#'
#' @param id id of Twitter user for which his/her tweets will be downloaded
#' (Use either of these two arguments)
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param since_id id of the oldest tweet to be downloaded. Useful if, for
#' example, we're only interested in getting tweets sent after a certain
#' date.
#'
#' @param trim_user if "true", downloaded tweets will include user object
#' embedded. If "false", only tweet information will be downloaded.
#'
#' @param tweet_mode if "extended", will return up to 280 characters per tweet.
#'
#' @param sleep numeric, number of seconds between API calls. Higher number
#' will increase reliability of API calls; lower number will increase speed.
#'
#' @param verbose If TRUE, provides additional output in console about API
#' rate limits
#'
#' @examples \dontrun{
#' ## Download recent tweets by user "p_barbera"
#'  friends <- getTimeline(screen_name="p_barbera", oauth=my_oauth)
#' }
#'

getTimeline <- function(filename, n=3200, oauth, screen_name=NULL,
    id=NULL, since_id=NULL, trim_user="false", tweet_mode='extended',
    sleep=.5, verbose=FALSE){

    ## loading credentials
    my_oauth <- getOAuth(oauth, verbose=verbose)
    ## while rate limit is 0, open a new one
    limit <- getLimitTimeline(my_oauth)
    if (verbose) message(limit, " hits left")
    while (limit==0){
        my_oauth <- getOAuth(oauth, verbose=verbose)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitTimeline(my_oauth)
        if (verbose) message(limit, " hits left")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"

    ## first API call
    if (!is.null(screen_name)){
        params <- list(screen_name = screen_name, count=200, trim_user=trim_user,
                       tweet_mode=tweet_mode)
    }
    if (!is.null(id)){
        params <- list(id=id, count=200, trim_user=trim_user,
                       tweet_mode=tweet_mode)
    }
    if (!is.null(since_id)){
        params[["since_id"]] <- since_id
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
    cr_old <- my_oauth
    while (limit==0){
        my_oauth <- getOAuth(oauth, verbose=verbose)
        Sys.sleep(sleep)
        # sleep for 5 minutes if limit rate is less than 100
        rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitTimeline(my_oauth)
        if (verbose) message(limit, " hits left")
    }
    if (!all.equal(my_oauth, cr_old)) {
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
    ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn, useBytes=TRUE))
    close(conn)
    ## max_id
    tweets <- length(json.data)
    max_id <- json.data[[tweets]]$id_str
    message(tweets, " tweets. Max id: ", max_id)
    max_id_old <- "none"
    if (is.null(since_id)) {since_id <- 1}

    while (tweets < n & max_id != max_id_old &
        as.numeric(max_id) > as.numeric(since_id)){
        max_id_old <- max_id
        if (!is.null(screen_name)){
            params <- list(screen_name = screen_name, count=200, max_id=max_id,
                trim_user=trim_user, tweet_mode=tweet_mode)
        }
        if (!is.null(id)){
            params <- list(id=id, count=200, max_id=max_id, trim_user=trim_user,
                           tweet_mode=tweet_mode)
        }
        if (!is.null(since_id) && since_id != 1 ){
           params[['since_id']] <- since_id
        }
        query <- lapply(params, function(x) URLencode(as.character(x)))
        url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
        Sys.sleep(sleep)
        ## one API call less
        limit <- limit - 1
        ## changing oauth token if we hit the limit
        message(limit, " hits left")
        cr_old <- my_oauth
        while (limit==0){
            my_oauth <- getOAuth(oauth, verbose=verbose)
            Sys.sleep(sleep)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitTimeline(my_oauth)
            message(limit, " hits left")
        }
        if (all.equal(my_oauth, cr_old)) {
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
        ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn, useBytes=TRUE))
        close(conn)
        ## max_id
        tweets <- tweets + length(json.data)
        max_id <- json.data[[length(json.data)]]$id_str
        message(tweets, " tweets. Max id: ", max_id)
    }
}


#' @rdname searchUsers
#' @export
#'
#' @title
#' Returns list of users related to a search query
#'
#' @description
#' \code{searchUsers} Provides a simple, relevance-based search interface
#' to public user accounts on Twitter. Try querying by topical interest,
#' full name, company name, location, or other criteria. Exact match searches
#' are not supported. Only the first 1,000 matches are available.
#'
#' @author
#' Pablo Barbera \email{P.Barbera@@lse.ac.uk}
#'
#' @param q The search query to run against people search
#'
#' @param count Number of potential user results to retrieve
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param verbose shows additional ouput about token usage in console
#'
#'
#' @examples \dontrun{
#' ## Search users using query "pablo"
#'  userdata <- searchUsers(q="pablo", count=200, oauth=my_oauth)
#' }
#'

searchUsers <- function(q=NULL, count=100, oauth, verbose=TRUE){

    ## load credentials
    my_oauth <- getOAuth(oauth, verbose=verbose)

    ## url to call
    url <- "https://api.twitter.com/1.1/users/search.json"

    ## first API call
    params <- list(q=q, count=ifelse(count<20, count, 20), page=1)

    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
        secret = my_oauth$consumerSecret)
    credentials <- list(oauth_token = my_oauth$oauthKey, oauth_token_secret = my_oauth$oauthSecret)
    twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE),
        app = app, credentials = credentials)
    query <- lapply(params, function(x) URLencode(as.character(x)))
    url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
    json.data <- new.json <- httr::content(url.data)
    n <- length(json.data)

    ## rest of API calls
    while (n<count & length(new.json)>0){
      params <- list(q=q, count=20, page=params$page+1)
      query <- lapply(params, function(x) URLencode(as.character(x)))
      url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
      new.json <- httr::content(url.data)
      json.data <- c(json.data, new.json)
      n <- length(json.data)
    }

    users.df <- data.frame(
      id_str = unlistWithNA(json.data, 'id_str'),
      screen_name = unlistWithNA(json.data, 'screen_name'),
      name = unlistWithNA(json.data, 'name'),
      description = unlistWithNA(json.data, 'description'),
      followers_count = unlistWithNA(json.data, 'followers_count'),
      statuses_count = unlistWithNA(json.data, 'statuses_count'),
      friends_count = unlistWithNA(json.data, 'friends_count'),
      created_at = unlistWithNA(json.data, 'created_at'),
      location = unlistWithNA(json.data, 'location'),
      lang = unlistWithNA(json.data, 'lang'),
      time_zone = unlistWithNA(json.data, 'time_zone'),
      status.id_str = unlistWithNA(json.data, c('status', 'id_str')),
      status.created_at = unlistWithNA(json.data, c('status', 'created_at')),
      status.text = unlistWithNA(json.data, c('status', 'text')),
      stringsAsFactors=F)

  return(users.df)
}



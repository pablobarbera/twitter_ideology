#' @rdname getFriends
#' @export
#'
#' @title
#' Returns the list of user IDs a given Twitter user follows
#'
#' @description
#' \code{getFriends} connects to the REST API of Twitter and returns the
#' list of user IDs a given user follows. Note that this function allows the
#' use of multiple OAuth token to make the process more efficient.
#'
#' @author
#' Pablo Barbera \email{P.Barbera@@lse.ac.uk}
#'
#' @param screen_name user name of the Twitter user for which their friends
#' will be downloaded
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/friends/ids}
#'
#' @param user_id user id of the Twitter user for which their friends will be
#' downloaded
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## Download list of friends of user "p_barbera"
#'  friends <- getFriends(screen_name="p_barbera", oauth=my_oauth)
#' }
#'

getFriends <- function(screen_name=NULL, oauth, cursor=-1, user_id=NULL, verbose=TRUE, sleep=1){

  ## loading credentials
  my_oauth <- getOAuth(oauth, verbose=verbose)

  ## while rate limit is 0, open a new one
  limit <- getLimitFriends(my_oauth)
  if (verbose){message(limit, " API calls left")}
  while (limit==0){
    my_oauth <- getOAuth(oauth, verbose=verbose)
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitFriends(my_oauth)
    if (verbose){message(limit, " API calls left")}
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/friends/ids.json"
  ## empty list for friends
  friends <- c()
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, cursor = cursor, stringify_ids="true")
    }
    if (!is.null(user_id)){
      params <- list(user_id = user_id, cursor = cursor, stringify_ids="true")
    }
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## trying to parse JSON data
    json.data <- jsonlite::fromJSON(url.data)
    if (length(json.data$error)!=0){
      if (verbose){message(url.data)}
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    friends <- c(friends, as.character(json.data$ids))

    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    ## giving info
    message(length(friends), " friends. Next cursor: ", cursor)

    ## changing oauth token if we hit the limit
    if (verbose){message(limit, " API calls left")}
    while (limit==0){
      my_oauth <- getOAuth(oauth, verbose=verbose)
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 50
      rate.limit <- getLimitRate(my_oauth)
      if (rate.limit<50){
        Sys.sleep(300)
      }
      limit <- getLimitFriends(my_oauth)
      if (verbose){message(limit, " API calls left")}
    }
  }
  return(friends)
}


#' @rdname getFollowers
#' @export
#'
#' @title
#' Returns the list of user IDs that correspond to a given user's follower
#'
#' @description
#' \code{getFollowers} connects to the REST API of Twitter and returns the
#' list of followers of a given user. Note that this function allows the
#' use of multiple OAuth token to make the process more efficient.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_name user name of the Twitter user for which their followers
#' will be downloaded
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/followers/ids}
#'
#' @param user_id user id of the Twitter user for which their friends will be
#' downloaded
#'
#' @param verbose If \code{TRUE}, prints information about API calls on console
#'
#' @param sleep Number of seconds to sleep between API calls.
#'
#' @examples \dontrun{
#' ## Download list of followers of user "p_barbera"
#'  followers <- getFollowers(screen_name="p_barbera", oauth_folder="oauth")
#' }
#'

getFollowers <- function(screen_name=NULL, oauth_folder, cursor=-1, user_id=NULL, verbose=TRUE, sleep=1){

  ## create list of credentials
  creds <- list.files(oauth_folder, full.names=T)
  ## open a random credential
  cr <- sample(creds, 1)
  if (verbose) {cat(cr, "\n")}
  load(cr)
  ## while rate limit is 0, open a new one
  limit <- getLimitFollowers(my_oauth)
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
    limit <- getLimitFollowers(my_oauth)
    if (verbose){cat(limit, " API calls left\n")}
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/followers/ids.json"
  ## empty list for followers
  followers <- c()
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, cursor = cursor)
    }
    if (!is.null(user_id)){
      params <- list(user_id = user_id, cursor = cursor)
    }
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## trying to parse JSON data
    json.data <- jsonlite::fromJSON(url.data)
    if (length(json.data$error)!=0){
      if(verbose){cat(url.data)}
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
      limit <- getLimitFollowers(my_oauth)
      if (verbose){cat(limit, " API calls left\n")}
    }
  }
  return(followers)
}

getLimitFollowers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))
}

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
#' Pablo Barbera \email{P.Barbera@@lse.ac.uk}
#'
#' @param screen_name user name of the Twitter user for which their followers
#' will be downloaded
#'
#' @param oauth_folder folder where OAuth tokens are stored. It can also be the name
#' of a file that contains the token, or a csv file with the format: consumer_key,
#' consumer_secret,access_token,access_token_secret.
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
#' @param file If not \code{NULL}, will store followers list in file instead of in
#' memory (useful for users with many followers in computer with low RAM memory).
#'
#' @examples \dontrun{
#' ## Download list of followers of user "p_barbera"
#'  followers <- getFollowers(screen_name="p_barbera", oauth_folder="oauth")
#' }
#'

getFollowers <- function(screen_name=NULL, oauth_folder, cursor=-1, user_id=NULL, verbose=TRUE, sleep=1, file=NULL){

  ## loading credentials
  my_oauth <- getOAuth(oauth_folder, verbose=verbose)

  ## while rate limit is 0, open a new one
  limit <- getLimitFollowers(my_oauth)
  if (verbose) {message(limit, " API calls left")}
  while (limit==0){
    my_oauth <- getOAuth(oauth_folder, verbose=verbose)
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitFollowers(my_oauth)
    if (verbose){message(limit, " API calls left")}
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/followers/ids.json"
  ## empty list for followers
  if (is.null(file)) followers <- c()
  if (!is.null(file)){
    con <- file(file, "a")
    count <- 0
  }
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
      if(verbose){message(url.data)}
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    if (is.null(file)) followers <- c(followers, as.character(json.data$ids))
    if (!is.null(file)){
      followers <- as.character(json.data$ids)
      writeLines(followers, con=con)
    }

    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    ## giving info
    if (is.null(file)) message(length(followers), " followers. Next cursor: ", cursor)
    if (!is.null(file)){
      count <- count + length(followers)
      message(count, " followers. Next cursor: ", cursor)
    }

    ## changing oauth token if we hit the limit
    if (verbose){message(limit, " API calls left")}
    while (limit==0){
      my_oauth <- getOAuth(oauth_folder, verbose=verbose)
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(my_oauth)
      if (rate.limit<100){
        Sys.sleep(300)
      }
      limit <- getLimitFollowers(my_oauth)
      if (verbose){message(limit, " API calls left")}
    }
  }
  if (is.null(file)) return(followers)
  if (!is.null(file)) close(con)
}



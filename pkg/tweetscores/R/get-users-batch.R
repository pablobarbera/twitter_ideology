#' @rdname getUsersBatch
#' @export
#'
#' @title
#' Returns user data for a vector of Twitter user IDs or screen_names
#'
#' @description
#' \code{getUsersBatch} connects to the REST API of Twitter and returns user
#' objects (user information) for Twitter users, based on their
#' screen names or user IDs
#'
#' @param screen_names user names of the Twitter users
#'
#' @param ids ids of Twitter users
#'
#' @param include_entities if "true", returned data will include most
#' recent tweet
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param verbose shows additional ouput about token usage in console
#'
#' @param output If not \code{NULL}, will write user data in raw JSON format
#' to that file
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## Download user data for users "p_barbera" and "barackobama"
#'  userdata <- getUsersBatch(screen_names=c("p_barbera", "BarackObama"),
#'    oauth=my_oauth)
#' }
#'

getUsersBatch <- function(ids=NULL, screen_names=NULL, oauth, include_entities="false",
                          verbose=TRUE, output=NULL){

  left.ids <- if (is.null(ids)) {screen_names} else {ids}
  if (!is.null(output)){ conn = file(output, 'w')}
  users.df <- list()
  i <- 1
  while (length(left.ids)>0){
    message(i, "--", length(left.ids), ' users left')
    ids.tmp <- sample(left.ids, min(c(100, length(left.ids))))

    if (!is.null(ids)){
      error <- tryCatch(tmp <- getUsers( oauth, ids = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    if (!is.null(screen_names)){
      error <- tryCatch(tmp <- getUsers( oauth,
                                         screen_names = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    # if error is found, go to next loop iteration
    if (inherits(error, 'error')){ next }

    if (!is.null(output)){ out <- lapply(tmp, function(x) writeLines(jsonlite::toJSON(x), con=conn)) }

    users.df[[i]] <- data.frame(
      id_str = unlist(lapply(tmp, '[[', 'id_str')),
      screen_name = unlist(lapply(tmp, '[[', 'screen_name')),
      name = unlist(lapply(tmp, '[[', 'name')),
      description = unlist(lapply(tmp, '[[', 'description')),
      followers_count = unlist(lapply(tmp, '[[', 'followers_count')),
      statuses_count = unlist(lapply(tmp, '[[', 'statuses_count')),
      friends_count = unlist(lapply(tmp, '[[', 'friends_count')),
      created_at = unlist(lapply(tmp, '[[', 'created_at')),
      location = unlist(lapply(tmp, '[[', 'location')),
      stringsAsFactors=F)

    i <- i + 1
    left.ids <- left.ids[left.ids %in% ids.tmp == FALSE]
  }
  users.df <- do.call(rbind, users.df)
  if (!is.null(output)){ close(conn) }
  return(users.df)
}

#' @rdname getUsers
#' @export
#'
#' @title
#' Returns user data for a vector of up to 100 Twitter user IDs or screen_names
#'
#' @description
#' \code{getUsers} connects to the REST API of Twitter and returns user
#' objects (user information) for Twitter users, based on their
#' screen names or user IDs, for up to 100 users
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_names user names of the Twitter users
#'
#' @param ids ids of Twitter users
#'
#' @param include_entities if "true", returned data will include most
#' recent tweet
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param verbose shows additional ouput about token usage in console
#'
#' @examples \dontrun{
#' ## Download user data for users "p_barbera" and "barackobama"
#'  userdata <- getUsers(screen_names=c("p_barbera", "BarackObama"),
#'    oauth="~/Dropbox/credentials")
#' }

getUsers <- function(oauth="~/credentials", screen_names=NULL,
                     ids=NULL, include_entities="true", verbose=FALSE){

  ## loading credentials
  my_oauth <- getOAuth(oauth, verbose=verbose)

  ## while rate limit is 0, open a new one
  limit <- getLimitUsers(my_oauth)
  if (verbose) message(limit, " hits left")
  while (limit==0){
    my_oauth <- getOAuth(oauth, verbose=verbose)
    Sys.sleep(1)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitUsers(my_oauth)
    if (verbose) message(limit, " hits left")
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/users/lookup.json"

  ## first API call
  if (!is.null(screen_names)){
    screen_names <- paste(screen_names, collapse=",")
    params <- list(screen_name = screen_names, include_entities=include_entities)
  }
  if (!is.null(ids)){
    ids <- paste(ids, collapse=",")
    params <- list(user_id=ids, include_entities=include_entities)
  }

  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data)
}




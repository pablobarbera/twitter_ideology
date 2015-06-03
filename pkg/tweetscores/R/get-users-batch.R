#' @rdname getUsersBatch
#' @export
#'
#' @title
#' Returns user data for a vector of Twitter user IDs or screen_names
#'
#' @description
#' \code{getUsers} connects to the REST API of Twitter and returns user
#' objects (user information) for Twitter users, based on their
#' screen names or user IDs
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param screen_names user names of the Twitter users
#'
#' @param id ids of Twitter users
#'
#' @param include_entities if "true", returned data will include most
#' recent tweet
#'
#' @param oauth_folder folder where OAuth tokens are stored.
#'
#' @param verbose shows additional ouput about token usage in console
#'
#' @param output If not \code{NULL}, will write user data in raw JSON format
#' to that file
#'
#'
#' @examples \dontrun{
#' ## Download user data for users "p_barbera" and "barackobama"
#'  userdata <- getUsersBatch(screen_names=c("p_barbera", "BarackObama"), oauth_folder="~/Dropbox/credentials")
#' }
#'

getUsersBatch <- function(ids=NULL, screen_names=NULL, oauth_folder, include_entities="false",
                          verbose=TRUE, output=NULL){

  left.ids <- if (is.null(ids)) {screen_names} else {ids}
  if (!is.null(output)){ conn = file(output, 'w')}
  users.df <- list()
  i <- 1
  while (length(left.ids)>0){
    cat(i, "--", length(left.ids), 'users left\n')
    ids.tmp <- sample(left.ids, min(c(100, length(left.ids))))

    if (!is.null(ids)){
      error <- tryCatch(tmp <- getUsers( oauth_folder, id = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    if (!is.null(screen_names)){
      error <- tryCatch(tmp <- getUsers( oauth_folder,
                                         screen_names = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    # if error is found, go to next loop iteration
    if (inherits(error, 'error')){ next }

    if (!is.null(output)){ out <- lapply(tmp, function(x) writeLines(toJSON(x), con=conn)) }

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


getUsers <- function(oauth_folder="~/credentials", screen_names=NULL,
                     id=NULL, include_entities="true", verbose=FALSE){

  ## create list of credentials
  creds <- list.files(oauth_folder, full.names=T)
  ## open a random credential
  cr <- sample(creds, 1)
  if (verbose) cat(cr, "\n")
  load(cr)
  ## while rate limit is 0, open a new one
  limit <- getLimitUsers(my_oauth)
  if (verbose) cat(limit, " hits left\n")
  while (limit==0){
    cr <- sample(creds, 1)
    if (verbose) cat(cr, "\n")
    load(cr)
    Sys.sleep(1)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitUsers(my_oauth)
    if (verbose) cat(limit, " hits left\n")
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/users/lookup.json"

  ## first API call
  if (!is.null(screen_names)){
    screen_names <- paste(screen_names, collapse=",")
    params <- list(screen_name = screen_names, include_entities=include_entities)
  }
  if (!is.null(id)){
    ids <- paste(id, collapse=",")
    params <- list(user_id=ids, include_entities=include_entities)
  }

  options("httr_oauth_cache"=FALSE)
  app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
                         secret = my_oauth$consumerSecret)
  sig <- httr::sign_oauth1.0(app, token=my_oauth$oauthKey,
                             token_secret=my_oauth$oauthSecret)
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, config(token=sig[["token"]]))
  json.data <- httr::content(url.data)
  return(json.data)
}


getLimitUsers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "users,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(rjson::fromJSON(response)$resources$users$`/users/lookup`[['remaining']]))

}

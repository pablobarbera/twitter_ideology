#' @rdname getList
#' @export
#'
#' @title
#' Returns the list of users added to a Twitter list
#'
#' @description
#' \code{getList} connects to the REST API of Twitter and returns a
#' data frame with information about users included in a Twitter list
#'
#' @param list_id ID of the list
#'
#' @param oauth One of the following: either a list with details for an access token
#' (see example below), a folder where OAuth tokens are stored, or a csv file
#' with the format: consumer_key, consumer_secret, access_token, access_token_secret.
#'
#' @param cursor See \url{https://dev.twitter.com/docs/api/1.1/get/lists/members}
#'
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## Download Twitter list of U.S. Governors created by @@cspan
#'  accts <- getList(list_id="7560205", oauth=my_oauth)
#' }
#'

getList <- function(list_id, oauth, cursor=-1){

  ## loading credentials
  my_oauth <- getOAuth(oauth, verbose=verbose)

  ## while rate limit is 0, open a new one
    limit <- getLimitList(my_oauth)
    message(limit, " API calls left\n")
    while (limit==0){
      my_oauth <- getOAuth(oauth, verbose=verbose)
      Sys.sleep(1)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(my_oauth)
        if (rate.limit<100){
            Sys.sleep(300)
        }
        limit <- getLimitList(my_oauth)
        message(limit, " API calls left\n")
    }
    ## url to call
    url <- "https://api.twitter.com/1.1/lists/members.json"
    ## empty list for members
    members <- list()
    ## while there's more data to download...
    while (cursor!=0){
        ## making API call
        params <- list(list_id=list_id,
            include_entities='true', cursor=cursor, skip_status='true')
        query <- lapply(params, function(x) URLencode(as.character(x)))
        url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
        Sys.sleep(1)
        ## one API call less
        limit <- limit - 1
        ## trying to parse JSON data
        json.data <- httr::content(url.data)
        if (length(json.data$error)!=0){
            message(url.data)
            stop("error! Last cursor: ", cursor)
        }
        ## transforming to DF and storing in list
        members[[length(members)+1]] <- userDataToDF(json.data$users)

        ## previous cursor
        prev_cursor <- json.data$previous_cursor_str
        ## next cursor
        cursor <- json.data$next_cursor_str
        ## giving info
        message(sum(unlist(lapply(members, nrow))),
            " users in list. Next cursor: ", cursor, "\n")

        ## changing oauth token if we hit the limit
        message(limit, " API calls left\n")
        while (limit==0){
            my_oauth <- getOAuth(oauth, verbose=verbose)
            Sys.sleep(1)
            # sleep for 5 minutes if limit rate is less than 100
            rate.limit <- getLimitRate(my_oauth)
            if (rate.limit<100){
                Sys.sleep(300)
            }
            limit <- getLimitList(my_oauth)
            message(limit, " API calls left\n")
        }
    }
    members <- do.call(rbind, members)
    return(members)
}

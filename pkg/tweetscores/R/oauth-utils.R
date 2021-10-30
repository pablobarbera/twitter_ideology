getOAuth <- function(x, verbose=TRUE){
	# list of credentials
  if (class(x)[1]=="list"){
    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = x$consumer_key,
                           secret = x$consumer_secret)
    credentials <- list(oauth_token = x$access_token,
                        oauth_token_secret = x$access_token_secret)
    my_oauth <- httr::Token1.0$new(endpoint = httr::oauth_endpoints("twitter"),
                                        params = list(as_header = TRUE),
                                        app = app, credentials = credentials)
  }
  # tokens created with ROAuth
  if (class(x)[1]=="OAuth"){
    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = x$consumerKey,
                           secret = x$consumerSecret)
    credentials <- list(oauth_token = x$oauth_token,
                        oauth_token_secret = x$oauth_token_secret)
    my_oauth <- httr::Token1.0$new(endpoint = httr::oauth_endpoints("twitter"),
                                   params = list(as_header = TRUE),
                                   app = app, credentials = credentials)
  }
  # tokens created with httr
  if (class(x)[1]=="Token1.0"){ my_oauth <- x }

  # first check if x exists in disk
	if (class(x)[1] %in% c("list", "OAuth", "Token1.0") == FALSE && file.exists(x)){
		info <- file.info(x)
		# if it's a folder, load one and return
		if (info$isdir){
			creds <- list.files(x, full.names=TRUE)
			cr <- sample(creds, 1)
			if (verbose){message(cr)}
  			load(cr)
  			my_oauth <- getOAuth(my_oauth)
		}
		# if not, check type
		if (!info$isdir){
			# if it's not csv, guess it's Rdata and load it
			if (!grepl("csv", x)){
				if (verbose){message(x)}
				load(x)
				my_oauth <- getOAuth(my_oauth)
			}
			# if it's a csv file read it, and create token
			if (grepl("csv", x)){
				d <- read.csv(x, stringsAsFactors=F)
				creds <- d[sample(1:nrow(d),1),]
				options("httr_oauth_cache"=FALSE)
				app <- httr::oauth_app("twitter", key = creds$consumer_key,
				                       secret = creds$consumer_secret)
				credentials <- list(oauth_token = creds$access_token,
				                    oauth_token_secret = creds$access_token_secret)
				my_oauth <- httr::Token1.0$new(endpoint = httr::oauth_endpoints("twitter"),
				                               params = list(as_header = TRUE),
				                               app = app, credentials = credentials)
				# testing that it works
				#url = "https://api.twitter.com/1.1/users/show.json"
				#params = list(screen_name = "twitter")
				#my_oauth$OAuthRequest(URL=url, params=params, method="GET",
         #                     cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
			}

		}
	}

	return(my_oauth)
}



getLimitFriends <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "friends,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$friends$`/friends/ids`$remaining)
}

getLimitRate <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$application$`/application/rate_limit_status`$remaining)
}

getLimitFollowers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$followers$`/followers/ids`$remaining)
}

getLimitUsers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "users,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$users$`/users/lookup`$remaining)

}

getLimitSearch <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "search")
    query <- lapply(params, function(x) URLencode(as.character(x)))
    url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
    json.data <- httr::content(url.data)
    return(json.data$resources$search$`/search/tweets`$remaining)
}

getLimitList <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "lists,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$lists$`/lists/members`$remaining)
}

getLimitRetweets <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "statuses,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$statuses$`/statuses/retweeters/ids`$remaining)
}

getLimitStatuses <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "statuses,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$statuses$`/statuses/lookup`$remaining)
}

getLimitTimeline <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "statuses,application")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=my_oauth))
  json.data <- httr::content(url.data)
  return(json.data$resources$statuses$`/statuses/user_timeline`$remaining)
}

getOAuth <- function(x, verbose=TRUE){
	# first check if x is an object
  if (class(x)=="list"){
    my_oauth <- ROAuth::OAuthFactory$new(consumerKey=x$consumer_key,
                                         consumerSecret=x$consumer_secret,
                                         oauthKey=x$access_token,
                                         oauthSecret=x$access_token_secret,
                                         needsVerifier=FALSE,
                                         handshakeComplete=TRUE,
                                         verifier="1",
                                         requestURL="https://api.twitter.com/oauth/request_token",
                                         authURL="https://api.twitter.com/oauth/authorize",
                                         accessURL="https://api.twitter.com/oauth/access_token",
                                         signMethod="HMAC")
  }

  # first check if x exists in disk
	if (class(x)!="list" && file.exists(x)){
		info <- file.info(x)
		# if it's a folder, load one and return
		if (info$isdir){
			creds <- list.files(x, full.names=TRUE)
			cr <- sample(creds, 1)
			if (verbose){message(cr)}
  			load(cr)
		}
		# if not, check type
		if (!info$isdir){
			# if it's not csv, guess it's Rdata and load it
			if (!grepl("csv", x)){
				if (verbose){message(x)}
				load(x)
			}
			# if it's a csv file read it, and create token
			if (grepl("csv", x)){
				d <- read.csv(x, stringsAsFactors=F)
				creds <- d[sample(1:nrow(d),1),]
				my_oauth <- ROAuth::OAuthFactory$new(consumerKey=creds$consumer_key,
												consumerSecret=creds$consumer_secret,
												oauthKey=creds$access_token,
												oauthSecret=creds$access_token_secret,
												needsVerifier=FALSE,
												handshakeComplete=TRUE,
												verifier="1",
												requestURL="https://api.twitter.com/oauth/request_token",
												authURL="https://api.twitter.com/oauth/authorize",
												accessURL="https://api.twitter.com/oauth/access_token",
												signMethod="HMAC")
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
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$friends$`/friends/ids`['remaining']))
}

getLimitRate <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}

getLimitFollowers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))
}

getLimitUsers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "users,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$users$`/users/lookup`[['remaining']]))

}

getLimitSearch <- function(my_oauth){
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    params <- list(resources = "search")
    response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
        cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    return(unlist(rjson::fromJSON(response)$resources$search$`/search/tweets`[['remaining']]))

}

getLimitList <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "lists,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(rjson::fromJSON(response)$resources$lists$`/lists/members`['remaining']))
}

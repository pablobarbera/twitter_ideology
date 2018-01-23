#' @rdname scrapeCongressData
#' @export
#'
#' @title
#' Scrape the social mediaaccounts of Members of US Congress
#'
#' @description
#' \code{scrapeCongressData} downloads the list of social media accounts
#' (Twitter and Facebook) for the Members of the US Congress from the
#' unitedstates GitHub account at any point in time, and returns a data
#' frame.
#'
#' @param commit Commit from which data will be pulled
#'
#' @examples \dontrun{
#'  congress <- scrapeCongressData()
#' }
#'

scrapeCongressData <- function(commit="master"){
  ## Downloading Congress data
  txt <- httr::content(httr::GET(paste0("https://raw.githubusercontent.com/unitedstates/",
                            "congress-legislators/", commit, "/legislators-current.yaml")), 'text')
  congress <- yaml::yaml.load(txt)
  congress <- data.frame(
    id = unlistCongress(congress, c('id', 'thomas')),
    bioid = unlistCongress(congress, c('id', 'bioguide')),
    name = unlistCongress(congress, c('name', 'official_full')),
    gender = unlistCongress(congress, c('bio', 'gender')),
    type = unlist(lapply(congress, function(x)
      x$terms[[length(x$terms)]]$type)),
    party = unlist(lapply(congress, function(x)
      x$terms[[length(x$terms)]]$party)),
    stringsAsFactors=F)
  ## Downloading List of Social Media Accounts
  txt <- httr::content(httr::GET(paste0("https://raw.githubusercontent.com/unitedstates/",
                            "congress-legislators/", commit, "/legislators-social-media.yaml")), 'text')
  sm <- yaml::yaml.load(txt)
  sm <- data.frame(
    bioid = unlistCongress(sm, c('id', 'bioguide')),
    twitter = unlistCongress(sm, c('social', 'twitter')),
    facebook = unlistCongress(sm, c('social', 'facebook')),
    youtube = unlistCongress(sm, c('social', 'youtube')),
    stringsAsFactors=F)
  ## merging
  df <- merge(congress, sm, all.x=TRUE)
  return(df)
}


unlistCongress <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, '[[', field))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  return(vect)
}

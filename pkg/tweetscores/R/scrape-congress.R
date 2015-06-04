#' @rdname scrapeCongressData
#' @export
#'
#' @title
#' Scrape the list of Twitter accounts for Members of US Congress from
#  unitedstates GitHub account
#'
#'
#' @examples \dontrun{
#'  congress <- scrapeCongressData()
#' }
#'

scrapeCongressData <- function(){
  ## Downloading Congress data
  txt <- content(httr::GET(paste0("https://raw.githubusercontent.com/unitedstates/",
                            "congress-legislators/master/legislators-current.yaml")), 'text')
  congress <- yaml::yaml.load(txt)
  congress <- data.frame(
    id = unlistCongress(congress, c('id', 'bioguide')),
    name = unlistCongress(congress, c('name', 'official_full')),
    gender = unlistCongress(congress, c('bio', 'gender')),
    type = unlist(lapply(congress, function(x)
      x$terms[[length(x$terms)]]$type)),
    party = unlist(lapply(congress, function(x)
      x$terms[[length(x$terms)]]$party)),
    stringsAsFactors=F)
  ## Downloading List of Social Media Accounts
  txt <- content(httr::GET(paste0("https://raw.githubusercontent.com/unitedstates/",
                            "congress-legislators/master/legislators-social-media.yaml")), 'text')
  sm <- yaml::yaml.load(txt)
  sm <- data.frame(
    id = unlistCongress(sm, c('id', 'thomas')),
    twitter = unlistCongress(sm, c('social', 'twitter')),
    stringsAsFactors=F)
  ## merging
  df <- merge(congress, sm)
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

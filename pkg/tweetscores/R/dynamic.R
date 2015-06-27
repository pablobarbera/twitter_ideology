#' @rdname getCreated
#' @export
#'
#' @title
#' Returns approximate date in which an account was created based on its
#' user ID
#'
#' @description
#' \code{getCreated} estimates the date in which an account was created
#' using a dataset that matches user IDs to dates.
#'
#'
#' @param id Twitter user ID
#'
#'
getCreated <- function(id){
  created <- tweetscores::dict$created_at[tail(which(id > tweetscores::dict$id_str),n=1)]
  if (length(created)==0 & id<2998851){ created = as.Date("2007-01-01") }
  return(created)
}


#' @rdname estimateDateBreaks
#' @export
#'
#' @title
#' Computes dates in which a sample of followers created
#' their accounts. This function is then used to estimate
#' what that list of followers looked like in the past.
#'
#' @param followers List of followers of account
#' @param seed Random seed
#'

estimateDateBreaks <- function(followers, seed=777){

  # estimate user IDs for either 10,000 or number of followers (if smaller)
  n <- min(c(10000, length(followers)))
  set.seed(seed)
  # take random sample of size n and sort it
  breaks <- sort(sample(1:length(followers), n, replace=FALSE))
  # for that random sample of users, estimate creation dates
  dates_sample <- unlist(sapply(as.numeric(followers[breaks]), getCreated))

  return(list(breaks=breaks, dates_sample=as.Date(dates_sample)))
}

#' @rdname estimatePastFollowers
#' @export
#'
#' @title
#' Estimates the follower list of a given user at a point in time
#' in the past
#'
#' @param followers List of followers
#' @param breaks user IDs that indicate potential date breaks
#' @param dates_sample dates in which the user IDs specified in
#' \code{breaks} were created
#' @param date specific date at which follower list will be estimated
#' @param verbose outputs additional information on the console
#'
#'
estimatePastFollowers <- function(followers, breaks, dates_sample, date, verbose=TRUE){

  # print date
  if (verbose) message(as.character(date), " -- ")

  # find relevant break in followers list: oldest follower created at `date`
  thr <- breaks[tail(which(dates_sample > date), n=1)]
  # sample followers until that point
  sbs <- followers[thr:length(followers)]
  if (verbose) message(length(sbs), ' followers')

  return(sbs)

}



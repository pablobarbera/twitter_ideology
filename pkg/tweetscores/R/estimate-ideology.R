#' @rdname estimateIdeology
#' @export
#'
#' @title
#' Estimates ideology for a given Twitter user
#'
#' @description
#' \code{estimateIdeology} estimates ideology for a given user using the
#' Metropolis algorithm developed in Barbera, 2013. It takes as argument
#' of the function a list of user IDs indicating who a given user follows.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param user screen name of user for which ideology is to be estimated.
#'
#' @param friends vector of user IDs that the user for which ideology wants
#' to be estimated follows. If missing, \code{\link{getFriends}} is called for 
#' the value of \code{user}.
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output
#' to the R console with information about progress of the sampler.
#'
#' @param method "MCMC" (default) for the Metropolis algorithm described in the paper.
#' "MLE" will use simple maximum likelihood estimation to compute the point estimate.
#' Note that the standard error for the ML method is likely to be wrong.
#'
#' @param iters number of iterations of the metropolis algorithm. Default is 5000
#'
#' @param n.warmup warmup period for the sampler. Default is 1000 iterations.
#'
#' @param thin thinning of the sampler. Default is 20.
#'
#' @param ... other options to be passed to the estimation functions
#'
#' @return The function returns a matrix with summary statistics of the posterior
#' distribution of the two estimated parameters, beta (political interest) and
#' theta (ideology).
#'
#' @examples \dontrun{
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth_folder="oauth")
#' ## estimating ideology
#'  results <- estimateIdeology(friends)
#'  results['theta', 'mean']
#' ## estimating ideology using ML (fast) method
#'  results <- estimateIdeology(friends, method="ML")
#'  results['theta', 'mean']
#' }
#'

estimateIdeology <- function(user, friends, verbose=TRUE, method="MCMC",
                              iters=5000, n.warmup=1000, thin=20, ...){
  if(missing(friends))
    friends <- getFriends(user)
  # getting row of adjacency matrix
  y <- tweetscores::posterior_samples$id %in% friends
  # info message
  if (sum(y)==0){
    stop("User follows 0 elites!")
  }
  message(user, " follows ", sum(y), " elites: ",
      paste(tweetscores::posterior_samples$screen_name[
          tweetscores::posterior_samples$id %in% friends ], collapse=", "))
  # estimation
  if (method=="MCMC"){
    results <- metropolis.logit(y, iters=iters, n.warmup=n.warmup,
                                thin=thin, verbose=verbose, ...)
  }
  if (method=="MLE"){
    results <- ml.logit(y, iters=iters, n.warmup=n.warmup,
                                thin=thin, verbose=verbose, ...)
  }
  results$user <- user
  attr(results, "class") <- "twideology"
  return(results)
}

#' @rdname estimateIdeology2
#' @export
#'
#' @title
#' Estimates ideology for a given Twitter user
#'
#' @description
#' \code{estimateIdeology2} estimates ideology for a given user using the
#' method described in Barbera et al, 2015, Psychological Science. It
#' projects each user onto a latent ideological space using correspondence
#' analysis, assuming that all else is constant, and relying on the column
#' estimates in the paper. The values are then normalized to a N(0,1) distribution.
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
#' @param exact logical, default is \code{FALSE}, which adds some random noise
#' (from a normal distribution with mean 0 and standard deviation 0.05) to avoid
#' spikes in distribution of ideal points. See
#' \url{https://github.com/pablobarbera/echo_chambers/blob/master/02_estimation/11-second-stage.r}
#' for details.
#'
#' @return The function returns a matrix with summary statistics of the posterior
#' distribution of the two estimated parameters, beta (political interest) and
#' theta (ideology).
#'
#' @examples \dontrun{
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth_folder="oauth")
#' ## estimating ideology
#'  results <- estimateIdeology2(friends)
#' }
#'

estimateIdeology2 <- function(user, friends, verbose=TRUE, exact=FALSE){
  if(missing(friends))
    friends <- getFriends(user)
  # getting row of adjacency matrix
  y <- matrix((tweetscores::refdataCA$id %in% friends)*1, nrow=1)
  # info message
  if (sum(y)==0){
    stop("User follows 0 elites!")
  }
  message(user, " follows ", sum(y), " elites: ",
      paste(tweetscores::refdataCA$colname[
        tweetscores::refdataCA$id %in% friends], collapse=", "))
  # estimation
  values <- supplementaryRows(tweetscores::refdataCA, y)
  # normalizing
  theta <- tweetscores::refdataCA$qs$theta[which.min(abs(values[1] - (tweetscores::refdataCA$qs$value)))]
  # adding random noise 
  # see https://github.com/pablobarbera/echo_chambers/blob/master/02_estimation/11-second-stage.r
  if (!exact) theta <- theta + rnorm(1, 0, 0.05)

  return(theta)
}

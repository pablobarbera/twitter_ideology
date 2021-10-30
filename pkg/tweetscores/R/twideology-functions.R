#' @rdname plot.twideology
#' @export
#'
#' @import ggplot2
#'
#' @title
#' Displays estimated ideology with other reference ideal points
#'
#' @param x object of class 'twideology'
#'
#' @param ... ignored
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth=my_oauth)
#' ## estimating ideology
#'  results <- estimateIdeology(user, friends)
#' ## trace plot
#'  tracePlot(results, "theta")
#' ## ideology plot
#'  plot(results)
#' }
#'

plot.twideology <- function(x, ...){
  # loading reference data
  data <- tweetscores::refdata
  # computing credible interval for user
  theta.lo <- quantile(x$samples[,,2], .025)
  theta <- mean(x$samples[,,2])
  theta.hi <- quantile(x$samples[,,2], .975)
  data <- rbind(data, c(paste0("@",x$user), theta, theta.lo, theta.hi))
  data$phi <- as.numeric(data$phi)
  data$phi.lo <- as.numeric(data$phi.lo)
  data$phi.hi <- as.numeric(data$phi.hi)
  # preparing plot
  p <- ggplot(data, aes(y=reorder(screenName, -phi), x=phi))
  pq <- p + geom_point(size=1.25) +
    geom_segment(width=.5, aes(x=phi.lo, xend=phi.hi, y=reorder(screenName, -phi),
                               yend=reorder(screenName, -phi)), position=position_dodge(.5)) +
    theme_bw() + scale_y_discrete("") +
    scale_x_continuous(expression(paste("95% Intervals for ", phi[j],
                                        " or ", theta[i], ", Estimated Ideological Ideal Points")),
                       limits=range(data[,2:4]))
  suppressMessages(suppressWarnings(print(pq)))
}


#' @rdname summary.twideology
#' @export
#'
#' @title
#' Displays summary of estimated ideology
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param object object of class 'twideology'
#'
#' @param ... ignored
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth=my_oauth)
#' ## estimating ideology
#'  results <- estimateIdeology(user, friends)
#' ## summary
#'  summary(results)
#' ## trace plot
#'  tracePlot(results, "theta")
#' ## ideology plot
#'  plot(results)
#' }
#'

summary.twideology <- function(object, ...){
  print(round(R2WinBUGS::monitor(object$samples), 2))
}

#' @rdname tracePlot
#' @export
#'
#' @title
#' Displays trace plots of MCMC chains
#'
#' @param results object of class 'twideology'
#'
#' @param par parameter for which trace plot is to be displayed
#'
#' @examples \dontrun{
#' ## Creating OAuth token
#'  my_oauth <- list(consumer_key = "CONSUMER_KEY",
#'    consumer_secret = "CONSUMER_SECRET",
#'    access_token="ACCESS_TOKEN",
#'    access_token_secret = "ACCESS_TOKEN_SECRET")
#' ## download list of friends for a given user
#'  friends <- getFriends(screen_name = "p_barbera", oauth=my_oauth)
#' ## estimating ideology
#'  results <- estimateIdeology(user, friends)
#' ## summary
#'  summary(results)
#' ## trace plot
#'  tracePlot(results, "theta")
#' ## ideology plot
#'  plot(results)
#' }
#'

tracePlot <- function(results, par="theta"){
  iters <- dim(results$samples)[[1]]
  chains <- dim(results$samples)[[3]]
  par(mar=c(3, 3, 2, 3))
  plot(1:iters, results$samples[,1,par], type="l", col="red",
       ylim=range(results$samples[,,par]))
  mtext("Iteration", side=1, line=2)
  mtext(par, side=2, line=2)
  if (chains==2) lines(1:iters, results$samples[,2,par], col="blue")
  if (chains==3) lines(1:iters, results$samples[,3,par], col="green")
  if (chains==4) lines(1:iters, results$samples[,4,par], col="black")
}


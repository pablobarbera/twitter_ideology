#' Tools for the estimation of ideology scores with Twitter data
#'
#' This package provides a series of functions
#' to collect and analyze Twitter data, and to estimate the
#" ideological positions of Twitter users with the method described
#' in Barbera (2015, Political Analysis).
#'
#' @seealso \code{\link{estimateIdeology}}, \code{\link{getFriends}},
#' @name tweetscores-package
#' @aliases tweetscores
#' @docType package
#' @author Pablo Barbera \email{pbarbera@@usc.edu}
NULL

#' @import httr
#' @import jsonlite
#' @import yaml
#' @import R2WinBUGS
#' @import ggplot2
#' @importFrom graphics lines mtext plot
#' @importFrom stats dnorm optim plogis quantile reorder rnorm runif
#' @importFrom utils URLencode read.csv setTxtProgressBar tail txtProgressBar
NULL

#' Posterior samples of ideology estimates for elites
#'
#' List that contains posterior samples of ideology and popularity estimates
#' for elites (politicians, media outlets, and journalists)
#'
#' @docType data
#' @keywords datasets
#' @name posterior_samples
#' @usage data(posterior_samples)
NULL

#' Summary ideology estimates for elites
#'
#' Data frame that contains ideal points for elites
#'
#' @docType data
#' @keywords datasets
#' @name refdata
#' @usage data(refdata)
NULL

#' Column coordinates from correspondence analysis
#'
#' List that contains output from correspondence analysis model in
#' Barbera et al, 2015, Psychological Science
#'
#' @docType data
#' @keywords datasets
#' @name refdataCA
#' @usage data(refdataCA)
NULL

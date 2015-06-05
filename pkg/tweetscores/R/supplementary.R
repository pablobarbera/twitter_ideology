#' @rdname supplementaryRows
#' @export
#'
#' @title
#' Projects additional rows (users) to a latent ideological space
#' using correspondence analysis
#'
#' @description
#' \code{supplementaryRows} takes additional rows of a follower matrix
#' and projects them to the latent ideological space using the parameters
#' of an already-fitted correspondence analysis model.
#' Code was adapted from the \code{ca} function in the \code{ca} package
#'
#' @author
#' Michael Greenacre, Oleg Nenadic, Michael Friendly (Modified by Pablo Barbera)
#'
#' @param res Output from \code{CA} function
#' @param points Boolean vector that indicates whether a user follows the
#' political accounts used to estimate the full model
#'

supplementaryRows <- function(res, points){
  svphi <- matrix(res$sv[1:res$nd], nrow = nrow(points), ncol = res$nd,
                  byrow = TRUE)
  ## missing values
  supcol <- which(is.na(res$colmass))
  ## adapted from CA package
  cs <- res$colmass[-supcol]
  gam.00 <- res$colcoord[-supcol,]
  SR <- (as.matrix(points)*1)[,-supcol]
  rs.sum <- rowSums(points)
  base2 <- t(SR/matrix(rs.sum, nrow = nrow(SR), ncol = ncol(SR)))
  cs.0 <- matrix(cs, nrow = nrow(base2), ncol = ncol(base2))
  base2 <- base2 - cs.0
  phi2 <- (t(as.matrix(base2)) %*% gam.00)/svphi
  return(phi2)

}

#' @rdname supplementaryColumns
#' @export
#'
#' @title
#' Projects additional columns (political accounts) to a latent ideological space
#' using correspondence analysis
#'
#' @description
#' \code{supplementaryColumns} takes additional columns of a follower matrix
#' and projects them to the latent ideological space using the parameters
#' of an already-fitted correspondence analysis model.
#' Code was adapted from the \code{ca} function in the \code{ca} package
#'
#' @author
#' Michael Greenacre, Oleg Nenadic, Michael Friendly (Modified by Pablo Barbera)
#'
#' @param res Output from \code{CA} function
#' @param points Boolean vector that indicates whether a political account is
#' followed by the users included in the full model
#'

supplementaryColumns <- function(res, points){
  ## from CA package
  sv <- res$sv
  rs <- res$rowmass
  phi.00 <- res$rowcoord
  nd <- res$nd0
  SC <- matrix(points, ncol=1)
  nd <- res$nd
  supcol <- ncol(SC)

  cs.sum <- apply(SC, 2, sum)

  base2 <- SC/matrix(cs.sum, nrow=nrow(SC),
                     ncol=ncol(SC), byrow=TRUE)
  rs.0 <- matrix(rs, nrow = nrow(base2), ncol = ncol(base2))
  svgam <- matrix(sv[1:nd], nrow = length(supcol), ncol = nd,
                  byrow = TRUE)
  base2 <- base2 - rs.0
  gam2 <- (as.matrix(t(base2)) %*% phi.00)/svgam
  return(gam2)
}

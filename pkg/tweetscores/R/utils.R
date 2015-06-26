lpd <- function(alpha, beta, gamma, theta, phi, mu_beta, sigma_beta, y){
  value <- alpha + beta - gamma * (theta - phi)^2
  sum(log(plogis(value)^y * (1-plogis(value))^(1-y)))  +
    dnorm(theta, 0, 1, log=TRUE) + dnorm(beta, mean=mu_beta, sd=sigma_beta, log=TRUE)
}

metropolis.logit <- function(y,
                             alpha.i=tweetscores::posterior_samples$alpha,
                             gamma.i=tweetscores::posterior_samples$gamma,
                             phi.i=tweetscores::posterior_samples$phi,
                             beta.init=rep(log(sum(y)), chains),
                             theta.init=rnorm(chains, 0, 1),
                             mu_beta.i=tweetscores::posterior_samples$mu_beta,
                             sigma_beta.i=tweetscores::posterior_samples$sigma_beta,
                             iters=5000, delta=0.15, chains=2, n.warmup=1000,
                             thin=20, verbose=TRUE)
{

  # preparing vectors for stored samples
  keep <- seq(n.warmup+1, iters, by=thin)
  pars.samples <- array(NA, dim=c(length(keep), chains, 2),
                        dimnames=list(NULL,NULL,c("beta", "theta")))
  # preparing iterations from other parameters
  options(warn=-1) # deactivating warnings for different lenghts
  alpha.it <- apply(alpha.i, 2, function(x) matrix(x, nrow=iters, ncol=1))
  phi.it <- apply(phi.i, 2, function(x) matrix(x, nrow=iters, ncol=1))
  gamma.it <- matrix(gamma.i, nrow=iters, ncol=1)
  mu_beta.it <- matrix(mu_beta.i, nrow=iters, ncol=1)
  sigma_beta.it <- matrix(sigma_beta.i, nrow=iters, ncol=1)
  options(warn=0)

  # iterations of the metropolis algorithm
  for (chain in 1:chains){
    # drawing starting points
    pars.cur <- c(beta.init[chain], theta.init[chain])
    i <- 1
    if (verbose==TRUE){
      message("\nChain ", chain)
      pb <- txtProgressBar(min=1,max=iters, style=3)
    }
    # iterations
    for (iter in 1:iters){
      # getting samples from iterations
      alpha <- alpha.it[iter,]
      gamma <- gamma.it[iter]
      phi <- phi.it[iter,]
      mu_beta <- mu_beta.it[iter]
      sigma_beta <- sigma_beta.it[iter]
      # sampling candidate values
      pars.cand <- sapply(pars.cur, function(x) runif(n=1, min=x-delta, max=x+delta))
      # computing acceptance probability
      accept.prob <- exp(lpd(alpha, beta=pars.cand[1], gamma, theta=pars.cand[2], phi, mu_beta, sigma_beta, y) -
                           lpd(alpha, beta=pars.cur[1], gamma, theta=pars.cur[2], phi, mu_beta, sigma_beta, y))
      alpha <- min(accept.prob, 1)
      # jumping with probability alpha
      if (runif(1)<=alpha) { pars.cur <- pars.cand}
      # storing samples
      if (iter %in% keep) {pars.samples[i,chain,] <- pars.cur; i <- i + 1}
      if (verbose==TRUE){ setTxtProgressBar(pb, iter) }
    }
  }
  # reporting summary statistics
  results <- round(R2WinBUGS::monitor(pars.samples), 2)
  if (verbose==TRUE) {
    message("")
    print(results)
    message(chains, " chains, keeping ", length(keep),
        " iterations out of ", iters)
  }
  return(list(samples=pars.samples, Rhat=results[,"Rhat"], n.eff=results[,"n.eff"]))
}

lpd.ml <- function(pars, alpha, gamma, phi, mu_beta, sigma_beta, y){
  beta <- pars[1]; theta <- pars[2]
  value <- alpha + beta - gamma * (theta - phi)^2
  lk <- sum( y * plogis(value, log.p=TRUE) + (1-y) * plogis(-value, log.p=TRUE) ) +
    dnorm(theta, 0, 1, log=TRUE) + dnorm(beta, mean=mu_beta, sd=sigma_beta, log=TRUE)
  return(-lk)
}

ml.logit <- function(y,
                     alpha=colMeans(tweetscores::posterior_samples$alpha),
                     gamma=mean(tweetscores::posterior_samples$gamma),
                     phi=colMeans(tweetscores::posterior_samples$phi),
                     beta.init=log(sum(y)),
                     theta.init=rnorm(1, 0, 1),
                     mu_beta=mean(tweetscores::posterior_samples$mu_beta),
                     sigma_beta=mean(tweetscores::posterior_samples$sigma_beta),
                     iters=5000, chains=2, n.warmup=1000, thin=20, verbose=TRUE)
{

  # number of simulations
  sims <- length(seq(n.warmup+1, iters, by=thin))
  # maximizing the lk function
  res <- optim(par=c(beta.init, theta.init), fn=lpd.ml, alpha=alpha,
               gamma=gamma, phi=phi, mu_beta=mu_beta, sigma_beta=sigma_beta,
               y=y*1, hessian=TRUE)
  sds <- sqrt(diag(solve(res$hessian)))
  # simulating from distribution of parameters to compute percentiles, etc.
  pars.samples <- array(NA, dim=c(sims, chains, 2), dimnames=list(NULL, NULL, c("beta", "theta")))
  pars.samples[,,"beta"] <- rnorm(sims*chains, res$par[1], sds[1])
  pars.samples[,,"theta"] <- rnorm(sims*chains, res$par[2], sds[2])
  # reporting summary statistics
  results <- round(R2WinBUGS::monitor(pars.samples), 2)
  if (verbose) {
    message("")
    print(results)
  }
  return(list(samples=pars.samples, Rhat=results[,"Rhat"], n.eff=results[,"n.eff"]))
}


#==============================================================================
# 05_second_stage_functions.R
# Purpose: prepare functions that will be used in Rscript 06 to compute
# ideology for rest of ordinary users.
# Runtime: ~20 minutes
# Author: Pablo Barbera
#==============================================================================

setwd("~/Dropbox/git/twitter_ideology")
source("code/functions.R")

# load results of first stage on Stan
load("results/stanfit.Rdata")

# preparing posterior samples
samples <- extract(stan.fit, permute=FALSE)
K <- stan.fit@par_dims$alpha
J <- stan.fit@par_dims$theta
phi.i <- samples[,1,paste0("phi[",1:K, "]")]
gamma.i <- samples[,1,"gamma"]
sigma_beta.i <- samples[,1,"sigma_beta"]

samples <- extract(stan.fit, permute=FALSE)
alpha.i <- samples[,1,paste("alpha[", 1:K, "]", sep="")]
mu_beta.i <- samples[,1,"mu_beta"]

## log posterior density
lpd <- function(alpha, beta, gamma, theta, phi, mu_beta, sigma_beta, y){
    require(arm, quiet=TRUE)
    value <- alpha + beta - gamma * (theta - phi)^2
    sum(log(invlogit(value)^y * (1-invlogit(value))^(1-y)))  + 
         dnorm(theta, 0, 1, log=TRUE) + dnorm(beta, mean=mu_beta, sd=sigma_beta, log=TRUE)
}

# metropolis algorithm to compute ideology for an ordinary user
metropolis.logit <- function(y, alpha.i, gamma.i, phi.i, beta.init, theta.init, mu_beta, sigma_beta, 
    iters=2000, delta=0.05, chains=2, n.warmup=1000, thin=1, verbose=FALSE)
{
    require(R2WinBUGS, quiet=TRUE)
    # preparing vector for stored samples
    # preparing vectors for stored samples
   keep <- seq(n.warmup+1, iters, by=thin)
    pars.samples <- array(NA, dim=c(length(keep), chains, 2),
    dimnames=list(NULL,NULL,c("beta", "theta")))
    # preparing iterations from other parameters
    alpha.it <- apply(alpha.i, 2, function(x) matrix(x, nrow=iters, ncol=1))
    phi.it <- apply(phi.i, 2, function(x) matrix(x, nrow=iters, ncol=1))
    gamma.it <- matrix(gamma.i, nrow=iters, ncol=1)
    mu_beta.it <- matrix(mu_beta.i, nrow=iters, ncol=1)
    sigma_beta.it <- matrix(sigma_beta.i, nrow=iters, ncol=1)

    # iterations of the metropolis algorithm
    for (chain in 1:chains){
        # drawing starting points
        pars.cur <- c(beta.init, theta.init[chain])
        i <- 1
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
        }
    }
      # reporting summary statistics 
  results <- round(monitor(pars.samples), 2)
  if (verbose==TRUE) {
    print(results)
    cat(chains, "chains, keeping last", length(keep), 
    "iterations out of", iters, "\n")
 }
    return(list(samples=pars.samples, Rhat=results[,"Rhat"], n.eff=results[,"n.eff"]))
}

## parallelized version of metropolis algorithm

estimation <- function(first, last=first+4999){
    pars <- first:last
    beta.samples <- array(NA, dim=c(length(pars), 200), 
        dimnames=list(paste("beta[", first:last, "]", sep=""), 
            paste("Iteration ", 1:200, sep="")))
    theta.samples <- array(NA, dim=c(length(pars), 200), 
        dimnames=list(paste("theta[", first:last, "]", sep=""), 
            paste("Iteration ", 1:200, sep="")))
    theta.results <- data.frame(phat=NA, 
        id=dimnames(y)[[1]][1:length(pars)], rhat=NA, n.eff=NA,
        stringsAsFactors=F) 

    for (i in 1:length(pars)){
        fit <- metropolis.logit(y[pars[i],], alpha.i, gamma.i, phi.i, 
            beta.init=log(sum(y[pars[i],])), theta.init=rnorm(2,0,1), iters=3000, 
            delta=0.15, chains=2, n.warmup=1000, thin=20, verbose=FALSE)
        if (fit$Rhat[2]>1.05){
            fit <- metropolis.logit(y[pars[i],], alpha.i, gamma.i, phi.i, 
                beta.init=log(sum(y[pars[i],])), theta.init=rnorm(2,0,1), 
                iters=10000, delta=0.15, chains=2, n.warmup=5000, thin=50, 
                verbose=FALSE)
     }
        beta.samples[i,] <- c(fit$samples[,,"beta"])
        theta.samples[i,] <- c(fit$samples[,,"theta"])
        theta.results$phat[i] <- mean(theta.samples[i,])
        theta.results$rhat[i] <- fit$Rhat[2]
        theta.results$n.eff[i] <- fit$n.eff[2]
        cat(pars[i], "\n")
    }
    return(list(beta.samples=beta.samples, theta.samples=theta.samples, 
        theta.results=theta.results))
}


### removing objects from memory...

rm("stan.fit")
rm("samples")

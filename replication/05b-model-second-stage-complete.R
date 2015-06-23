#==============================================================================
# 05b-model-second-stage-complete.R
# Purpose: running second stage of the model (estimation of mass-level ideal
# points) with a parallelized metropolis algorithm
# Author: Pablo Barbera
#==============================================================================

# NOTE: this is the full script used to get the estimates for the 300K 
# users in the US. It can take a very long time to run, depending on how
# many cores you're using. If standard errors are not interesting, maximum
# likelihood can be used to get the point estimates and make it much faster.


source('functions.R')
library(rstan)
library(parallel)

matrixfile <- 'output/adj-matrix-US.rdata'
samplesfile <- 'output/samples-US.rdata'
resultsfile <- 'output/estimates-US.rdata'

# loading results of first stage
load(samplesfile)
alpha.i <- samples$alpha
gamma.i <- samples$gamma
phi.i <- samples$phi
mu_beta.i <- samples$mu_beta
sigma_beta.i <- samples$sigma_beta

# loading data matrix
library(Matrix)
load(matrixfile)

## log posterior density
lpd <- function(alpha, beta, gamma, theta, phi, mu_beta, sigma_beta, y){
    require(arm, quiet=TRUE)
    value <- alpha + beta - gamma * (theta - phi)^2
    sum(log(invlogit(value)^y * (1-invlogit(value))^(1-y)))  + 
         dnorm(theta, 0, 1, log=TRUE) + dnorm(beta, mean=mu_beta, sd=sigma_beta, log=TRUE)
}


# metropolis algorithm to compute ideology for an ordinary user
metropolis.logit <- function(y, alpha.i, gamma.i, phi.i, mu_beta.i, sigma_beta.i, beta.init, theta.init, 
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
    theta.results <- data.frame(theta=NA, 
        id=dimnames(y)[[1]][1:length(pars)], rhat=NA, n.eff=NA,
        stringsAsFactors=F) 

    for (i in 1:length(pars)){
        fit <- metropolis.logit(y[pars[i],], alpha.i, gamma.i, phi.i, mu_beta.i, sigma_beta.i,
            beta.init=log(sum(y[pars[i],])), theta.init=rnorm(2,0,1), iters=3000, 
            delta=0.15, chains=2, n.warmup=1000, thin=20, verbose=FALSE)
        if (fit$Rhat[2]>1.05){
            fit <- metropolis.logit(y[pars[i],], alpha.i, gamma.i, phi.i, mu_beta.i, sigma_beta.i,
                beta.init=log(sum(y[pars[i],])), theta.init=rnorm(2,0,1), 
                iters=10000, delta=0.15, chains=2, n.warmup=5000, thin=50, 
                verbose=FALSE)
     }
        beta.samples[i,] <- c(fit$samples[,,"beta"])
        theta.samples[i,] <- c(fit$samples[,,"theta"])
        theta.results$theta[i] <- mean(theta.samples[i,])
        theta.results$rhat[i] <- fit$Rhat[2]
        theta.results$n.eff[i] <- fit$n.eff[2]
        cat(pars[i], "\n")
    }
    return(list(beta.samples=beta.samples, theta.samples=theta.samples, 
        theta.results=theta.results))
}

# first batch of results
n1 <- 1
n2 <- 50000

## preparing data
load(matrixfile)
y <- y[n1:(n1+49999),]
results1 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results1, file="temp/results1.Rdata")

# repeat code above for results 2 to 6
n1 <- 50001
n2 <- 100000
load(matrixfile)
y <- y[n1:(n1+49999),]
results2 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results2, file="temp/results2.Rdata")

n1 <- 100001
n2 <- 150000
load(matrixfile)
y <- y[n1:(n1+49999),]
results3 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results3, file="temp/results3.Rdata")


n1 <- 150001
n2 <- 200000
load(matrixfile)
y <- y[n1:(n1+49999),]
results4 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results4, file="temp/results4.Rdata")

n1 <- 200001
n2 <- 250000
load(matrixfile)
y <- y[n1:(n1+49999),]
results5 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results5, file="temp/results5.Rdata")

n1 <- 250001
n2 <- 300000
load(matrixfile)
y <- y[n1:(n1+49999),]
results6 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results6, file="temp/results6.Rdata")

## last batch of results
load(matrixfile)
y <- y[300001:dim(y)[1],]
results7 <- estimation(first=1, last=dim(y)[1])


#####################################################################
##### PUTTING IT ALL TOGETHER
#####################################################################
load(matrixfile)

load("temp/results1.Rdata")
load("temp/results2.Rdata")
load("temp/results3.Rdata")
load("temp/results4.Rdata")
load("temp/results5.Rdata")
load("temp/results6.Rdata")
load("temp/results7.Rdata")


beta.samples <- list()
j <- 1

for (i in 1:length(results1)){
    beta.samples[[j]] <- results1[i][[1]][['beta.samples']]
    j <- j + 1
}
for (i in 1:length(results2)){
    beta.samples[[j]] <- results2[i][[1]][['beta.samples']]
    j <- j + 1
}
for (i in 1:length(results3)){
    beta.samples[[j]] <- results3[i][[1]][['beta.samples']]
    j <- j + 1
}
for (i in 1:length(results4)){
    beta.samples[[j]] <- results4[i][[1]][['beta.samples']]
    j <- j + 1
}
for (i in 1:length(results5)){
    beta.samples[[j]] <- results5[i][[1]][['beta.samples']]
    j <- j + 1
}
for (i in 1:length(results6)){
    beta.samples[[j]] <- results6[i][[1]][['beta.samples']]
    j <- j + 1
}
beta.samples[[j]] <- results7[['beta.samples']]

beta.samples <- t(do.call(rbind, beta.samples))


theta.samples <- list()
j <- 1

for (i in 1:length(results1)){
    theta.samples[[j]] <- results1[i][[1]][['theta.samples']]
    j <- j + 1
}
for (i in 1:length(results2)){
    theta.samples[[j]] <- results2[i][[1]][['theta.samples']]
    j <- j + 1
}
for (i in 1:length(results3)){
    theta.samples[[j]] <- results3[i][[1]][['theta.samples']]
    j <- j + 1
}
for (i in 1:length(results4)){
    theta.samples[[j]] <- results4[i][[1]][['theta.samples']]
    j <- j + 1
}
for (i in 1:length(results5)){
    theta.samples[[j]] <- results5[i][[1]][['theta.samples']]
    j <- j + 1
}
for (i in 1:length(results6)){
    theta.samples[[j]] <- results6[i][[1]][['theta.samples']]
    j <- j + 1
}

theta.samples[[j]] <- results7[['theta.samples']]

theta.samples <- t(do.call(rbind, theta.samples))

rm("results1")
rm("results2")
rm("results3")
rm("results4")
rm("results5")
rm("results6")
rm("results7")

samples <- list(
    alpha = samples$alpha,
    beta = beta.samples,
    gamma = samples$gamma,
    theta = theta.samples,
    phi = samples$phi)



### RESULTS ####

load("temp/results1.Rdata")
load("temp/results2.Rdata")
load("temp/results3.Rdata")
load("temp/results4.Rdata")
load("temp/results5.Rdata")
load("temp/results6.Rdata")
load("temp/results7.Rdata")

theta.results <- list()
j <- 1
for (i in 1:length(results1)){
    theta.results[[j]] <- results1[i][[1]][['theta.results']]
    j <- j + 1
}
for (i in 1:length(results2)){
    theta.results[[j]] <- results2[i][[1]][['theta.results']]
    j <- j + 1
}
for (i in 1:length(results3)){
    theta.results[[j]] <- results3[i][[1]][['theta.results']]
    j <- j + 1
}
for (i in 1:length(results4)){
    theta.results[[j]] <- results4[i][[1]][['theta.results']]
    j <- j + 1
}
for (i in 1:length(results5)){
    theta.results[[j]] <- results5[i][[1]][['theta.results']]
    j <- j + 1
}
for (i in 1:length(results6)){
    theta.results[[j]] <- results6[i][[1]][['theta.results']]
    j <- j + 1
}
theta.results[[j]] <- results7[['theta.results']]

theta.results <- do.call(rbind, theta.results)

#==============================================================================
## THIS IS THE KEY PART
## 'RESULTS' is a list that contains all sets of parameter estimates:
## theta = ideology for ordinary users
## theta.sd = standard deviation of ideology for ordinary users
## phi, phi.sd = same for politicians
## m.names = names of politicians (twitter handles)
## n.names = IDs of users
## rhat.theta = Rhat for users (measures convergence of MCMC chains)


results <- list(
    alpha = apply(samples$alpha, 2, mean),
    beta = apply(samples$beta, 2, mean),
    gamma = mean(samples$gamma),
    theta = apply(samples$theta, 2, mean),
    theta.sd = apply(samples$theta, 2, sd),
    phi = apply(samples$phi, 2, mean),
    phi.sd = apply(samples$phi, 2, sd),
    m.names = dimnames(y)[[2]],
    n.names = dimnames(y)[[1]],
    rhat.theta = theta.results$rhat
    )

save(results, file=resultsfile)






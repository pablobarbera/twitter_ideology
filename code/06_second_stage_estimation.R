#==============================================================================
# 06_second_stage_estimation.R
# Purpose: running second stage of the model (estimation of mass-level ideal
# points) with a parallelized metropolis algorithm
# Runtime: ~48 hours
# Note: this script is prepared for n=305,000
# Author: Pablo Barbera
#==============================================================================

library(rstan)

# parameters
n1 <- 1
n2 <- 45001

## preparing data
load("temp/matrix.Rdata")
y <- y[n1:(n2+4999),]

## loading functions
source("05_second_stage_functions.R")

### parallel computing
library(multicore)

## this is for the 1st batch of results
results1 <- mclapply(seq(n1,n2,5000), estimation, mc.cores=12)
save(results1, file="temp/results1.Rdata")

# repeat code above for results 2 to 6

## last batch of results
load("temp/fullmatrix.Rdata")
y <- y[300001:dim(y)[1],]
source("05_second_stage_functions.R")
library(multicore)
results7 <- estimation(first=1, last=dim(y)[1])

#####################################################################
##### PUTTING IT ALL TOGETHER
#####################################################################


### loading and merging

load("temp/fullmatrix.Rdata")

load("temp/results1.Rdata")
load("temp/results2.Rdata")
load("temp/results3.Rdata")
load("temp/results4.Rdata")
load("temp/results5.Rdata")
load("temp/results6.Rdata")
load("temp/results7.Rdata")

## loading first-stage results
load("results/stanfit_old.Rdata")
samples <- extract(stan.fit, permute=FALSE)
K <- stan.fit@par_dims$alpha
J <- stan.fit@par_dims$theta


alpha.samples <-  samples[,1,paste("alpha[", 1:K, "]", sep="")]

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

gamma.samples <- samples[,1,"gamma"]

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

phi.samples <- samples[,1,paste("phi[", 1:K, "]", sep="")]

rm("results1")
rm("results2")
rm("results3")
rm("results4")
rm("results5")
rm("results6")
rm("results7")

samples <- list(
    alpha = alpha.samples,
    beta = beta.samples,
    gamma = gamma.samples,
    theta = theta.samples,
    phi = phi.samples)

save(samples, file="results/samples_stan.Rdata")

rm(list=ls())

### RESULTS ####

load("results/samples_stan.Rdata")
load("results/stanfit_old.Rdata")
load("temp/fullmatrix.Rdata")

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
    m.names = names(stan.fit@stan_args[[1]]$init$alpha),
    n.names = dimnames(y)[[1]],
    rhat.theta = theta.results$rhat
    )

save(results, file="results/results_stan.Rdata")

#==============================================================================

rm(list=ls())

### SANITY CHECK
load("results/stanfit.Rdata")
load("results/results_stan.Rdata")
estimates <- summary(stan.fit, permute=FALSE)
results.met <- data.frame(id=results$n.names, phat.met=results$theta, stringsAsFactors=F)
results.stan <- data.frame(id=names(stan.fit@stan_args[[1]]$init$beta), 
    phat.stan=estimates$c_summary[paste("theta[", 1:10000, "]", sep=""),1,1], 
    stringsAsFactors=F)
results.tot <- merge(results.met, results.stan)
plot(results.tot$phat.met, results.tot$phat.stan)
cor(results.tot$phat.met, results.tot$phat.stan)


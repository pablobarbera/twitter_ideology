#==============================================================================
# 03-model-first-stage.R
# Purpose: fitting spatial following model
# Runtime: ~18 hours on NYU HPC
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
library(Matrix)
setwd("~/git/twitter_ideology/primary")

matrixfile <- 'data/adj-matrix-US.rdata'
outputfile <- 'output/stan-fit-US.rdata'
resultsfile <- 'output/results-elites-US.rdata'

# loading data
load(matrixfile)

# initial values
users <- read.csv('data/accounts-twitter-data.csv', stringsAsFactors=F)
users <- users[users$followers_count>1000,]
start.phi <- rep(0, nrow(users))
start.phi[users$party=="Democrat"] <- -1
start.phi[users$party=="Republican"] <- +1

# choosing a sample of 10,000 "informative" users who follow 10 or more
# politicians, and then subsetting politicians followed by >200 of these
J <- dim(y)[1]
rs <- rowSums(y[,colnames(y) %in% users$screen_name[users$type=="Congress"]])

if (J>10000){
  J <- 10000
  inform <- which(rs>10)
  set.seed(12345)
  subset.i <- sample(inform, J)
  y <- y[subset.i, ]
  start.phi <- start.phi[which(colSums(y)>200)]
  census <- users$screen_name[which(colSums(y)>200)]
  y <- y[,which(colSums(y)>200)]
}

## data for model
J <- dim(y)[1]
K <- dim(y)[2]
N <- J * K
jj <- rep(1:J, times=K)
kk <- rep(1:K, each=J)
colK <- colSums(y)
rowJ <- rowSums(y)
y <- c(as.matrix(y))

stan.data <- list(J=J, K=K, N=length(y), jj=jj, 
    kk=kk, y=y*1)

normalize <- function(x){ (x-mean(x))/sd(x) }

inits <- function(chain_id = 1){
    list(alpha=normalize(log(colK+0.0001)), 
        beta=normalize(log(rowJ+0.0001)),
        theta=rnorm(J, 0, 1), phi=start.phi,mu_beta=0, sigma_beta=1, 
        gamma=abs(rnorm(1, 0.8, 0.2)), mu_phi=mean(start.phi), 
        sigma_phi=1, sigma_alpha=1)
}

library(rstan)

stan.code <- '
data {
  int<lower=1> J; // number of twitter users
  int<lower=1> K; // number of elite twitter accounts
  int<lower=1> N; // N = J x K
  int<lower=1,upper=J> jj[N]; // twitter user for observation n
  int<lower=1,upper=K> kk[N]; // elite account for observation n
  int<lower=0,upper=1> y[N]; // dummy if user i follows elite j
}
parameters {
  vector[K] alpha;
  vector[K] phi;
  vector[J] theta;
  vector[J] beta;
  real mu_beta;
  real<lower=0.1> sigma_beta;
  real<lower=0.1> sigma_alpha;
  real gamma;
}
model {
  alpha ~ normal(0, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  phi ~ normal(0, 1);
  theta ~ normal(0, 1);
  gamma ~ normal(0.8, 0.2); 
  for (n in 1:N)
    y[n] ~ bernoulli_logit( alpha[kk[n]] + beta[jj[n]] - 
      gamma * square( theta[jj[n]] - phi[kk[n]] ) );
}
'

# parameters for Stan model
n.iter <- 300
n.warmup <- 100
thin <- 1

## compiling model
stan.model <- stan(model_code=stan.code, 
    data = stan.data, init=inits, iter=1, warmup=0, chains=1)

## running model (2 chains)
library(parallel)
sflist <- 
  mclapply(1:2, mc.cores = 2, 
           function(i) stan(fit=stan.model, data=stan.data, 
                            chains = 1, chain_id = i, refresh = 1,
                            iter = n.iter, warmup = n.warmup, 
                            thin = thin, init = inits))

stan.fit <- sflist2stanfit(sflist)
save(stan.fit, file=outputfile)

## extracting and saving samples
samples <- extract(stan.fit, pars=c("alpha", "phi", "gamma", "mu_beta",
  "sigma_beta", "sigma_alpha"))

## saving estimates
results <- data.frame(
	screen_name = census,
	phi = apply(samples$phi[101:200,], 2, mean),
	phi.sd = apply(samples$phi[101:200,], 2, sd),
	alpha = apply(samples$alpha, 2, mean),
	alpha.sd = apply(samples$alpha, 2, sd),
	stringsAsFactors=F)

results <- results[order(results$phi),]
results[,c("screen_name", "phi", "phi.sd")]











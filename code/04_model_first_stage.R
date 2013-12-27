#==============================================================================
# 04_model_first_stage.R
# Purpose: fitting spatial following model
# Runtime: ~18 hours on NYU HPC
# Author: Pablo Barbera
#==============================================================================

options(scipen=20)

# load likes matrix
load("temp/matrix.Rdata")
load("temp/startingvalues.Rdata")

# parameters for Stan model
n.iter <- 250
n.warmup <- 100
thin <- 1

J <- dim(y)[1]

# choosing a sample of 10,000 "informative" users who follow 10 or more
# politicians, and then subsetting politicians followed by >200 of these

if (J>10000){
  J <- 10000
  inform <- which(rowSums(y)>10)
  set.seed(12345)
  subset.i <- sample(inform, J)
  y <- y[subset.i, ]
  start.phi <- start.phi[which(colSums(y)>200)]
  y <- y[,which(colSums(y)>200)]
}


J <- dim(y)[1]
K <- dim(y)[2]
N <- J * K
jj <- rep(1:J, times=K)
kk <- rep(1:K, each=J)

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
  real mu_phi;
  real<lower=0.1> sigma_phi;
  real gamma;
}
model {
  alpha ~ normal(0, 1);
  beta ~ normal(mu_beta, sigma_beta);
  phi ~ normal(mu_phi, sigma_phi);
  theta ~ normal(0, 1); 
  for (n in 1:N)
    y[n] ~ bernoulli_logit( alpha[kk[n]] + beta[jj[n]] - 
      gamma * square( theta[jj[n]] - phi[kk[n]] ) );
}
'

stan.data <- list(J=J, K=K, N=N, jj=jj, kk=kk, y=c(y))
colK <- colSums(y)
rowJ <- rowSums(y)
normalize <- function(x){
  (x-mean(x))/sd(x)
}


## RUNNING MODEL
inits <- list(list(alpha=normalize(log(colK+0.0001)), beta=normalize(log(rowJ+0.0001)),
  theta=rep(0, J), phi=start.phi,
  mu_beta=0, sigma_beta=1, gamma=2,  mu_phi=0, sigma_phi=1))
stan.fit <- stan(model_code=stan.code, data = stan.data, iter=n.iter, warmup=n.warmup, chains=1, 
  thin=1, init=inits)


save(stan.fit, file="results/stanfit.Rdata")




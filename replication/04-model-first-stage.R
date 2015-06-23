#==============================================================================
# 04-model-first-stage.R
# Purpose: fitting spatial following model
# Runtime: ~18 hours on NYU HPC
# Author: Pablo Barbera
#==============================================================================

source('functions.R')

## change the following lines to run this Rscript for other countries
matrixfile <- 'output/adj-matrix-US.rdata'
outputfile <- 'temp/stan-fit-US.rdata'
samplesfile <- 'output/samples-US.rdata'
resultsfile <- 'output/results-elites-US.rdata'
country <- 'US'

# parameters for Stan model
n.iter <- 500
n.warmup <- 100
thin <- 2 ## this will give up to 200 effective samples for each chain and par

# loading data
load(matrixfile)

## starting values for elites (for identification purposes)
load("elites-data.Rdata")

# US:
if (country=="US"){
	us <- elites.data[['US']]
	parties <- merge(
		data.frame(screen_name = colnames(y), stringsAsFactors=F),
		us[,c("screen_name", "party")], sort=FALSE, all.x=TRUE)$party
	start.phi <- rep(0, length(parties))
	start.phi[parties=="D"] <- -1
	start.phi[parties=="R"] <- 1
}
# United Kingdom
if (country=="UK"){
	uk <- elites.data[['UK']]
	parties <- merge(
		data.frame(screen_name = colnames(y), stringsAsFactors=F),
		uk[,c("screen_name", "party")], sort=FALSE, all.x=TRUE)$party
	start.phi <- rep(0, length(parties))
	start.phi[parties=="labour"] <- -1
	start.phi[parties=="conservatives"] <- 1
}
# Netherlands
if (country=="NL"){
	nl <- elites.data[['NL']]
	parties <- merge(
		data.frame(screen_name = colnames(y), stringsAsFactors=F),
		nl[,c("screen_name", "party")], sort=FALSE, all.x=TRUE)$party
	start.phi <- rep(0, length(parties))
	start.phi[parties %in% c('PVDA', 'GL', 'SP')] <- -1
	start.phi[parties %in% c('CDA', 'VVD')] <- -1
}
# Spain
if (country=="spain"){
	sp <- elites.data[['spain']]
	parties <- merge(
		data.frame(screen_name = colnames(y), stringsAsFactors=F),
		sp[,c("screen_name", "party")], sort=FALSE, all.x=TRUE)$party
	start.phi <- rep(0, length(parties))
	start.phi[parties %in% c('pp', 'ciu', 'eajpnv')] <- 1
	start.phi[parties %in% c('psoe', 'iu', 'iu-icv', 'equo')] <- -1
}
# Italy
if (country=="italy"){
	it <- elites.data[['italy']]
	parties <- merge(
		data.frame(screen_name = colnames(y), stringsAsFactors=F),
		it[,c("screen_name", "party")], sort=FALSE, all.x=TRUE)$party
	start.phi <- rep(0, length(parties))
	start.phi[parties %in% c('M5S', 'SEL')] <- -1
	start.phi[parties %in% c('PD', 'IDV')] <- -0.5
	start.phi[parties %in% c('UDC', 'IND', 'PDL', 'FLI', 'MA')] <- 0.5
	start.phi[parties %in% c('LN')] <- 1
}
# Germany
if (country=="germany"){
	de <- elites.data[['germany']]
	parties <- merge(
		data.frame(screen_name = colnames(y), stringsAsFactors=F),
		de[,c("screen_name", "party")], sort=FALSE, all.x=TRUE)$party
	start.phi <- rep(0, length(parties))
	start.phi[parties %in% c('CDU', 'CSU')] <- 1
	start.phi[parties %in% c('GREEN', 'LINKE', 'SPD')] <- -1
}


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

## data for model

J <- dim(y)[1]
K <- dim(y)[2]
N <- J * K
jj <- rep(1:J, times=K)
kk <- rep(1:K, each=J)

stan.data <- list(J=J, K=K, N=N, jj=jj, kk=kk, y=c(as.matrix(y)))

## rest of starting values
colK <- colSums(y)
rowJ <- rowSums(y)
normalize <- function(x){ (x-mean(x))/sd(x) }

inits <- rep(list(list(alpha=normalize(log(colK+0.0001)), 
	beta=normalize(log(rowJ+0.0001)),
  theta=rnorm(J), phi=start.phi,mu_beta=0, sigma_beta=1, 
  gamma=abs(rnorm(1)), mu_phi=0, sigma_phi=1, sigma_alpha=1)),2)


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
  real<lower=0.1> sigma_alpha;
  real gamma;
}
model {
  alpha ~ normal(0, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  phi ~ normal(mu_phi, sigma_phi);
  theta ~ normal(0, 1); 
  for (n in 1:N)
    y[n] ~ bernoulli_logit( alpha[kk[n]] + beta[jj[n]] - 
      gamma * square( theta[jj[n]] - phi[kk[n]] ) );
}
'

## compiling model
stan.model <- stan(model_code=stan.code, 
    data = stan.data, inits=inits, iter=1, warmup=0, chains=1)

## running modle
stan.fit <- stan(fit=stan.model, data = stan.data, 
	iter=n.iter, warmup=n.warmup, chains=2, 
  	thin=thin, inits=inits)

save(stan.fit, file=outputfile)

## extracting and saving samples
samples <- extract(stan.fit, pars=c("alpha", "phi", "gamma", "mu_beta",
	"sigma_beta", "sigma_alpha"))
save(samples, file=samplesfile)

## saving estimates
results <- data.frame(
	screen_name = samples$m.names,
	phi = apply(samples$phi, 2, mean),
	phi.sd = apply(samples$phi, 2, sd),
	alpha = apply(samples$alpha, 2, mean),
	alpha.sd = apply(samples$alpha, 2, sd),
	stringsAsFactors=F)
save(results, file=resultsfile)









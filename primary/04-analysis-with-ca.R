#==============================================================================
# 04-analysis-with-ca.R
# Purpose: alternative method to estimate parameters using Correspondence
# Analysis.
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
library(Matrix)
setwd("~/git/twitter_ideology/primary")

matrixfile <- 'data/adj-matrix-US.rdata'
resultsfile <- 'output/ca-results-US.rdata'

# loading data
load(matrixfile)

## subsetting matrix: only those who follow 2+ MCs or primary candidates to help 
## identifying latent ideological space
users <- read.csv('data/accounts-twitter-data.csv', stringsAsFactors=F)
included <- users$twitter[users$type %in% c("Congress", "Primary Candidate")]
supcol <- which(tolower(colnames(y)) %in% included == FALSE)
colnames(y)[supcol] ## these will be excluded
rs <- rowSums(y[,-supcol]) ## rowsums
y <- y[-which(rs<3),] ## excluding users following <3 accounts

# fitting CA model
res <- CA(y, nd=3, supcol=supcol)

save(res, file=resultsfile)


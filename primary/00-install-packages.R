#==============================================================================
# 00-install-packages.R
# Purpose: install R packages and provide information about how to create
# OAuth token to query Twitter's API
# Author: Pablo Barbera
#==============================================================================

#### INSTALLING R PACKAGES FROM CRAN ####

doInstall <- TRUE  # Change to FALSE if you don't want packages installed.

toInstall <- c(
    "ggplot2", "scales", "gridExtra",  ## gplot2 and extensions
    "streamR", ## library to capture and parse Tweets
    "twitteR", ## library to query Twitter REST API
    "Matrix", ## efficient storage of sparse matrices
    "reshape2", ## reshape data frames
    "R2WinBUGS", ## used in Metropolis sampler
    "devtools" ## will use this to install paper's package
    )

if(doInstall){
    install.packages(toInstall, repos = "http://cran.r-project.org")
}

#### INSTALLING R PACKAGE WITH TWITTER FUNCTIONS ####
library(devtools)
install_github("pablobarbera/twitter_ideology/pkg/tweetscores")

### INSTALLING STAN #### 

# For most up-to-date instructions, go to:
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

Sys.setenv(MAKEFLAGS = "-j4") 

source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
install_rstan()

### REGISTERING OAUTH TOKEN ###

## Step 1: go to apps.twitter.com and sign in
## Step 2: click on "Create New App"
## Step 3: fill name, description, and website (it can be anything, even google.com)
##          (make sure you leave 'Callback URL' empty)
## Step 4: Agree to user conditions
## Step 5: copy consumer key and consumer secret and paste below

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "XXXXXXXXXXXX"
consumerSecret <- "YYYYYYYYYYYYYYYYYYY"

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
  consumerSecret=consumerSecret, requestURL=requestURL,
  accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## Setting working folder
## From windows machine usually this works
# setwd("H:\\credentials\\twitter")
## From Mac computer, something like...
setwd("~/Dropbox/credentials/twitter/")

## now you can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file="my_oauth")

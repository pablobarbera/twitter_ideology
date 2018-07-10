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
  "Matrix", ## efficient storage of sparse matrices
  "reshape2", ## reshape data frames
  "devtools", ## will use this to install other packages
  "rlang"
)

if(doInstall){
  install.packages(toInstall, repos = "http://cran.r-project.org")
}

#### INSTALLING R PACKAGE WITH TWITTER FUNCTIONS ####
library(devtools)
install_github("pablobarbera/twitter_ideology/pkg/tweetscores")

### REGISTERING OAUTH TOKEN ###

# 1. Go to apps.twitter.com and sign in.  
# 2. Click on "Create New App". You will need to have a phone number
# associated with your account in order to be able to create a token.  
# 3. Fill name, description, and website (it can be anything, even http://www.google.com). 
# Make sure you leave 'Callback URL' empty.
# 4. Agree to user conditions.  
# 5. From the "Keys and Access Tokens" tab, copy consumer key and consumer secret
# and paste below
# 6. Click on "Create my access token", then copy and paste your access token
#and access token secret below

library(ROAuth)
my_oauth <- list(consumer_key = "CONSUMER_KEY",
                 consumer_secret = "CONSUMER_SECRET",
                 access_token="ACCESS_TOKEN",
                 access_token_secret = "ACCESS_TOKEN_SECRET")

## Setting working folder
## From windows machine usually this works
# setwd("H:\\credentials\\twitter")
## From Mac computer, something like...
setwd("~/Dropbox/credentials/twitter/")

## now you can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file="my_oauth")
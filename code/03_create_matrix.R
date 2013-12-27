#==============================================================================
# 03_create_matrix.R
# Purpose: create adjacency matrix indicating what users follow each
# politician
# Runtime: ~20 minutes
# Author: Pablo Barbera
#==============================================================================

setwd("~/Dropbox/git/twitter_ideology")
source("code/functions.R")

#==============================================================================
# CENSUS: M
#==============================================================================

user.data <- read.csv("data/congress_social_media_data.csv", stringsAsFactors=F)
census <- user.data$congress_account[user.data$congress_account_id!=""]
m <- length(census)

#==============================================================================
# USERS: N
#==============================================================================

# loading entire user list following >=3 politicians
load("temp/userlist.Rdata")

users <- as.character(userlist)
n <- length(users)

#==============================================================================
# CREATING COMPLETE MATRIX
#==============================================================================

# first, checking names
filesList <- gsub(".rdata", "", list.files("followers_lists"))
census <- census[census %in% filesList == TRUE]
m <- length(census)

y <- matrix(NA, nrow=n, ncol=m)
rownames(y) <- users
colnames(y) <- census


pb <- txtProgressBar(min=1,max=m)
for (j in 1:m){
	load(paste(getwd(), "/followers_lists/", census[j], ".rdata", sep=""))
	y[,j] <- (users %in% followers) * 1
	setTxtProgressBar(pb, j)
}

users <- row.names(y)
n <- length(users)

save(y, file="temp/matrix.Rdata")


#==============================================================================
# CHOOSING STARTING VALUES
#==============================================================================

d <- merge(data.frame(congress_account=census, stringsAsFactors=F),
    user.data, sort=FALSE)

# -1 for D, +1 for R
start.phi <- ifelse(d$party=="R", 1, -1)

save(start.phi, file="temp/startingvalues.Rdata")








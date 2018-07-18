#==============================================================================
# 03-estimation.R
# Purpose: produce ideology estimates. First, estimate with a subset of the
# adjacency matrix that follow 3+ MCs and a total of 10+ accounts; then for 
# the rest of users, and users in the sample for this project.
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
library(Matrix)

# params
folder <- 'followers-lists-201807'
usermatrix <- 'adjmatrix201807'
dropbox <- "~/Dropbox (Personal)/"
resultsfile <- 'ca-results-201807'

#==============================================================================
## ESTIMATION: FIRST STAGE W/CORRESPONDENCE ANALYSIS
#==============================================================================

# loading data
load(paste0(dropbox, 'tweetscores/', usermatrix, '.rdata'))

## subsetting matrix: only those who follow 3+ MCs to help 
## identify latent ideological space
congress <- readr::read_csv(paste0(dropbox, "tweetscores/accounts-twitter-data-2018-07.csv"),
                            col_types = "ccccciiiccccccccccc")
included <- tolower(congress$screen_name)[congress$type == "Congress"]
supcol <- which(tolower(colnames(y)) %in% included == FALSE)
colnames(y)[supcol] ## these will be excluded

# fitting CA model with reduced matrix
res <- CA(y, nd=3, supcol=supcol)

save(res, file=paste0(dropbox, 'tweetscores/', resultsfile, '.rdata'))

#==============================================================================
## SANITY CHECKS
#==============================================================================

load(paste0(dropbox, 'tweetscores/', resultsfile, '.rdata'))
congress <- readr::read_csv(paste0(dropbox, "tweetscores/accounts-twitter-data-2018-07.csv"),
                            col_types = "ccccciiiccccccccccc")

# merging with data
congress$merge <- tolower(congress$screen_name)
congress <- merge(congress, data.frame(
  merge=tolower(res$colnames), phi=-as.numeric(res$colcoord[,1]), stringsAsFactors=F))
congress$phi <- scale(congress$phi)


# who is on the extremes
head(congress[order(congress$phi),])
tail(congress[order(congress$phi),])

# primary candidates
congress <- congress[order(congress$phi),]
congress[congress$type=="Primary Candidate",c("screen_name", "phi")]

#screen_name         phi
#11    BernieSanders -1.29000243
#77   HillaryClinton -1.12139469
#99       JohnKasich -0.04148960
#149 realDonaldTrump -0.02979122
#110      marcorubio  0.30417649
#88          JebBush  0.38918179
#68      GovChristie  0.53862328
#572         tedcruz  1.07185418

# others
congress[congress$type=="Media Outlets",c("screen_name", "phi")]
congress[congress$type=="Journalists",c("screen_name", "phi")]
congress[congress$type=="Other Politicians",c("screen_name", "phi")]

# validation
library(dplyr)
dd <- group_by(congress, chamber, party)
summarize(dd, cor(ideal, phi))

#   chamber      party cor(ideal, phi)
#     (chr)      (chr)            (dbl)
# 1   House   Democrat        0.5492393
# 2   House Republican        0.4192808
# 3  Senate   Democrat        0.5706382
# 4  Senate Republican        0.4767923

library(ggplot2)
p <- ggplot(congress, aes(x=ideal, y=phi, color=party))
pq <- p + geom_point() + facet_wrap(~ chamber) +
  scale_color_manual(values=c("blue", "red"))
pq

p <- ggplot(congress, aes(x=phi, fill=party))
pq <- p + geom_density() + facet_wrap(~ chamber) +
  scale_fill_manual(values=c("blue", "red"))
pq

#==============================================================================
## UPLOADING ACCOUNT-LEVEL DATA
#==============================================================================

library(bigrquery)
project <- "usc-barbera"
set_service_token(paste0(dropbox, "credentials/bigquery-token.json"))

# uploading political accounts table
tab <- bq_table("usc-barbera", "tweetscores", "accountsestimates")
bq_table_upload(tab, congress)



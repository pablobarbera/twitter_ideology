#==============================================================================
# 03-estimation.R
# Purpose: produce ideology estimates. First, estimate with a subset of the
# adjacency matrix that follow 3+ MCs; then for the rest.
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
options(stringsAsFactors=F)
setwd("~/git/twitter_ideology/2016-election")
matrixfile <- 'data/adj-matrix-US.rdata'
resultsfile <- 'data/reduced-matrix-results.rdata'
outputfile <- 'data/ideology-estimates-20160101.rdata'
outputfilecsv <- 'data/ideology-estimates-20160101.csv'

# loading data
load(matrixfile)

## subsetting matrix: only those who follow 3+ MCs to help 
## identify latent ideological space
users <- read.csv("data/accounts-twitter-data.csv")
included <- users$twitter[users$type %in% c("Congress")]
supcol <- which(tolower(colnames(y)) %in% included == FALSE)
colnames(y)[supcol] ## these will be excluded
rs <- rowSums(y[,-supcol]) ## rowsums
newy <- y[-which(rs<3),] ## excluding users following <3 accounts

# fitting CA model with reduced matrix
res <- CA(newy, nd=3, supcol=supcol)

save(res, file=resultsfile)

#==============================================================================
## VALIDATION
#==============================================================================

# merging with data
users <- read.csv('data/accounts-twitter-data.csv', stringsAsFactors=F)
users <- merge(users, data.frame(
	twitter=tolower(res$colnames), phi1=res$colcoord[,1], 
	phi2=res$colcoord[,2], phi3=res$colcoord[,3], stringsAsFactors=F))

# who is on the extremes
head(users[order(users$phi1),])
tail(users[order(users$phi1),])

# primary candidates
users <- users[order(users$phi1),]
users[users$type=="Primary Candidate",c("screen_name", "phi1")]

#         screen_name        phi1
# 548      SenSanders -0.92013210
# 129   MartinOMalley -0.90287394
# 117   LincolnChafee -0.73712461
# 83   HillaryClinton -0.60077731
# 102      JimWebbUSA -0.04502052
# 70      gov_gilmore  0.40719497
# 71      GovChristie  0.59047439
# 76       GrahamBlog  0.69157606
# 108      JohnKasich  0.77414571
# 95          JebBush  0.79967822
# 72   GovernorPataki  0.82375241
# 169 realDonaldTrump  0.88911528
# 472    RickSantorum  1.16065941
# 123      marcorubio  1.23982284
# 25     CarlyFiorina  1.24576870
# 74  GovMikeHuckabee  1.29905012
# 17      BobbyJindal  1.32647736
# 73    GovernorPerry  1.34255394
# 484     ScottWalker  1.43519830
# 164        RandPaul  1.44800182
# 569         tedcruz  1.68322519
# 168   RealBenCarson  1.70647329

# others
users[users$type=="Media Outlets",c("screen_name", "phi1")]
users[users$type=="Journalists",c("screen_name", "phi1")]
users[users$type=="Other Politicians",c("screen_name", "phi1")]

# adding Jackman's ideal point data
# downloaded from http://jackman.stanford.edu/blog/
house <- read.csv("data/house.csv", stringsAsFactors=F); house$chamber <- "House"
senate <- read.csv("data/senate.csv", stringsAsFactors=F); senate$chamber <- "Senate"
ideal <- rbind(house[,c("nameid", "idealPoint", "chamber")], 
			senate[,c("nameid", "idealPoint", "chamber")])
names(ideal) <- c("bioid", "ideal", "chamber")
users <- merge(users, ideal)

# validation
library(dplyr)
dd <- group_by(users, chamber, party)
summarize(dd, cor(ideal, phi1))

#   chamber      party cor(ideal, phi1)
#     (chr)      (chr)            (dbl)
# 1   House   Democrat        0.5492393
# 2   House Republican        0.4192808
# 3  Senate   Democrat        0.5706382
# 4  Senate Republican        0.4767923

library(ggplot2)
p <- ggplot(users, aes(x=ideal, y=phi1, color=party))
pq <- p + geom_point() + facet_wrap(~ chamber) +
	scale_color_manual(values=c("blue", "red"))
pq

p <- ggplot(users, aes(x=phi1, fill=party))
pq <- p + geom_density() + facet_wrap(~ chamber) +
	scale_fill_manual(values=c("blue", "red"))
pq


#==============================================================================
## PROJECTION OF FULL MATRIX
#==============================================================================

load(resultsfile)

# first checking everything works in a subset of the dataset
ids <- sample(res$rownames, 1000)
points <- y[dimnames(y)[[1]] %in% ids,]
phis <- res$rowcoord[res$rownames %in% ids, 1]
pred <- supplementaryRows(res, as.matrix(points))
cor(phis, pred[,1]) # 0.941709
plot(phis, pred[,1])

# now all users, split in groups of 100,000
groups <- as.numeric(cut(1:nrow(y), c(seq(0, nrow(y), 100000), nrow(y))))
n.groups <- length(unique(groups))
results <- list()
for (i in 1:n.groups){
	cat(i, "/", n.groups, "\n")
	# here I'm projecting the new users not included before as supplementary rows
	# in the correspondence analysis method
	results[[i]] <- supplementaryRows(res, as.matrix(y[which(groups==i),]))
}

results <- do.call(rbind, results)

# putting it all together in a dataset
ideology <- data.frame(
	id = dimnames(results)[[1]],
	theta = results[,1],
	pol.follow = rowSums(y[1:nrow(results),]))

# normalizing to N(0,1)
ideology$theta <- (ideology$theta - mean(ideology$theta)) / sd(ideology$theta)

save(ideology, file=outputfile)
write.csv(ideology, file=outputfilecsv, row.names=FALSE)

# descriptive statistics
summary(ideology$theta)
hist(sample(ideology$theta, 50000), breaks=100)

prop.table(table(ideology$theta<(-1))) # 9%
prop.table(table(ideology$theta>(-1) & ideology$theta<0)) # 51%
prop.table(table(ideology$theta>(0) & ideology$theta<1)) # 29%
prop.table(table(ideology$theta>(1))) # 10%






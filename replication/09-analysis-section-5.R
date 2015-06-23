#==============================================================================
# 09-analysis-section-5
# Purpose: study of political polarization in 2012 presidential election
# Author: Pablo Barbera
#==============================================================================

library(ggplot2)
library(scales)
library(reshape2)

#==============================================================================
# Figure 6: number of tweets sent, by ideology
#==============================================================================

## loading user estimates and data
load("output/users-data-US.rdata")

## loading tweet counts
load("data/tweet-counts.rdata")

## merging
results <- merge(users.tweets, users)

## preparing data for histogram
results$q <- cut(results$theta, breaks=seq(-3, 3, 0.05))
counts1 <- aggregate(results$obama, by=list(q=results$q), FUN=sum)
counts1$keyword <- 'Obama'
counts2 <- aggregate(results$romney, by=list(q=results$q), FUN=sum)
counts2$keyword <- 'Romney'
counts <- rbind(counts1, counts2)
counts$theta <- rep(seq(-(3-0.025), 3, 0.05), 2)

# figure 6
p <- ggplot(counts, aes(x=theta, y=x))
pq <- p + geom_bar(stat="identity") + 
		facet_wrap(~keyword,nrow=1) +
		scale_x_continuous("Estimated Ideology", limits=c(-3, 3)) + 
		scale_y_continuous("Count of Sent Tweets",
			breaks=c(0,50000,100000,150000,200000), 
			labels=c("0", "50K", "100K", "150K", "200K"),
			expand=c(0,0)) +
		theme_bw()
pq

ggsave(filename="plots/figure6.pdf", plot=pq, 
        height=3, width=9)


#==============================================================================
# Figure 7: political polarization in retweets
#==============================================================================

# loading retweets data
load("data/retweets-data.rdata")

## loading user and elites estimates
load("output/users-data-US.rdata")
load("output/results-elites-US.rdata")
users <- users[,c("uid", "theta")]
results <- results[,c("screen_name", "phi")]
names(results) <- c("uid", "theta")
users <- rbind(users, results)

## merging with ideology estimates
names(users)[1] <- 'retweeter_uid'
retweets <- merge(retweets, users)
names(retweets)[4] <- "phat_y"
names(users)[1] <- 'retweeted_uid'
retweets <- merge(retweets, users)
names(retweets)[5] <- "phat_x"
	
## function to summarize data for heatmap
min <- -3
max <- 3
breaks <- 0.10

expand_data <- function(breaks=0.10, candidate, min=-3, max=3){
	x <- retweets$phat_x[retweets$candidate==candidate]
	y <- retweets$phat_y[retweets$candidate==candidate]	
	x <- (round((x - min) / breaks, 0) * breaks) + min
	y <- (round((y - min) / breaks, 0) * breaks) + min
	tab <- table(x, y)
	tab <- melt(tab)
	tab$prop <- tab$value/sum(tab$value)
	tab$candidate <- candidate
	return(tab)
}

obamaxy <- expand_data(breaks=0.20, candidate="Obama")
romneyxy <- expand_data(breaks=0.20, candidate="Romney")
xy <- rbind(obamaxy, romneyxy)

## quantities reported in paper
sum(xy$prop[xy$x<0 & xy$y<0 & xy$candidate=="Obama"])
sum(xy$prop[xy$x<0 & xy$y<0 & xy$candidate=="Romney"])

sum(xy$prop[xy$x>0 & xy$y>0 & xy$candidate=="Obama"])
sum(xy$prop[xy$x>0 & xy$y>0 & xy$candidate=="Romney"])

tw <- sum(xy$value[xy$y<(-0.5)])
sum(xy$value[xy$y<(-0.5) & xy$x>(-0.5)])/tw

tw <- sum(xy$value[xy$y>(0.5)])
sum(xy$value[xy$y>(0.5) & xy$x<(0.5)])/tw


p <- ggplot(xy, aes(x, y))
pq <- p + geom_tile(aes(fill=prop), colour="white") + scale_fill_gradient(name="% of Tweets", 
		low = "white", high = "black", labels=percent_format()) +  
		labs(x="Estimated Ideology of Retweeter", y="Estimated Ideology of Author") + 
		scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-2.5, 2.5)) +
		scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-2.5, 2.5)) +
		facet_grid(. ~ candidate) + 
     	theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
     	coord_equal()
pq

ggsave(filename="plots/figure7.pdf", plot=pq, 
		height=4, width=8)








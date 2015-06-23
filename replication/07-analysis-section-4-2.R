#==============================================================================
# 07-analysis-section-4-2
# Purpose: validation of model results for legislators and parties
# Author: Pablo Barbera
#==============================================================================

source('functions.R')
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

#==============================================================================
# Figure 4: Distribution of ordinary users and political elites
#==============================================================================

## loading user estimates and data
load("output/users-data-US.rdata")

df1 <- users[users$type!="NA", c("theta", "type")]
df1$facet <- "Mass Ideal Points"

## adding elite estimates
load("output/results-elites-US.rdata")
results$type <- "Political Actors"
users$type <- "Ordinary Users"
names(results)[2] <- 'theta'

df2 <- rbind(users[,c("theta", "type")],
        results[,c("theta", "type")])

df2$facet <- 'Elite and Mass Ideal Points'

p <- ggplot(df2, aes(x=theta, linetype=type))
pq1 <- p + geom_density() + facet_wrap(~facet, nrow=1) + scale_y_continuous("distribution density") +
        scale_x_continuous("Twitter-Based", breaks=seq(-3,3,1), limits=c(-3.5, 3.5)) + theme_bw() +
        scale_linetype_discrete("Group") + 
        theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(),
            #axis.ticks.x=element_blank(), axis.text.x=element_blank(),
            axis.title.x=theme_text(hjust=1),
             plot.margin=unit(c(0.5, 0.05, 0, 0.5), "cm"), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), legend.position=c(0.18,.84), legend.title=element_blank())
pq1

p <- ggplot(df1, aes(x=theta, linetype=type))
pq2 <- p + geom_density() + facet_wrap(~facet, nrow=1) + scale_y_continuous("distribution density") +
        scale_x_continuous("Ideology Estimates", breaks=seq(-3,3,1), limits=c(-3.5, 3.5)) + theme_bw() +
        scale_linetype_discrete("Group") + 
        theme(axis.line.y=element_blank(), axis.title.y=element_blank(),
            axis.ticks.y=element_blank(), axis.text.y=element_blank(),
            #axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
            axis.title.x=theme_text(hjust=0), 
             plot.margin=unit(c(0.5, 0.5, 0, -.45), "cm"), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), legend.position=c(0.16,.79), legend.title=element_blank())
pq2

pdf("plots/figure4.pdf", height=3, width=9)
grid.arrange(arrangeGrob(pq1, pq2, ncol=2, heights=c(0.55, 0.45)))
dev.off()


#==============================================================================
# Analysis of ideology by state
#==============================================================================

# loading public opinion data
load("data/state-data.rdata")

# loading user data
load("output/users-data-US.rdata")

# normalizing estimates
users$theta <- (users$theta - mean(users$theta))/sd(users$theta)

# keeping only users with a state
users <- users[users$state %in% state.name,]

# computing confidence intervals for each state
med <- aggregate(users$theta, by=list(state=users$state), median)
lo <- aggregate(users$theta, by=list(state=users$state), quantile, prob=0.025)
hi <- aggregate(users$theta, by=list(state=users$state), quantile, prob=0.975)

df <- merge(med, lo, by="state")
df <- merge(df, hi, by="state")
names(df)[2:4] <- c("value", "lo", "hi")

# merging with state-level data
state.data <- merge(state.data, df)

## correlations reported in paper
cor(state.data$value, state.data$opinion)
cor(state.data$value, state.data$obama)













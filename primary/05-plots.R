#==============================================================================
# 05-plots.R
# Purpose: generate figures with results.
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
library(Matrix)
library(ggplot2)
library(scales)
library(ggthemes)
library(MASS)
setwd("~/git/twitter_ideology/primary")
resultsfile <- 'output/ca-results-US.rdata'

# loading results
load(resultsfile)

##############################################################
## MERGING WITH DATA
##############################################################

# merging with data
users <- read.csv('data/accounts-twitter-data.csv', stringsAsFactors=F)
users <- merge(users, data.frame(
	twitter=tolower(res$colnames), phi1=res$colcoord[,1], 
	phi2=res$colcoord[,2], phi3=res$colcoord[,3], 
	phi1sq = res$colcoord[,1]^2, phi2sq=res$colcoord[,2]^2, stringsAsFactors=F))

##############################################################
## PROJECTING INTO SAME LATENT SPACE AS ROLL-CALL VOTES
##############################################################

# reading Jackman's ideal point data
house <- read.csv("data/house.csv", stringsAsFactors=F); house$chamber <- "House"
senate <- read.csv("data/senate.csv", stringsAsFactors=F); senate$chamber <- "Senate"
ideal <- rbind(house[,c("nameid", "idealPoint", "chamber")], 
			senate[,c("nameid", "idealPoint", "chamber")])
names(ideal) <- c("bioid", "ideal", "chamber")

# I will project to the same latent space as Clinton et al's ideal points 
# First, merging with my dataset and finding projection coefficients
users <- merge(users, ideal, all.x=TRUE)
summary(reg <- lm(ideal ~ phi1 + phi2 + phi3 + phi1sq + phi2sq, data=users))

# > round(coef(reg), 4)
# (Intercept)        phi1        phi2        phi3   I(phi1^2)   I(phi2^2) 
#      0.4022      0.6468      0.4206      0.1213     -0.3276      0.0961 

# > vcov(reg)
#               (Intercept)          phi1          phi2          phi3        phi1sq        phi2sq
# (Intercept)  0.0021045562 -0.0002213181 -4.235702e-04  2.707149e-04 -9.342480e-04 -0.0002952230
# phi1        -0.0002213181  0.0026013804  2.335350e-05 -3.249450e-04  1.760855e-03  0.0002129782
# phi2        -0.0004235702  0.0000233535  7.984063e-04  3.089653e-05  7.959438e-05 -0.0002869526
# phi3         0.0002707149 -0.0003249450  3.089653e-05  5.423160e-04 -5.495912e-04  0.0000734796
# phi1sq      -0.0009342480  0.0017608550  7.959438e-05 -5.495912e-04  1.689390e-03  0.0001910040
# phi2sq      -0.0002952230  0.0002129782 -2.869526e-04  7.347960e-05  1.910040e-04  0.0003330533

# projecting to ideal point latent space
users$twscore <- as.matrix(cbind(1, 
		users[,c("phi1", "phi2", "phi3", "phi1sq", "phi2sq")])) %*% coef(reg)
res$twscore <- as.matrix(cbind(1, res$rowcoord, 
		res$rowcoord[,1]^2, res$rowcoord[,2]^2)) %*% coef(reg)

##############################################################
## COMPARISON WITH IDEAL POINTS
##############################################################

# high -- not surprising, since I'm projecting them
cor(users$ideal, users$twscore, use='complete.obs') # 0.95
# correlation with first dimension is 0.80

p <- ggplot(users[!is.na(users$ideal),], aes(y=ideal, x=twscore, label=party))
pq <- p + geom_point(aes(color=party), size=1.3) +
        facet_grid(. ~ chamber) + 
        scale_x_continuous("Estimated Twitter Ideal Points", limits=c(-2.5, 2.5)) +
        scale_y_continuous("Ideology Estimates Based on Roll-Call Votes\n(Clinton et al, 2004)",
            limits=c(-2.5, 2.5)) +
        theme(panel.border=element_rect(fill=NA), panel.background = element_blank(), 
            legend.position="none") 
pq

c1 <- round(cor(users$ideal[users$chamber=="House" & users$party=="Democrat"], 
    users$twscore[users$chamber=="House" & users$party=="Democrat"], use='complete.obs'), 2)
c2 <- round(cor(users$ideal[users$chamber=="House" & users$party=="Republican"], 
    users$twscore[users$chamber=="House" & users$party=="Republican"], use='complete.obs'), 2)
c3 <- round(cor(users$ideal[users$chamber=="Senate" & users$party=="Democrat"], 
    users$twscore[users$chamber=="Senate" & users$party=="Democrat"], use='complete.obs'), 2)
c4 <- round(cor(users$ideal[users$chamber=="Senate" & users$party=="Republican"], 
    users$twscore[users$chamber=="Senate" & users$party=="Republican"], use='complete.obs'), 2)

a1 <- data.frame(twscore = -1.2, ideal=-2.5, chamber=factor("House",levels=c("House","Senate")), 
            party=as.character(paste0("rho[D]==", sprintf('%0.2f', c1))))
a2 <- data.frame(twscore = 1.8, ideal=2.2, chamber=factor("House",levels=c("House","Senate")), 
            party=as.character(paste0("rho[R]==", sprintf('%0.2f', c2))))
a3 <- data.frame(twscore = -1.2, ideal=-2.2, chamber=factor("Senate",levels=c("House","Senate")), 
            party=as.character(paste0("rho[D]==", sprintf('%0.2f', c3))))
a4 <- data.frame(twscore = 1.8, ideal=2.2, chamber=factor("Senate",levels=c("House","Senate")), 
            party=as.character(paste0("rho[R]==", sprintf('%0.2f', c4))))
a <- rbind(a1, a2, a3, a4)

pq <- pq + geom_text(data=a, size=3, parse=TRUE)
pq

brds <- data.frame(
    twscore = rep(c(-2, 2), 2),
    ideal = c(-2, 2, NA, NA),
    party = "Democrat", chamber = c("House", "House", "Senate", "Senate"),
    stringsAsFactors=F)


pq2 <- pq + 
    geom_rangeframe(sides="bl", data=brds) + theme_tufte() + 
    theme(text=element_text(size=12, family="Palatino")) + 
    scale_color_manual(name="Political Party", values=c("blue", "red"), guide="none")

pq2

ggsave(pq2, file="img/twitter-congress.png", height=4, width=7)
ggsave(pq2, file="img/twitter-congress.pdf", height=4, width=7)


##############################################################
### VISUALIZING TWEETSCORES 
##############################################################

tt <- users[users$type %in% c("Media Outlets"),
	c("screen_name","twscore")]
tt[order(tt$twscore),]

(med.rep <- mean(users$twscore[users$party=="Republican" & users$type=="Congress"]))
(med.dem <- mean(users$twscore[users$party=="Democrat" & users$type=="Congress"]))

toinclude <- which(users$type %in% c("Primary Candidate","Pres. Obama"))
more <- which(users$twitter %in% c("motherjones", "msnbc", "nytimes", 
	"foxnews", "rushlimbaugh", "senwarren", "reppaulryan", "drudge_report", 
	"wsj", "washingtonpost"))
tt <- users[c(toinclude, more),
	c("screen_name","twscore", "type", "party")]
tt <- tt[order(tt$twscore, decreasing=TRUE),]
tt$party[is.na(tt$party)] <- "Z"

## standard errors
tt$twscore.sd <- NA
n.sims <- 100
coefs <- mvrnorm(n.sims, coef(reg), vcov(reg))

for (i in 1:nrow(tt)){
	account <- tt$screen_name[i]
	cat(account, '\n')
	fl <- names(which(y[,colnames(y)==account]))
	# compute points
	points <- (res$rownames %in% fl)*1
	# project points
	phi <- supplementaryColumns(res, points)
	values <- cbind(1, phi, phi[,1]^2, phi[,2]^2) %*% t(coefs)
	tt$twscore.sd[i] <- sd(values)
}

## adding '@' signs
tt$screen_name <- paste0('@', tt$screen_name)
tt$screen_name <- factor(tt$screen_name, levels=tt$screen_name)

p <- ggplot(tt, aes(y=screen_name, x=twscore, color=party))
pq <- p + geom_point(size=1.75) + 
    geom_segment(aes(y=screen_name, yend=screen_name, 
    	x=twscore-1.96*twscore.sd, xend=twscore+1.96*twscore.sd)) +
    scale_x_continuous(
        "Position on latent ideological scale", lim=c(-2.50, 2.25),
        breaks=c(-2, -1, 0, 1, 2)) +
    geom_text(data=tt[tt$twscore<0,], 
    	aes(label=screen_name, x=twscore-1.96*twscore.sd-.025), hjust=1, 
    	vjust=0.5, size=2.5, family="Palatino") +
	geom_text(data=tt[tt$twscore>0,], 
    	aes(label=screen_name, x=twscore+1.96*twscore.sd+.025), hjust=0, 
    	vjust=0.5, size=2.5, family="Palatino") +
    scale_color_manual(guide="none", values=c("blue", "red", "black")) +
    theme_tufte() +
    theme(axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        text=element_text(family="Palatino")) +
    geom_vline(xintercept=med.rep, color="red", alpha=1/5) +
    annotate(x=med.rep, y=nrow(tt)-2, label="Average Republican\nin 114th Congress", 
    	angle=90, geom="text", hjust=1, vjust=-0.2, size=2.5, alpha=0.90) +
    geom_vline(xintercept=0, color="black", alpha=1/5) +
    annotate(x=0, y=2, label="Average Twitter User", angle=90, 
    	geom="text", hjust=0, vjust=-0.2, size=2.5, alpha=0.90) +
    geom_vline(xintercept=med.dem, color="blue", alpha=1/5) +
   	annotate(x=med.dem, y=2, label="Average Democrat\nin 114th Congress", angle=90, 
    	geom="text", hjust=0, vjust=-0.2, size=2.5, alpha=0.90) +
    geom_rangeframe(sides="b", data=data.frame(twscore=c(-2, 2), screen_name=NA, party="Z"))


pq

ggsave(pq, file="img/primary-candidates.png", height=4.5, width=7)
ggsave(pq, file="img/primary-candidates.pdf", height=4.5, width=7)

n.voters <- 10000
load(matrixfile)

## projecting and normalizing to mean=0 and sd=1
set.seed(12345)
suppl <- supplementaryRows(res, as.matrix(y[sample(1:nrow(y), n.voters),]))
suppl <- cbind(1, suppl, suppl[,1]^2, suppl[,2]^2) %*% coef(reg)
suppl <- (suppl - mean(suppl)) / sd(suppl)

d <- data.frame(
	ideology = c(
		users$twscore[users$party=="Republican"],
		users$twscore[users$party=="Democrat"],
		#sample(res$twscore, n.voters)),
		suppl),
	type = c(
		users$party[users$party=="Republican"],
		users$party[users$party=="Democrat"],
		rep("Z", n.voters)))
d <- d[!is.na(d$ideology),]
levels(d$type) <- c("Democrat", "Republican", "Z")

pq2 <- pq +
	geom_density(data=d, aes(x=ideology, fill=type, y=..density..*20, color=type),
		alpha=0.25, color=NA) +
	scale_fill_manual(guide="none", values=c("blue", "red", "black")) +
	ggtitle("Twitter ideology scores of potential Democratic and Republican presidential primary candidates") +
	theme(plot.title=element_text(size = rel(.75), vjust=1.5, face="bold")) +
	theme(plot.margin=unit(c(0.30, 0.20, 0.65, .20), "cm"))
pq2


ggsave(pq2, file="img/primary-candidates-densities.pdf", height=4.5, width=5.5)

prop.table(table(suppl<tt$twscore[tt$screen_name=="SenSanders"]))

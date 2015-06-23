#==============================================================================
# 08-analysis-section-4-3
# Purpose: comparison of individual-level estimates with campaign contribution
# records, and with voter registration history (Ohio)
# Author: Pablo Barbera
#==============================================================================

source('functions.R')
library(ggplot2)
library(scales)
library(grid)

#==============================================================================
# Analysis of contributor data
#==============================================================================

## loading user estimates and data
load("output/users-data-US.rdata")

## loading Ohio data
load("data/contributor-data.rdata")

## merging
contr <- merge(contr, users)

## category
contr$cat <- "Both parties"
contr$cat[contr$amount_dem>0 & contr$amount_rep==0] <- "Only Democrats"
contr$cat[contr$amount_dem==0 & contr$amount_rep>0] <- "Only Republicans"

## quantities reported in paper
table(contr$theta>0)
tab <- table(contr$theta>0, contr$cat)
(tab["TRUE", "Both parties"] + tab["TRUE", "Only Republicans"]) / sum(tab["TRUE",])
(tab["FALSE", "Both parties"] + tab["FALSE", "Only Democrats"]) / sum(tab["FALSE",])
cor(contr$theta, contr$cfscore, use='complete.obs')

#==============================================================================
# Figure 5: ideology estimates and party registration history in Ohio
#==============================================================================

## loading user estimates and data
load("output/users-data-US.rdata")

## loading Ohio data
load("data/ohio-data.rdata")

## merging
ohio <- merge(ohio.data, users)

## preparing variables
ohio$party <- "Not registered"
ohio$party[ohio$PARTY_AFFILIATION=="D"] <- "Registered DEM"
ohio$party[ohio$PARTY_AFFILIATION=="R"] <- "Registered REP"

sum.regs <- apply(ohio[,4:42], 1, function(x) sum(x %in% "D") - sum(x %in% "R"))
ohio$sum.regs <- sum.regs

### categorical variable times registered for each party
ohio$regs.cat <- NA
ohio$regs.cat[ohio$sum.regs<(-5)] <- "<-5"
ohio$regs.cat[ohio$sum.regs<=(-3) & ohio$sum.regs>=(-5)] <- "[-3,-5]"
ohio$regs.cat[ohio$sum.regs==(-2)] <- "-2"
ohio$regs.cat[ohio$sum.regs==(-1)] <- "-1"
ohio$regs.cat[ohio$sum.regs==(0)] <- "0"
ohio$regs.cat[ohio$sum.regs==(2)] <- "+2"
ohio$regs.cat[ohio$sum.regs==(1)] <- "+1"
ohio$regs.cat[ohio$sum.regs>(5)] <- ">+5"
ohio$regs.cat[ohio$sum.regs>=(3) & ohio$sum.regs<=(5)] <- "[+3,+5]"
ohio$regs.cat <- factor(ohio$regs.cat, levels=c("<-5", "[-3,-5]", "-2",
    "-1", "0", "+1", "+2", "[+3,+5]", ">+5"))

## combining both datasets before plotting
plotdata <- ohio[,c("uid", "theta", "regs.cat")]
names(plotdata) <- c("uid", "estimates", "x")
plotdata$facet <- "Registration History"

plotdata <- rbind(plotdata, data.frame("uid"=ohio$uid, "estimates"=ohio$theta, x=ohio$party,
    facet="2012"))
plotdata <- plotdata[plotdata$x %in% "Not registered" == FALSE,]
levels(plotdata$x)[c(11,12)] <- c("Dem.", "Rep.")

### figure 5

p <- ggplot(plotdata, aes(x=x, y=estimates))
pq <- p + geom_boxplot(outlier.colour="grey", outlier.size=1) +
    scale_x_discrete("  Party                (# elections registered Dem. - # elections registered Rep.)") + 
    scale_y_continuous(expression(paste(theta[i], ", Twitter-Based Ideology Estimates")), limits=c(-2.5, 2.5)) +
        theme(panel.border=element_rect(fill=NA), panel.background = element_blank(), legend.position="none") +
    geom_hline(aes(yintercept=0), linetype=3) +
    facet_grid(~facet,scales="free", space="free")

pq

ggsave(filename="plots/figure5.pdf", plot=pq, 
        height=3.5, width=7)












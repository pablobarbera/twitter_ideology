#==============================================================================
# 06-analysis-section-4-1
# Purpose: validation of model results for legislators and parties
# Author: Pablo Barbera
#==============================================================================

source('functions.R')
library(ggplot2)
library(scales)
library(grid)

#==============================================================================
# Figure 1: US Congress
#==============================================================================

# loading elite data
load("data/elites-data.Rdata")
# loading elite estimates
load("output/results-elites-US.rdata")
results$merge <- tolower(results$screen_name)
# merging data
d <- merge(elites.data[['US']], results, by="merge")

# normalizing elite estimates
load("output/estimates-US.rdata")
d$phi <- (d$phi - mean(results$theta)) / sd(results$theta)

## keeping only congress
d <- d[d$title %in% c("House", "Senate"),]

## computing correlation coefficients reported in paper
sum(table(d$title[!is.na(d$dw.nom.1)]))
cor(d$phi[d$title=="House"], d$dw.nom.1[d$title=="House"], use='complete.obs')
cor(d$phi[d$title=="Senate"], d$dw.nom.1[d$title=="Senate"], use='complete.obs')
cor(d$phi[d$party=="R"], d$dw.nom.1[d$party=="R"], use='complete.obs')
cor(d$phi[d$party=="D"], d$dw.nom.1[d$party=="D"], use='complete.obs')


## correlations from footnote 17
sum(table(d$title[!is.na(d$idealPoint)]))
cor(d$phi[d$title=="House"], d$idealPoint[d$title=="House"], use='complete.obs')
cor(d$phi[d$title=="Senate"], d$idealPoint[d$title=="Senate"], use='complete.obs')
cor(d$phi[d$party=="R"], d$idealPoint[d$party=="R"], use='complete.obs')
cor(d$phi[d$party=="D"], d$idealPoint[d$party=="D"], use='complete.obs')

## deleting missing values
d <- d[!is.na(d$phi) & !is.na(d$dw.nom.1),]


# correlations for figure 1
c1 <- round(cor(d$phi[d$title=="House" & d$party=="D"], 
    d$dw.nom.1[d$title=="House" & d$party=="D"]), 2)
c2 <- round(cor(d$phi[d$title=="House" & d$party=="R"], 
    d$dw.nom.1[d$title=="House" & d$party=="R"]), 2)
c3 <- round(cor(d$phi[d$title=="Senate" & d$party=="D"], 
    d$dw.nom.1[d$title=="Senate" & d$party=="D"]), 2)
c4 <- round(cor(d$phi[d$title=="Senate" & d$party=="R"], 
    d$dw.nom.1[d$title=="Senate" & d$party=="R"]), 2)

a1 <- data.frame(dw.nom.1 = -0.9, phi=-2, title=factor("House",levels=c("House","Senate")), 
            party=as.character(paste0("rho[D]==", c1)))
a2 <- data.frame(dw.nom.1 = 0.9, phi=2.2, title=factor("House",levels=c("House","Senate")), 
            party=as.character(paste0("rho[R]==", c2)))
a3 <- data.frame(dw.nom.1 = -0.9, phi=-2, title=factor("Senate",levels=c("House","Senate")), 
            party=as.character(paste0("rho[D]==", c3)))
a4 <- data.frame(dw.nom.1 = 0.9, phi=2.2, title=factor("Senate",levels=c("House","Senate")), 
            party=as.character(paste0("rho[R]==", c4)))
a <- rbind(a1, a2, a3, a4)

a5 <- data.frame(dw.nom.1 = -0.55, phi=1.40, title=factor("House",levels=c("House","Senate")), 
            party="A. Davis (now R)")
a6 <- data.frame(dw.nom.1 = -0.30, phi=-2.15, title=factor("Senate",levels=c("House","Senate")), 
            party="B. Sanders")
a7 <- data.frame(dw.nom.1 = -0.60, phi=.9, title=factor("Senate",levels=c("House","Senate")), 
            party="J. Lieberman")

a2 <- rbind(a5, a6, a7)

a8 <- data.frame(y =-0.50 , yend = -0.32, x = 1.5, xend = 1.70, party=NA, title=factor("House",levels=c("House","Senate")))
a9 <- data.frame(y =-0.34 , yend = -0.52, x = -2.2, xend = -2, party=NA, title=factor("Senate",levels=c("House","Senate")))
a10 <- data.frame(y =-0.52 , yend = -0.26, x = .78, xend = .58, party=NA, title=factor("Senate",levels=c("House","Senate")))

a3 <- rbind(a8, a9, a10)

p <- ggplot(d, aes(y=dw.nom.1, x=phi, label=party))
pq <- p + geom_text(size=2.25) + facet_grid(. ~ title) + 
        scale_x_continuous(expression(paste(phi[j], ", Estimated Twitter Ideal Points")), limits=c(-2.5, 2.5)) +
        scale_y_continuous("DW-NOMINATE scores (1st dim.)", 
            limits=c(-1,1), breaks=seq(-1,1,.5)) +
        #scale_color_manual(name="Political Party", values=c("blue", "purple", "red")) +
        geom_text(data=a, size=3, parse=TRUE) + geom_text(data=a2, size=2.5) + 
        geom_segment(data=a3, aes(x=x, xend=xend, y=y, yend=yend), size=0.25, 
            arrow=arrow(length=unit(0.2,"cm")),show_guide=F) + 
        theme(panel.border=element_rect(fill=NA), panel.background = element_blank(), legend.position="none")
pq

ggsave(filename="plots/figure1.pdf", plot=pq, 
        height=3, width=6)


#==============================================================================
# Figure 2: Key elites in the US
#==============================================================================

# loading elite data
load("data/elites-data.Rdata")
# loading elite estimates
load("output/results-elites-US.rdata")
results$merge <- tolower(results$screen_name)
# merging data
d <- merge(elites.data[['US']], results, by="merge")
# loading samples to compute standard errors
load("output/samples-US.rdata")

# normalizing elite estimates
load("output/estimates-US.rdata")
d$phi <- (d$phi - mean(results$theta)) / sd(results$theta)
d <- d[order(d$phi),]

# finding median legislators
housedem <- median(d$phi[d$title=="House" & d$party=="D"])
houserep <- median(d$phi[d$title=="House" & d$party=="R"])
sendem <- median(d$phi[d$title=="Senate" & d$party=="D"])
senrep <- median(d$phi[d$title=="Senate" & d$party=="R"])

housedem <- which.min(abs(d$phi - housedem))
houserep <- which.min(abs(d$phi - houserep))
sendem <- which.min(abs(d$phi - sendem))
senrep <- which.min(abs(d$phi - senrep))

# preparing plot data
d <- rbind(
    d[c(housedem, houserep, sendem, senrep),],
    d[d$merge %in% c("maddow", "mmflint", "hrc", "barackobama",
        "nytimes", "jonhuntsman", "mittromney", "foxnews",
        "glennbeck", "limbaugh"),])

target <- c("Median House D.", "Median House R.", "Median Senate D.", "Median Senate R.", "@Maddow", 
    "@MMFlint", "@HRC", "@BarackObama", "@nytimes", "@JonHuntsman", "@MittRomney",
    "@foxnews", "@GlennBeck", "@limbaugh")
d2 <- data.frame(med = d$phi, lo=d$phi-2*d$phi.sd, up=d$phi+2*d$phi.sd, target=target, 
    party=c(1, 2, 1, 2, 3,
        3, 3, 1, 3, 2, 2, 
        3, 3, 3))

d2$x <- 0
d2 <- d2[order(d2$med),]

p <- ggplot(data=d2, aes(y=med, x=x, shape=factor(party)))
pq2 <- p + geom_linerange(width=.5, aes(ymin=lo, ymax=up), color="grey20") + 
        geom_point(aes(shape=factor(party))) + 
        scale_x_continuous(limits=c(-0.07,0.06), expand=c(0,0)) + 
            scale_y_continuous(expression(paste(phi[j], ", Estimated Twitter Ideal Points")), limits=c(-2.1, 2.4)) +
            theme_bw() + coord_flip() +
            scale_shape_manual(name="Political Party", values=c(16, 15, 4), 
                labels=c("Democrat",  "Republican", "Nonpartisan")) +
        theme( legend.position = "bottom", legend.direction = "horizontal",
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        panel.border = element_blank(), 
        panel.grid = element_blank(),
        legend.margin=unit(-0.75, "cm"),
        plot.margin=unit(c(0.30, 0.75, 0.10, 1), "cm")) +
        geom_vline(xintercept=0, alpha=1/3, size=0.2)

pq2

add <- rep_len(c(-0.03, 0.03, -0.05, 0.05), 14) 
d2$x2 <- d2$x + add

arrows <- data.frame(y = d2$med, yend = d2$med, x = add - add/4, xend = add/5, party=1)

pq2 <- pq2 + geom_text(data=d2, aes(label=target, x=x2, y=med), size=2.1) +
    geom_segment(data=arrows, aes(x=x, xend=xend, y=y, yend=yend), size=0.25,
        arrow=arrow(length=unit(0.1,"cm"))) +
    geom_vline(xintercept=-0.07, colour="black", size=0.75)

pq2 

ggsave(filename="plots/figure2.pdf", plot=pq2, 
        height=2, width=6)


#==============================================================================
# Figure 3: ideological locations of parties in Europe
#==============================================================================

# loading elite data
load("data/elites-data.Rdata")
# countries in study
countries <- names(elites.data)[2:6]
df <- list()

for (country in countries){
   
    elites.data[[country]]$merge <- tolower(elites.data[[country]]$screen_name)

    # loading elite estimates
    load(paste0("output/results-elites-", country, ".rdata"))
    results$merge <- tolower(results$screen_name)

    # normalizing for comparative comparison
    results$phi <- (results$phi - mean(results$phi))/sd(results$phi)

    # merging data
    d <- merge(elites.data[[country]], results, by="merge")
    d$country <- country

    df[[country]] <- d

}

df <- do.call(rbind, df)
df$sd <- 0
df$source <- "Twitter Estimates"
df <- df[,c("phi", "sd", "party", "source", "country")]

## expert survey data
parties <- c("conservatives", "labour", "libdems",
            "PSOE", "PP", "IU", "UPyD",
            "CDA", "PVDA", "VVD", "D66", "GL", "SP", "CU", "PVV",
            "CDU", "CSU", "FDP", "GREEN", "SPD", "LINKE",
            "LN", "UDC", "PDL", "IDV", "PD", "MA", "PCI")
countries <- c(
    rep("UK", 3), rep("spain", 4), rep("NL", 8),
    rep("germany", 6), rep("italy", 7))

exp <- data.frame(
    phi = c(
            7.13, 4, 4, #UK
            3.66, 7.33, 1.83, 5.45, #spain
            6.28, 3.85, 7.85, 5, 2.57, 1.64, 5.35, 8.61, #NL
            6.1, 7.1, 6.6, 3.6, 3.6, 1.3, # germany
            8.55, 5.33, 7.55, 4, 3.22, 6.28, 0.8), # italy
    sd = c(
        0.63, 0.75, 1, #UK
        0.65, 0.65, 0.71, 1.57, #spain
        .9138736,.5345225,.5345225,.877058,.8516306,.7449464,1.008208,1.445595, # NL
        0.6, 1.0, 1.2, 1.4, 0.5, 0.6, #germany
        .881, .707, .882, 1.19, .666, 1.113, .447), #italy
    party = parties, source = 'Expert Survey', country = countries,
    stringsAsFactors=F)

df <- rbind(df, exp)
df <- df[tolower(df$party) %in% tolower(parties),]

## converting to factors in order to order parties in plot
df$party[df$party=='psoe'] <- "PSOE"
df$party[df$party=='pp'] <- "PP"
df$party[df$party=='iu'] <- "IU"
df$party[df$party=='upyd'] <- "UPyD"

## computing medians of Twitter estimates, by party
tw <- df[df$source=="Twitter Estimates",]
tw <- aggregate(tw$phi, by=list(party=tw$party, country=tw$country), median)
tw$xend <- tw$x
tw$source <- factor("Twitter Estimates", 
    levels=c("Twitter Estimates", "Expert Survey"))
tw$country <- factor(tw$country, levels=unique(tw$country))
levels(tw$country) <- c("Germany", "Italy", "Netherlands", "Spain", "UK")


df$party <- factor(df$party, levels=tw$party[order(tw$x, decreasing=TRUE)])
df$country <- factor(df$country, levels=unique(countries))
levels(df$country) <- c("UK", "Spain", "Netherlands", "Germany", "Italy")
df$source <- factor(df$source, levels=c("Twitter Estimates", "Expert Survey"))

p <- ggplot(df, aes(y=party, x=phi))
pq <- p + geom_point(size=1) + geom_segment(aes(x=phi-sd, xend=phi+sd, y=party, 
    yend=party)) +
    scale_x_continuous(expression(paste(phi[j], 
        ", Estimated Twitter Ideal Points      Left-Right Dimension             "))) +
    theme_bw() + scale_y_discrete("") + 
    facet_grid(country~source, scales="free", space="free_y") +
    theme(axis.title.x = element_text(size = rel(0.8)))
pq


tw <- tw[order(tw$country, tw$x),]
tw$p <- c(6:1, 7:1, 8:1, 4:1, 3:1)
tw$y <- as.numeric(tw$p) - 0.25
tw$yend <- as.numeric(tw$p) + 0.25


pq <- pq + geom_segment(data=tw, aes(x=x, xend=xend, y=y, yend=yend), size=0.25)
pq

ggsave(filename="plots/figure3.pdf", plot=pq, height=6, width=5)





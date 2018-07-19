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
accountstable <- "phi201807"
usertable <- 'theta201807'

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
phi <- -res$colcoord[,1]
congress <- merge(congress, data.frame(
  merge=tolower(res$colnames), phi=phi, stringsAsFactors=F))
congress$phi <- as.numeric(scale(congress$phi))

# who is on the extremes
head(congress[order(congress$phi),])
tail(congress[order(congress$phi),])

# primary candidates
congress <- congress[order(congress$phi),]
congress[congress$type=="Primary Candidate",c("screen_name", "phi")]

# NOTE: this is before applying user-level normalization (adding ~ 0.5)

#screen_name         phi
#11    BernieSanders -1.27540314
#77   HillaryClinton -1.10497970
#99       JohnKasich -0.02749425
#149 realDonaldTrump -0.01011567
#110      marcorubio  0.32876053
#88          JebBush  0.41543770
#68      GovChristie  0.55228030
#572         tedcruz  1.10278576

# others
congress[congress$type=="Media Outlets",c("screen_name", "phi")]
congress[congress$type=="Journalists",c("screen_name", "phi")]
#screen_name        phi
#28      chrislhayes -1.5370875
#50         donlemon -1.4264065
#109          maddow -1.3843445
#6    andersoncooper -1.0924280
#73  GStephanopoulos -0.8383425
#119      megynkelly  0.7819207
#482     seanhannity  0.9494941
#581   TuckerCarlson  0.9797686
#83    IngrahamAngle  1.1727488
#7        AnnCoulter  1.2565500
#58    foxandfriends  1.4839764
#65        glennbeck  1.5107759

congress[congress$type=="Other Politicians",c("screen_name", "phi")]

#screen_name         phi
#560       SenWarren -1.51355067
#550      SenSchumer -1.49212624
#96         JoeBiden -1.43535741
#5            algore -1.27179303
#576    TheDemocrats -1.18507262
#47             dccc -1.13668338
#13      BillClinton -1.10955029
#486      SenateDems -1.10269912
#78   HouseDemocrats -1.03231400
#146           POTUS -0.83468155
#488    SenateMajLdr -0.52183055
#594              VP -0.04465312
#61     GeorgeHWBush  0.13583998
#124      MittRomney  0.48279692
#121      mike_pence  0.56789938
#79         HouseGOP  0.66562438
#487       SenateGOP  0.70657699
#66              GOP  0.77878266
#69  GovMikeHuckabee  0.86950686
#130    newtgingrich  1.16418129
#481   SarahPalinUSA  1.23113022

# validation
library(dplyr)
dwnom <- readr::read_csv(paste0(dropbox, 'tweetscores/HS115_members.csv'))
dwnom <- merge(dwnom, congress[,c("bioid", "phi")], 
               by.x='bioguide_id', by.y='bioid')

dwnom %>% 
  group_by(congress, chamber, party_code) %>% 
  summarize(cor=cor(nominate_dim1, phi), n=n())

#   congress chamber      party cor(ideal, phi)
#1      115   House        100 0.4086655   190
#2      115   House        200 0.3741613   229
#3      115  Senate        100 0.5694423    45
#4      115  Senate        200 0.3474372    48
#5      115  Senate        328 1.0000000     2

library(ggplot2)
p <- ggplot(dwnom, aes(x=nominate_dim1, y=phi, color=factor(party_code)))
pq <- p + geom_point() + facet_wrap(~ chamber) +
  scale_color_manual("Party", values=c("blue", "red", "green"),
                     labels=c("D", "R", "I"))
pq
ggsave(pq, file="plots/01-congress-nominate.pdf", height=5, width = 10)

p <- ggplot(dwnom[dwnom$party_code!=328,], aes(x=phi, fill=factor(party_code)))
pq <- p + geom_density(alpha=1/2) + facet_wrap(~ chamber) +
  scale_fill_manual("Party", values=c("blue", "red"),
                    labels=c("D", "R")) +
  theme_minimal()
pq

ggsave(pq, file="plots/02-congress-party.pdf", height=5, width = 10)

#==============================================================================
## UPLOADING ACCOUNT-LEVEL DATA
#==============================================================================

library(bigrquery)
project <- "usc-barbera"
set_service_token(paste0(dropbox, "credentials/bigquery-token.json"))

# preparing political accounts table
tab <- bq_table("usc-barbera", "tweetscores", accountstable)


# uploading data
df <- congress[!is.na(congress$phi),c("merge", "phi", "screen_name",
                                            "bioid", "gender", "name",
                                            "type", "party", "chamber")]
bq_table_upload(tab, df)

#==============================================================================
## COMPUTING USER-LEVEL IDEAL POINTS
#==============================================================================

users <- query_exec(paste0(
  "WITH user_scores AS (SELECT x.id_str, 
      ROUND(AVG(y.phi),3) AS theta, 
      COUNT(y.phi) AS accounts_followed
  FROM `usc-barbera.tweetscores.", tabname, "` x
  JOIN `usc-barbera.tweetscores.", accountstable, "` y
  ON LOWER(x.account) = y.merge
  GROUP BY x.id_str
  ),
  factors AS (SELECT 
    AVG(theta) AS mean_theta,
    STDDEV_POP(theta) AS sd_theta
  FROM user_scores )
  SELECT x.id_str,
    (x.theta - y.mean_theta) / y.sd_theta AS theta,
    x.accounts_followed
  FROM user_scores x
  CROSS JOIN factors y
  "), 
  project=project, use_legacy_sql=FALSE, 
  destination_table=list(project_id=project, dataset_id='tweetscores', 
                         table_id=usertable))

# sanity check
users <- query_exec(paste0(
  "SELECT x.id_str, x.theta, x.accounts_followed, y.party
  FROM `usc-barbera.tweetscores.", usertable, "` x
  JOIN `usc-barbera.tweetscores.voter_data` y
  ON x.id_str = y.id"), project=project, use_legacy_sql=FALSE,
  max_pages=Inf)

users <- users[users$party!=999,]

p <- ggplot(users[users$accounts_followed>=3,], 
            aes(x=theta, fill=factor(party)))
pq <- p + geom_density(alpha=1/2) + facet_wrap(~factor(party), nrow=3) +
  scale_fill_manual("Party", values=c("green", "blue", "red"),
                    labels=c("U", "D", "R")) +
  theme_minimal()
pq
ggsave(pq, file="plots/03-users-party.pdf", height=10, width = 10)

p <- ggplot(users[users$accounts_followed>=3,], 
            aes(y=theta, x=factor(party), fill=factor(party)))
pq <- p + geom_boxplot(alpha=1/2) + 
  scale_fill_manual("Party", values=c("green", "blue", "red"),
                    labels=c("U", "D", "R")) +
  coord_flip() +
  theme_minimal()
pq
ggsave(pq, file="plots/04-users-party-boxplots.pdf", height=6, width = 10)


partisans <- users[users$party %in% c(1,2),]
cm <- table(partisans$theta>0, partisans$party==2)
sum(diag(cm))/sum(cm)
# 0.7507467

partisans <- users[users$party %in% c(1,2) & users$accounts_followed>=3,]
cm <- table(partisans$theta>0, partisans$party==2)
sum(diag(cm))/sum(cm)
# 0.7958996



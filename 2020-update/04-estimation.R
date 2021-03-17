#==============================================================================
# 04-estimation.R
# Purpose: produce ideology estimates. First, estimate with a subset of the
# adjacency matrix that follow 3+ MCs and a total of 10+ accounts; then for 
# the rest of users, and users in the sample for this project.
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
library(Matrix)

# params
folder <- 'followers-lists-202008'
usermatrix <- 'adjmatrix202008'
dropbox <- "~/Dropbox/"
resultsfile <- 'ca-results-202008'
accountstable <- "phi202008"
usertable <- 'theta202008'

#==============================================================================
## ESTIMATION: FIRST STAGE W/CORRESPONDENCE ANALYSIS
#==============================================================================

# loading data
load(paste0(dropbox, 'data/tweetscores/', usermatrix, '.rdata'))

## subsetting matrix: only those who follow 3+ MCs to help 
## identify latent ideological space
congress <- readr::read_csv(paste0(dropbox, 
                          "data/tweetscores/accounts-twitter-data-2020-08.csv"),
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

load(paste0(dropbox, 'data/tweetscores/', resultsfile, '.rdata'))
congress <- readr::read_csv(paste0(dropbox, 
                          "data/tweetscores/accounts-twitter-data-2020-08.csv"),
                            col_types = "ccccciiiccccccccccc")

# merging with data
congress$merge <- tolower(congress$screen_name)
phi <- res$colcoord[,1]
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

# screen_name        phi
# 596       TomSteyer -1.2026757
# 141   PeteButtigieg -1.1965718
# 55          ewarren -1.1819997
# 102    KamalaHarris -1.1573958
# 5      amyklobuchar -1.0920129
# 13    BernieSanders -1.0585352
# 7        AndrewYang -0.9509830
# 121   MikeBloomberg -0.8415985
# 78   HillaryClinton -0.6939551
# 100      JohnKasich -0.1354860
# 598    TulsiGabbard -0.1033044
# 147 realDonaldTrump  0.3326923
# 90          JebBush  0.3359531
# 68      GovChristie  0.5992298
# 110      marcorubio  0.6881177
# 590         tedcruz  1.2983209

# others
congress[congress$type=="Media Outlets",c("screen_name", "phi")]
congress[congress$type=="Journalists",c("screen_name", "phi")]
# 32      chrislhayes -1.1661471
# 49         donlemon -1.1551787
# 105        Lawrence -1.1426925
# 31       ChrisCuomo -1.0937716
# 109          maddow -1.0921411
# 6    andersoncooper -0.9196026
# 73  GStephanopoulos -0.7461003
# 118      megynkelly  0.9160389
# 8        AnnCoulter  1.2456165
# 499     seanhannity  1.2871363
# 597   TuckerCarlson  1.4180766
# 85    IngrahamAngle  1.4427985
# 57    foxandfriends  1.4928859

congress[congress$type=="Other Politicians",c("screen_name", "phi")]

# screen_name        phi
# 97         JoeBiden -1.0618896
# 4            algore -1.0380486
# 593    TheDemocrats -0.9780471
# 46             dccc -0.9510967
# 15      BillClinton -0.9353108
# 504      SenateDems -0.8815960
# 10      BarackObama -0.8761316
# 79   HouseDemocrats -0.8716406
# 144           POTUS -0.8030206
# 124      MittRomney  0.1454755
# 60     GeorgeHWBush  0.2011746
# 610              VP  0.4976321
# 506    senatemajldr  0.9448514
# 120      Mike_Pence  0.9715784
# 505       SenateGOP  1.0367884
# 80         HouseGOP  1.0756582
# 66              GOP  1.1127743
# 498   SarahPalinUSA  1.3600671
# 69  GovMikeHuckabee  1.4869665
# 129    newtgingrich  1.5073204

# validation
library(dplyr)
dwnom <- readr::read_csv(paste0(dropbox, 'data/tweetscores/HS116_members.csv'))
dwnom <- merge(dwnom, congress[,c("bioid", "phi")], 
               by.x='bioguide_id', by.y='bioid')

dwnom %>% 
  filter(party_code %in% c(100, 200)) %>% 
  group_by(congress, chamber, party_code) %>% 
  summarize(cor=cor(nominate_dim1, phi), n=n())

# 1      116 House          100 0.319   230
# 2      116 House          200 0.441   190
# 3      116 Senate         100 0.647    44
# 4      116 Senate         200 0.480    53

library(ggplot2)
p <- ggplot(dwnom, aes(x=nominate_dim1, y=phi, color=factor(party_code)))
pq <- p + geom_point() + facet_wrap(~ chamber) +
  scale_color_manual("Party", values=c("blue", "red", "green"),
                     labels=c("D", "R", "I")) +
  theme_bw()
pq
ggsave(pq, file="plots/01-congress-nominate.pdf", height=5, width = 10)

# who are the outliers?
dwnom %>% filter(phi>0 & nominate_dim1 < 0) %>% select(bioname)
# 1 VAN DREW, Jefferson --> became Republican in 2020
dwnom %>% filter(phi<0 & nominate_dim1 > 0) %>% select(bioname)
# 1     COLLINS, Susan Margaret
# 2             MURKOWSKI, Lisa
# 3 ROMNEY, Willard Mitt (Mitt)



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
bq_auth(path=paste0(dropbox, "credentials/bigquery-token.json"))

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

# all users
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

# users with 3+ accounts followed
users <- query_exec(paste0(
  "SELECT id_str, theta, accounts_followed
  FROM `usc-barbera.tweetscores.", usertable, "`
  WHERE accounts_followed >= 3"), 
  project=project, use_legacy_sql=FALSE, 
  destination_table=list(project_id=project, dataset_id='tweetscores', 
                         table_id=paste0(usertable, "_3plus")))

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
  theme_minimal() + theme(strip.text=element_blank())
pq
ggsave(pq, file="plots/03-users-party.pdf", height=10, width = 10)

p <- ggplot(users[users$accounts_followed>=3,], 
            aes(y=theta, x=factor(party), fill=factor(party)))
pq <- p + geom_boxplot(alpha=1/2) + 
  scale_fill_manual("Party", values=c("green", "blue", "red"),
                    labels=c("U", "D", "R")) +
  coord_flip() +
  theme_minimal() + theme(axis.title.y = element_blank(),
                          axis.text.y = element_blank())
pq
ggsave(pq, file="plots/04-users-party-boxplots.pdf", height=6, width = 10)


partisans <- users[users$party %in% c(1,2),]
cm <- table(partisans$theta>0, partisans$party==2)
sum(diag(cm))/sum(cm)
# 0.7499963

partisans <- users[users$party %in% c(1,2) & users$accounts_followed>=3,]
cm <- table(partisans$theta>0, partisans$party==2)
sum(diag(cm))/sum(cm)
# 0.7825525

partisans <- users[users$party %in% c(1,2) & users$accounts_followed>=10,]
cm <- table(partisans$theta>0, partisans$party==2)
sum(diag(cm))/sum(cm)
# 0.8224001

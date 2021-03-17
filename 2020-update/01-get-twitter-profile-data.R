#==============================================================================
# 01-get-twitter-data.R
# Purpose: download list of Twitter followers of politicians from Twitter API
# Details: follower lists are stored in 'outfolder' as .Rdata files
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
dropbox <- "~/Dropbox/"
outfolder <- 'followers-lists-202008/'
polsfile <- "accounts-twitter-data-2020-08.csv"
oauth_folder <- paste0(dropbox, 'credentials/twitter')

## scraping list of social media accounts for Members of the US Congress
## from 'unitedstates' GitHub account
congress <- scrapeCongressData()
# fixing one account manually
congress$chamber <- congress$type

## preparing to download follower lists
accounts <- congress$twitter[!is.na(congress$twitter)]

## adding major presidential candidates and politicians

# DEMOCRATS - 2016 and 2020 primaries
dems <- c('JoeBiden', 'BernieSanders', 'ewarren', 'MikeBloomberg',
          'PeteButtigieg', 'amyklobuchar', 'TulsiGabbard', 'TomSteyer',
          'AndrewYang', 'KamalaHarris', 'HillaryClinton'
          )

# REPUBLICANS: 2016 primary election
reps <- c("realDonaldTrump", "tedcruz", "JohnKasich",
          "JebBush", "marcorubio", "GovChristie")

# adding also major media outlets (Pew+others) in the US to help w/estimation
media <- c("EconUS", "BBCWorld", "NPR", "NewsHour", "WSJ", "ABC", 
           "CBSNews", "NBCNews", "CNN", "USATODAY", "theblaze", "nytimes", 
           "washingtonpost", "msnbc", "GuardianUS", "Bloomberg", "NewYorker", 
           "politico", "YahooNews", "FoxNews", "MotherJones", "Slate", "BreitbartNews", 
           "HuffPostPol", "StephenAtHome", "thinkprogress", "TheDailyShow", 
           "dailykos", "seanhannity", "FiveThirtyEight",
           "glennbeck", "BuzzFeedPol", "voxdotcom", "OANN")

# Other relevant accounts
politicians <- c("algore", "MittRomney", "SarahPalinUSA", "POTUS", "mike_pence", "VP",
                 "JoeBiden", "newtgingrich", "TheDemocrats", "GOP", "billclinton",
                 "GeorgeHWBush", "dccc", "HouseDemocrats", "SenateDems", "SenateGOP",
                 "HouseGOP", "GovMikeHuckabee", "SenateMajLdr", "BarackObama")
journalists <- c("chrislhayes", "maddow", "Lawrence", 
                 "andersoncooper", "donlemon", "ChrisCuomo",
                 "IngrahamAngle", "seanhannity", "TuckerCarlson",
                 "gstephanopoulos", "AnnCoulter", "megynkelly", "foxandfriends")
interest_groups <- c("Heritage", "OccupyWallSt", "HRC", "RANDCorporation", "BrookingsInst",
                     "CatoInstitute", "AEI", "NRA", "glaad", "ACLU")

accounts <- unique(c(accounts, dems, reps, media, politicians, journalists, interest_groups))

## downloading user data
users <- getUsersBatch(screen_names=accounts, oauth=oauth_folder)
## checking which ones are missing
accounts[tolower(accounts) %in% tolower(users$screen_name) == FALSE]

## merging with congress data and adding labels
names(users)[names(users)=="name"] <- "twitter_name"
users$twitter <- tolower(users$screen_name)
congress$twitter <- tolower(congress$twitter)
congress$type <- "Congress"
users <- merge(users, congress, by="twitter", all.x=TRUE)
users$party[users$twitter %in% tolower(reps)] <- "Republican"
users$party[users$twitter %in% tolower(dems)] <- "Democrat"
users$type[users$twitter %in% tolower(c(dems, reps))] <- "Primary Candidate"
users$type[users$twitter %in% tolower(media)] <- "Media Outlets"
users$type[users$twitter %in% tolower(politicians)] <- "Other Politicians"
users$type[users$twitter %in% tolower(journalists)] <- "Journalists"
users$type[users$twitter %in% tolower(interest_groups)] <- "Interest groups"

table(users$type, exclude=NULL)
table(users$party, exclude=NULL)
table(users$chamber, exclude=NULL)

write.csv(users, file=paste0(dropbox, "data/tweetscores/", polsfile),
    row.names=FALSE)

## sanity check: accounts with < 2000 followers
users[users$followers_count<2000,]






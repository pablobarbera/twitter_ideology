#==============================================================================
# 01-get-twitter-data.R
# Purpose: download list of Twitter followers of politicians from Twitter API
# Details: follower lists are stored in 'outfolder' as .Rdata files
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
setwd("~/git/twitter_ideology/2016-election")
date <- gsub('-', '', Sys.Date())
outfolder <- paste0('data/followers-lists-', date, '/')
try(dir.create(outfolder))
oauth_folder <- '~/Dropbox/credentials/twitter'


## scraping list of social media accounts for Members of the US Congress
## from 'unitedstates' GitHub account
congress <- scrapeCongressData()
write.csv(congress, file=paste0('data/congress-social-media-', date, '.csv'), row.names=FALSE)

## preparing to download follower lists
accounts <- congress$twitter[!is.na(congress$twitter)]

## adding primary candidates

# DEMOCRATS: Hillary Clinton, Bernie Sanders, Martin O'Malley, 
# Lincoln Chafee, Jim Webb
dems <- c('HillaryClinton', 'BernieSanders', "MartinOMalley",
    "LincolnChafee", "JimWebbUSA")

# Ben Carson, Ted Cruz, Carly Fiorina, Lindsey Graham,
# Mike Huckabee, George Pataki, Rand Paul, Marco Rubio,
# Rick Santorum, Bobby Jindal, Rick Perry, Donald Trump,
# Jeb Bush, Chris Christie, John Kasich, Scott Walker, Jim Gilmore
reps <- c("RealBenCarson", "tedcruz", "CarlyFiorina", "GrahamBlog", 
    "GovMikeHuckabee", "GovernorPataki", "RandPaul", "marcorubio", 
    "RickSantorum", "bobbyjindal", "GovernorPerry", "realDonaldTrump",
    "JebBush", "GovChristie", "JohnKasich", "ScottWalker", 
    "gov_gilmore")

# adding also major media outlets in the US to help w/estimation
media <- c("EconUS", "BBCWorld", "nprnews", "NewsHour", "WSJ", "ABC", 
    "CBSNews", "NBCNews", "CNN", "USATODAY", "theblaze", "nytimes", 
    "washingtonpost", "msnbc", "GuardianUS", "Bloomberg", "NewYorker", 
    "politico", "YahooNews", "FoxNews", "MotherJones", "Slate", "BreitbartNews", 
    "HuffPostPol", "StephenAtHome", "thinkprogress", "TheDailyShow", 
    "DRUDGE_REPORT", "dailykos", "seanhannity", "ajam", "edshow", 
    "glennbeck", "rushlimbaugh", "BuzzFeedPol")

# Other relevant accounts
politicians <- c("algore", "MittRomnney", "SarahPalinUSA", "KarlRove", "POTUS",
    "JoeBiden", "newtgingrich", "TheDemocrats", "GOP", "billclinton",
    "GeorgeHWBush", "dccc", "HouseDemocrats", "SenateDems", "Senate_GOPs", "HouseGOP")
journalists <- c("maddow", "glennbeck", "limbaugh", "andersoncooper", "gstephanopoulos",
    "AnnCoulter", "seanhannity", "oreillyfactor", "megynkelly", "MHarrisPerry") # journalists
interest_groups <- c("Heritage", "OccupyWallSt", "HRC", "RANDCorporation", "BrookingsInst",
    "CatoInstitute", "AEI", "NRA", "glaad", "ACLU") # interest groups

accounts <- unique(c(accounts, dems, reps, media, politicians, journalists, interest_groups))

## downloading user data
users <- getUsersBatch(screen_names=accounts, oauth_folder=oauth_folder)
names(users)[names(users)=="name"] <- "twitter_name"

## merging with congress data and adding labels
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

write.csv(users, file=paste0('data/accounts-twitter-data-', date, '.csv'),
    row.names=FALSE)

## keeping only accounts with 1000+ followers
accounts <- users$screen_name[users$followers_count>1000]

# first check if there's any list of followers already downloaded to 'outfolder'
accounts.done <- gsub(".rdata", "", list.files(outfolder))
accounts.left <- accounts[tolower(accounts) %in% tolower(accounts.done) == FALSE]
accounts.left <- accounts.left[!is.na(accounts.left)]

# loop over the rest of accounts, downloading follower lists from API
while (length(accounts.left) > 0){

    # sample randomly one account to get followers
    new.user <- sample(accounts.left, 1)
    #new.user <- accounts.left[1]
    cat(new.user, "---", users$followers_count[users$screen_name==new.user], 
        " followers --- ", length(accounts.left), " accounts left!\n")    
    
    # download followers (with some exception handling...) 
    error <- tryCatch(followers <- getFollowers(screen_name=new.user,
        oauth_folder=oauth_folder, sleep=0.5, verbose=FALSE), error=function(e) e)
    if (inherits(error, 'error')) {
        cat("Error! On to the next one...")
        next
    }
    
    # save to file and remove from lists of "accounts.left"
    file.name <- paste0(outfolder, new.user, ".rdata")
    save(followers, file=file.name)
    accounts.left <- accounts.left[-which(accounts.left %in% new.user)]

}




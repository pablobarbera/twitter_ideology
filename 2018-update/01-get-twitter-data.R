#==============================================================================
# 01-get-twitter-data.R
# Purpose: download list of Twitter followers of politicians from Twitter API
# Details: follower lists are stored in 'outfolder' as .Rdata files
# Author: Pablo Barbera
#==============================================================================

# setup
library(tweetscores)
outfolder <- '~/Dropbox/tweetscores/followers-lists-201807/'
oauth_folder <- '~/Dropbox/credentials/twitter'

## scraping list of social media accounts for Members of the US Congress
## from 'unitedstates' GitHub account
congress <- scrapeCongressData()
# fixing one account manually
congress$twitter[congress$twitter=="RepBlainePress" & !is.na(congress$twitter)] <- "RepBlaine"
write.csv(congress, file='~/Dropbox/tweetscores/congress-social-media-20180.csv', 
          row.names=FALSE)

## preparing to download follower lists
accounts <- congress$twitter[!is.na(congress$twitter)]

## adding major presidential candidates

# DEMOCRATS: Hillary Clinton, Bernie Sanders
dems <- c('HillaryClinton', 'BernieSanders')

# REPUBLICANS: Donald Trump, Ted Cruz, John Kasich,
# Jeb Bush, Marco Rubio, Chris Christie
reps <- c("realDonaldTrump", "tedcruz", "JohnKasich",
          "JebBush", "marcorubio", "GovChristie")

# adding also major media outlets in the US to help w/estimation
media <- c("EconUS", "BBCWorld", "NPR", "NewsHour", "WSJ", "ABC", 
           "CBSNews", "NBCNews", "CNN", "USATODAY", "theblaze", "nytimes", 
           "washingtonpost", "msnbc", "GuardianUS", "Bloomberg", "NewYorker", 
           "politico", "YahooNews", "FoxNews", "MotherJones", "Slate", "BreitbartNews", 
           "HuffPostPol", "StephenAtHome", "thinkprogress", "TheDailyShow", 
           "DRUDGE_REPORT", "dailykos", "seanhannity", "ajam", "edshow", 
           "glennbeck", "rushlimbaugh", "BuzzFeedPol")

# Other relevant accounts
politicians <- c("algore", "MittRomney", "SarahPalinUSA", "POTUS", "mike_pence", "VP",
                 "JoeBiden", "newtgingrich", "TheDemocrats", "GOP", "billclinton",
                 "GeorgeHWBush", "dccc", "HouseDemocrats", "SenateDems", "SenateGOP", "HouseGOP",
                 "GovMikeHuckabee", "SenateMajLdr", "SenSchumer", "SenWarren")
journalists <- c("maddow", "glennbeck", "limbaugh", "andersoncooper", "gstephanopoulos",
                 "AnnCoulter", "seanhannity", "megynkelly", "IngrahamAngle", "chrislhayes",
                 "donlemon", "TuckerCarlson") # journalists
interest_groups <- c("Heritage", "OccupyWallSt", "HRC", "RANDCorporation", "BrookingsInst",
                     "CatoInstitute", "AEI", "NRA", "glaad", "ACLU") # interest groups

accounts <- unique(c(accounts, dems, reps, media, politicians, journalists, interest_groups))

## downloading user data
users <- getUsersBatch(screen_names=accounts, oauth=oauth_folder)
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

write.csv(users, file="~/Dropbox/tweetscores/accounts-twitter-data-2018-07.csv",
    row.names=FALSE)

## sanity check: accounts with < 2000 followers
users[users$followers_count<2000,]

# first check if there's any list of followers already downloaded to 'outfolder'
accounts.done <- gsub(".rdata", "", list.files(outfolder))
accounts.left <- accounts[tolower(accounts) %in% tolower(accounts.done) == FALSE]
accounts.left <- accounts.left[!is.na(accounts.left)]

# excluding Trump, nytimes, cnn, bccworld, potus, hillaryclinton for now:
accounts.left <- accounts.left[tolower(accounts.left) %in% c("realdonaldtrump",
                                                    "nytimes", "cnn", "bccworld",
                                                    "potus", "hillaryclinton") == FALSE]

# loop over the rest of accounts, downloading follower lists from API
while (length(accounts.left) > 0){

    # sample randomly one account to get followers
    new.user <- sample(accounts.left, 1)
    #new.user <- accounts.left[1]
    cat(new.user, "---", users$followers_count[users$screen_name==new.user], 
        " followers --- ", length(accounts.left), " accounts left!\n")    
    
    # download followers (with some exception handling...) 
    error <- tryCatch(followers <- getFollowers(screen_name=new.user,
        oauth=oauth_folder, sleep=0.5, verbose=FALSE), error=function(e) e)
    if (inherits(error, 'error')) {
        cat("Error! On to the next one...")
        next
    }
    
    # save to file and remove from lists of "accounts.left"
    file.name <- paste0(outfolder, new.user, ".rdata")
    save(followers, file=file.name)
    accounts.left <- accounts.left[-which(accounts.left %in% new.user)]

}




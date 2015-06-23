#==============================================================================
# 03-create-adjacency-matrix.R
# Purpose: create adjacency matrix indicating what users follow each
# politician
# Author: Pablo Barbera
#==============================================================================

source('functions.R')

## change the following two lines to run this Rscript for other countries
outfolder <- 'temp/US/followers_lists/'
userfile <- 'output/userlist-US.rdata'
matrixfile <- 'output/adj-matrix-US.rdata'

#==============================================================================
# CENSUS: M
#==============================================================================

fls <- list.files(outfolder, full.names=TRUE)
census <- gsub(paste0(outfolder, "\\/(.*).Rdata"), fls, repl="\\1")
m <- length(census)

#==============================================================================
# USERS: N
#==============================================================================

# loading entire user list following >=3 politicians
load(userfile)
n <- length(userlist)

#==============================================================================
# CREATING ADJACENCY MATRIX
#==============================================================================

m <- length(fls)
rows <- list()
columns <- list()

pb <- txtProgressBar(min=1,max=m, style=3)
for (j in 1:m){
	cat(fls[j])
    load(fls[j])
    to_add <- which(userlist %in% followers)
    rows[[j]] <- to_add
    columns[[j]] <- rep(j, length(to_add))
    setTxtProgressBar(pb, j)
}

rows <- unlist(rows)
columns <- unlist(columns)

# preparing sparse Matrix
library(Matrix)
y <- sparseMatrix(i=rows, j=columns)
rownames(y) <- userlist
colnames(y) <- census

save(y, file=matrixfile)







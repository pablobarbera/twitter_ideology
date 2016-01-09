Updated ideology estimates for Twitter users
--------------

This github repository contains the code used to generate updated ideology estimates for Twitter users in the United States as of January 2016.

- `00-install-packages.R` illustrates how to install the required packages to run the code.
- `01-get-twitter-data.R` downloads the list of Twitter followers of political accounts from the Twitter API, and provides more information about what specific accounts are included in the estimation.
- `02-create-adjacency-matrix.R` compiles the list of users who follow 3 or more political accounts, and generates the adjacency matrix based on what political accounts they follow, which will be used in the estimation.
- `03-estimation.R` produces the ideology estimates. First, the model is estimated with a subset of the adjacency matrix (those who follow 3+ Members of Congress). Then, the rest of users' ideology estimates are computed by considering them as supplementary rows in the correspondence analysis step.

The resulting estimates (and full data files) are available in .rdata and .csv format for SMaPP members only at the NYU's HPC:

- `/archive/smapp/archive/ideology-estimates-20160101.rdata` (~300MB)
- `/archive/smapp/archive/ideology-estimates-20160101.csv` (~675MB)
- `/archive/smapp/archive/ideology-data-files-20160101.zip` (~1.5GB)

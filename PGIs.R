# PGIs in WLS
source("00_MASTER.R")


# PGI EA3
pgi3cog <- read_dta("data/Lee_idpub_shuffled.dta")
pgi3noncog <- read_dta("data/Turley_idpub_shuffled.dta")

pgi3_ID <- paste(pgi3cog$idpub,pgi3cog$rtype, sep = "_")

# PGI EA4
pgi4 <- read_dta("data/PGIrepo_v1.1_idpub_shuffled.dta")

pgi4_ID <- paste(pgi4$idpub,pgi4$rtype, sep = "_")

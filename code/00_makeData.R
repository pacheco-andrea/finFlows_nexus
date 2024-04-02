# Global biodiversity finance flows and the nexus: 

# this script is the basis for the data analysis - it just simplifies the columns so that they're not huge and hard to see/read in R
# the output is the data-simplified version


# set directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/"

# upon first run, read data on ALL volumes of financial flows to biodiversity
bd_fin <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows.csv")) # this data describes all kinds of financial flows for biodiversity finance
# clean this data:
head(bd_fin)
bd_fin2 <- select(bd_fin, -c("Descr.Method", "CertaintyDetail", "SourceDetail"))# remove the description col for ease of viewing the data in R
# standardize the case of certain variables
unique(bd_fin2$Sector_econAct)
head(bd_fin2)
nrow(bd_fin2) == length(unique(bd_fin$id))
unlist(lapply(bd_fin2, class))
setwd(paste0(wdmain, "data/"))
write.csv(bd_fin2, "BD_allFinanceFlows_simplified.csv", row.names = F)

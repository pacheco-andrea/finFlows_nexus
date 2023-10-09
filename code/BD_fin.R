########################################################################################################################
###################################### Global biodiversity finance flows and the nexus #################################
########################################################################################################################

# Part of work for Chapter 6 of the IPBES Nexus Assessment
# Author: Andrea Pacheco
# first run: Sept 28th 2023
# last run: Oct 5th 2023

# description: this script reads raw data gathered from differen academic and gray lit sources on the global financial flows directed to biodiversity,
# and how these flows are connected to different nexus elements
# this script cleans up that data and has the main output of mapping the volumes of nature negative and positive flows via Treemaps
# (future scripts will deal with the mappinf of the nexiness of flows)
# outputs: clean(er) bd financing table, treemap of negative flows, treemap of all flows, treemap of positive flows

# pending issues from this script 
# the raw monetary values need to be standardized/deflated to one year of reference. I would advocate for 2024 numbers 

# libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(treemap)


# set directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/"

# upon first run, read data on ALL volumes of financial flows to biodiversity
# bd_fin <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows.csv")) # this data describes all kinds of financial flows for biodiversity finance
#  clean this data:
# head(bd_fin)
# bd_fin2 <- bd_fin[,-c(11:12)] # remove the description and certainty cols bc they're long descriptions
# head(bd_fin2)
# nrow(bd_fin2)
# lapply(bd_fin2, class)
# setwd(paste0(wdmain, "data/"))
# write.csv(bd_fin2, "BD_allFinanceFlows_simplified.csv", row.names = F)

# explore
bd_fin <- read.csv("BD_allFinanceFlows_simplified.csv", )
head(bd_fin)
# create subset for only positive flows
pos_flow <- bd_fin[which(bd_fin$Categ_impact == "Positive"),]
# plot(pos_flow$Value_lowerLim) # i see an extreme outlier: stocks and bonds that have signed up to UN principles for responsible investment
# pos_flow[which(pos_flow$Value_lowerLim > 20000),] 
# plot(pos_flow$Value_lowerLim[which(pos_flow$Value_lowerLim < 1500)])
# unique(pos_flow$Categ_instrmnt) # 26 different instruments these flows are dispersed/employed!
# create subset for only negative flows
neg_flow <- bd_fin[which(bd_fin$Categ_impact == "Negative"),]
# plot(neg_flow$Value_lowerLim)
# unique(neg_flow$Categ_instrmnt) # only 3 different instruments

# note, clearly there is a possibility of double counting financial flows, bc diff sources estimate similar, global figures
# it is important to take care of which figures are used and reported from this


# test treemap of nature-negative flows ----
# 
# # start with the harmful flows
# head(neg_flow[,1:9])
# nrow(neg_flow)
# # simple version with only state of nature 2022 estimates
# unique(neg_flow$Source)
# neg_2022 <- neg_flow[which(neg_flow$Source == "UNEP 2022 (State of Finance)"),]
# neg_2022
# 
# treemap(neg_2022, 
#         index = "Sector_econAct", 
#         vSize = "Value_lowerLim",
#         type = "index",
#         palette = "YlOrRd",
#         title = "Public financial support for nature-negative activities (UNEP 2022)")
# 
# # join the label with the number in order to print the number with the label
# neg_2022$label <- paste(neg_2022$Sector_econAct, paste0(neg_2022$Value_lowerLim, " B"), sep = "\n")
# 
# treemap(neg_2022, 
#         index = "label", 
#         vSize = "Value_lowerLim",
#         type = "index",
#         palette = "YlOrRd",
#         title = "Annual, public financial support for nature-negative activities (USD Billions) (UNEP 2022)")
# 
# # write out to test size
# setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/fin_flows/outputs/")
# png(filename = "testplot_subsidies_UNEP2022.png", width = 1000, height = 600, units = "px", bg = "transparent")
# treemap(neg_2022, 
#         index = "label", 
#         vSize = "Value_lowerLim",
#         type = "index",
#         palette = "YlOrRd",
#         title = "Annual, public financial support for nature-negative activities (USD Billions) (UNEP 2022)")
# dev.off()


# treemap scaled up to all nature-neg activities ----

# there are three main categories of negative flows: private, illegal, and public

# private flows: 
# should only include portfolio earth calculations (see description of Dasgupta estimate in raw data table)
neg_priv <- neg_flow[which(neg_flow$Source == "Portfolio Earth 2020" & neg_flow$Sector_econAct != "total"),] # remove total count to avoid double counting
# illegal flows:
# should have the estimate which is all environmental crimes (not just illegal wildlife trade)
neg_illeg <- neg_flow[which(neg_flow$Source == "OECD 2021, based on Nellemann et al., 2018"),] 
# public flows:
neg_flow[which(neg_flow$Sector == "Public"),]
# as seen here, these estimates vary a LOT, and they repeat across sectors of economic activity. this is how i defined priorities of what to map:
# i know that i definitely want to keep the UNEP 2022 estimates because according to my review, these were the most recent and reliable estimates
# agriculture: use the UNEP 2022 figs because lower and upper bounds are very similar to all other estimates from other sources. 
  # The only exception is the estimate from the OECD which estimates 800 billion. but this includes both ag and fossil fuels, so it makes sense to omit this aggregated number 
# forestry: from 28-55 (Deutz) to 155 (koplow). the latter is not technically a subsidy from what i understood - it's just how they categorized illegal flows. Therefore, I choose to use Deutz.
# (energy?) & fossil fuels: these are the most wild variations from 340-530B (UNEP 2022) to 5300B (Coady 2017) or 7000B (IMF). The latter two include estimates of the "harms" caused rather than direct costs. I think this would be better explained in the text than visualized.
  # thus. i choose to use the UNEP 2022 estimates because they include lower and upper estimates (upper estimates which are similar to other estimates)
# I keep estimates for construction, water, and transport: as only the Koplow study estimated these. 

# build table accordingly to these priorities:
a <- neg_flow[which(neg_flow$Sector == "Public" & neg_flow$Source == "UNEP 2022 (State of Finance)"),]
b <- neg_flow[which(neg_flow$Sector_econAct == "construction" | neg_flow$Sector_econAct == "water"| neg_flow$Sector_econAct == "transport" & neg_flow$Sector == "Public"),]
c <- neg_flow[which(neg_flow$Sector == "Public" & neg_flow$Sector_econAct == "forestry" & neg_flow$Source == "Deutz 2020"),]
neg_pub <- rbind(a,b,c)
nrow(neg_pub)

# table with all negative flows
neg_flow_clean <- rbind(neg_priv, neg_pub, neg_illeg)
nrow(neg_flow_clean)
neg_flow_clean

# create mean value for plotting
neg_flow_clean$mValue <- rowMeans(cbind(neg_flow_clean$Value_lowerLim, neg_flow_clean$Value_upperLim))
# need to create a label that will allow me to paste the range in values, but only when there is a range
neg_flow_clean$label <- NA
makeLableWithLims <- function(table, labelName){
  n <- grep(labelName, colnames(table))
  labels <- c()
  
  for(i in 1:nrow(table))
  {
    labels[i] <- paste0(table[i,n],
                        if(table$Value_lowerLim[i] < table$Value_upperLim[i]){
                          paste0(" (", table$Value_lowerLim[i], "-", table$Value_upperLim[i], " B)")
                          }else{paste0(" (", table$mValue[i], " B)")},
                        sep = "\n")
  }
  return(labels)
}
neg_flow_clean$label <- makeLableWithLims(neg_flow_clean, "Sector_econAct")

# write out treemap
setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/fin_flows/outputs/")
png(filename = "negFlows.png", width = 1000, height = 600, units = "px", bg = "white")
treemap(neg_flow_clean, 
        index = c("Categ_instrmnt", "label"), # the order determines groups and subgroups
        vSize = "mValue",
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"), 
          c("center", "center")
        ),  
        # position.legend = "right",
        # reverse.legend = T,
        fontsize.labels = c(14,12),
        fontsize.title = 16,
        palette = "YlOrRd",
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "Annual public and private financial support for nature-negative activities (Billions USD)")
dev.off()


# treemap scaled up for all financial flows ----

# to join positive and negative flows:

# 1. clean up positive flows 
pos_flow2 <- pos_flow[which(pos_flow$Source == "UNEP 2022 (State of Finance)"),]
pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "total"),]
pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "marine"),] # need to exclude marine because its double counting, but it should be mentioned in the text
# create mean value for plotting 
pos_flow2$mValue <-  rowMeans(cbind(pos_flow2$Value_lowerLim, pos_flow2$Value_upperLim))
# create column for labeling
# in this case, i want a composite label of the cateogry of instrument and of the sector of econ activity
pos_flow2$Categ_instrmnt <- gsub("Domestic budgets", "Domestic budgets:", pos_flow2$Categ_instrmnt)
pos_flow2$categ3 <- paste(pos_flow2$Categ_instrmnt, pos_flow2$Sector_econAct)
pos_flow2$label <- makeLableWithLims(pos_flow2, "categ3")
# visualize
treemap(pos_flow2, 
        index = c("Sector", "label"), # the order determines groups and subgroups
        vSize = "mValue",
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"), 
          c("center", "center")
        ),  
        fontsize.labels = c(14,12),
        fontsize.title = 16,
        palette = "Greens",
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "Annual public and private financial support for nature-positive activities (Billions USD)")

# because positive flows are so much smaller than negative flows, 
# plotting the small details of the different instruments via which private flows are dispersed doesn't make sense as they wont appear in the treemap
# instead, i chose to create a simplified row/observation for positive flows to add to the negative flow table
# this should illustrate the scale of financing of positive compared to negative
neg_flow_clean
colnames(neg_flow_clean) == colnames(pos_flow2[,-c(17)])
pos_flow3 <- pos_flow2 %>%
  group_by(Category, Categ_impact, Sector, Unit..USD.YY., NormalizedValue_YY, Source) %>%
  summarize(Value_lowerLim = sum(Value_lowerLim),
            Value_upperLim = sum(Value_upperLim)) 
# add label column at this point
pos_flow3$label <- makeLableWithLims(pos_flow3, "Sector")

# join into full dataset of flows
posWithinNegFlows <- full_join(neg_flow_clean, pos_flow3)
# recalculate mean values to include 2 new ones
posWithinNegFlows$mValue <- rowMeans(cbind(posWithinNegFlows$Value_lowerLim, posWithinNegFlows$Value_upperLim))


setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/outputs/")
png(filename = "summaryAllFlows.png", width = 1100, height = 650, units = "px", bg = "white")

treemap(posWithinNegFlows, 
        index = c("Categ_impact", "Categ_instrmnt", "label"), # the order determines groups and subgroups
        vSize = "mValue",
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"),
          c("left", "top"), 
          c("right", "bottom")),
        ymod.labels = c(5,0,0),
        position.legend = "bottom",
        
        fontsize.labels = c(28,22,16),
        fontsize.title = 33,
        fontsize.legend = 22,
        palette = c("#E5C494", "#66C2A5"),
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "Scale of annual nature-negative and nature-positive financing (Billions USD)")

dev.off()

# positive flows ----
# i don't think it makes sense to map the economic activity sector for positive flows - it makes more sense to map the instruments
# or does it?
pos_flow2

# potential treemap
setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/outputs/")
png(filename = "posFlows.png", width = 1100, height = 650, units = "px", bg = "white")

treemap(pos_flow2,
        index = c("Sector", "label"),
        vSize = "mValue", 
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"), 
          c("right", "bottom")
        ),  
        # position.legend = "right",
        # reverse.legend = T,
        fontsize.labels = c(22,16),
        fontsize.title = 28,
        palette = "#66C2A5",
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "Annual public and private financial support for nature-positive activities (Billions USD)")

dev.off()

pos_flow2 %>%
  group_by(Sector) %>%
  summarize(sums = sum(Value_lowerLim), sumsup = sum(Value_upperLim))


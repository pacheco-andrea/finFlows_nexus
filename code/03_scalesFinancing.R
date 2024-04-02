#### Global biodiversity finance flows and the nexus ####

# Part of work for Chapter 6 of the IPBES Nexus Assessment
# Author: Andrea Pacheco
# first run: 28.09.2023
# last run: 25.03.2024

# description: this script reads raw data that I gathered from different academic and gray lit sources 
# on the global financial flows directed to biodiversity, 
# and how these flows are connected to different nexus elements and cleans it.

# output: 
# 1. visualization of the amount and scale of nature negative and positive flows using treemaps
# 2. clean(er) bd financing table

# pending issues: 
# the raw monetary values need to be standardized/inflated to 2024

# libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(treemap)



# set directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/"
setwd(wdmain)

# read clean(er) data to explore ranges of fin volumes
bd_fin <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))
head(bd_fin)

# (not nature positive) but Financing to NbS
positiveData <- read.csv("data/positiveFinancialFlows_clean.csv")

positiveTreemapData <- positiveData %>% filter(Source == "UNEP 2023 SFN")
positiveTreemapData$label <- NA

positiveTreemapData$label[which(positiveTreemapData$Sector == "Private")] <- paste0(positiveTreemapData$Categ_instrmnt[which(positiveTreemapData$Sector == "Private")],
                                                                                    " ($", positiveTreemapData$meanUSD_Y[which(positiveTreemapData$Sector == "Private")], "B)")
positiveTreemapData$label[which(positiveTreemapData$Sector == "Public")] <- paste0(positiveTreemapData$Sector_econAct[which(positiveTreemapData$Sector == "Public")],
                                                                                    " ($", positiveTreemapData$meanUSD_Y[which(positiveTreemapData$Sector == "Public")], "B)")

setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
png(filename = "posFlows.png", width = 30, height = 14, units = "cm", res = 300, bg = "white")

treemap(positiveTreemapData,
        index = c("Sector", "label"),
        vSize = "meanUSD_Y", 
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"), 
          c("right", "bottom")),  
        # position.legend = "right",
        # reverse.legend = T,
        border.col = "white",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(20,12),
        fontcolor.labels ="gray20",
        fontsize.title = 22,
        palette = c("#C6D68A", "#D9AA80"),
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "Public and private financing towards NbS (Billions USD/year)")

dev.off()

# treemap of all nature-neg activities ----

# there are three main categories of negative flows: private, illegal, and public
neg_flows <- bd_fin %>% filter(Categ_impact == "Negative")
unique(neg_flows$Sector)
neg_flows <- select(neg_flows, c("id", "Sector", "Categ_instrmnt", "Sector_econAct", "Value_lowerLim", "Value_upperLim",
                                 "Unit..USD.YY.", "YearData", "NormalizedValue_YY", "HowNexusy", "HowNexusy1", "Source"))
neg_flows_total <- neg_flows %>% filter(str_to_lower(Sector_econAct) == "total")


# private flows: 
# filter for only portfolio earth calculations (see description of Dasgupta estimate in raw data table)
neg_priv <- bd_fin %>%
  filter(Categ_impact == "Negative", Source == "Portfolio Earth 2020", Sector_econAct != "total")
# illegal flows:
# should have the estimate which is all environmental crimes (not just illegal wildlife trade)
neg_illeg <- bd_fin %>%
  filter(Categ_impact == "Negative", Source == "OECD 2021, based on Nellemann et al., 2018")
# public flows:
test <- bd_fin %>%
  filter(Categ_impact == "Negative", Sector == "Public")
plot(test$Value_upperLim)
# as seen here, these estimates vary a LOT, and they repeat across sectors of economic activity. 
# Therefore, I prioritized which observations to report based on the following: 
# 1. I know that i definitely want to keep the UNEP 2022 estimates because according to my review, these were the most recent and reliable estimates
# 2. Specifically for agriculture: use the UNEP 2022 figs because lower and upper bounds are very similar to all other estimates from other sources. 
  # The only exception is the estimate from the OECD which estimates 800 billion. but this includes both ag and fossil fuels, so it makes sense to omit this aggregated number 
# forestry: from 28-55 (Deutz) to 155 (koplow). the latter is not technically a subsidy from what i understood - it's just how they categorized illegal flows. Therefore, I choose to use Deutz.
# Speciically for the (energy?) & fossil fuels sector: 
# these were the widest variations in estimates: from 340-530B (UNEP 2022) to 5300B (Coady 2017) or 7000B (IMF). 
# The latter two include estimates of the "harms" caused rather than direct costs. I think this would be better explained in the text than visualized.
  # Therefore, I prioritizsed using the UNEP 2022 estimates because they include lower and upper estimates (upper estimates which are similar to other estimates)
# I keep estimates for construction, water, and transport: as only the Koplow study estimated these. 

# can i make a smarter prioritization that 1) account for 2023, which are many repeated numbers, but 2) also visualizes the wide range of estimates?

# build table accordingly to these priorities:
a <- neg_flow[which(neg_flow$Sector == "Public" & neg_flow$Source == "UNEP 2022 (State of Finance)"),]
b <- neg_flow[which(neg_flow$Sector_econAct == "construction" | neg_flow$Sector_econAct == "water"| neg_flow$Sector_econAct == "transport" & neg_flow$Sector == "Public"),]
c <- neg_flow[which(neg_flow$Sector == "Public" & neg_flow$Sector_econAct == "forestry" & neg_flow$Source == "Deutz 2020"),]
neg_pub <- rbind(a,b,c)
nrow(neg_pub)

test2 <- bd_fin %>%
  filter(Categ_impact == "Negative", Sector == "Public", Sector_econAct == c(""))



# make table with all negative flows
neg_flow_clean <- rbind(neg_priv, neg_pub, neg_illeg)
nrow(neg_flow_clean)
neg_flow_clean

# create mean value for plotting
neg_flow_clean$mValue <- rowMeans(cbind(neg_flow_clean$Value_lowerLim, neg_flow_clean$Value_upperLim))
# create a label that will allow me to paste the range in values, but specify if condition for only when there is a range
neg_flow_clean$label <- NA
makeLabelWithLims <- function(table, labelName){
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
neg_flow_clean$label <- makeLabelWithLims(neg_flow_clean, "Sector_econAct")

# write out treemap
# setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/fin_flows/outputs/")
# png(filename = "negFlows.png", width = 1000, height = 600, units = "px", bg = "white")
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
# dev.off()


# treemap scaled up for all financial flows ----

# to join positive and negative flows:

# 1. clean up positive flows 
pos_flow2 <- pos_flow[which(pos_flow$Source == "UNEP 2022 (State of Finance)"),]
pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "total"),]
pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "marine"),] # need to exclude marine because its double counting, but it should be mentioned in the text
# create mean value for plotting 
pos_flow2$mValue <-  rowMeans(cbind(pos_flow2$Value_lowerLim, pos_flow2$Value_upperLim))
# create column for labeling
# in this case, i want a composite label of the category of instrument and of the sector of econ activity
pos_flow2$Categ_instrmnt <- gsub("Domestic budgets", "Domestic budgets:", pos_flow2$Categ_instrmnt)
pos_flow2$categ3 <- paste(pos_flow2$Categ_instrmnt, pos_flow2$Sector_econAct)
pos_flow2$label <- makeLabelWithLims(pos_flow2, "categ3")
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
# plotting the small details of the different instruments via which private flows are dispersed doesn't make sense 
# these details will be unreadable in the treemap
# instead, i chose to create a simplified row/observation for positive flows to add to the negative flow table
# this should illustrate the scale of financing of positive compared to negative

# join positive and negative flows:
neg_flow_clean
colnames(neg_flow_clean) == colnames(pos_flow2[,-c(17)])
pos_flow3 <- pos_flow2 %>%
  group_by(Category, Categ_impact, Sector, Unit..USD.YY., NormalizedValue_YY, Source) %>%
  summarize(Value_lowerLim = sum(Value_lowerLim),
            Value_upperLim = sum(Value_upperLim)) 
# need to add label column at this point
pos_flow3$label <- makeLabelWithLims(pos_flow3, "Sector")

# join into table of all flows
posWithinNegFlows <- full_join(neg_flow_clean, pos_flow3)
# recalculate mean values to include 2 new ones
posWithinNegFlows$mValue <- rowMeans(cbind(posWithinNegFlows$Value_lowerLim, posWithinNegFlows$Value_upperLim))


setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
png(filename = "summaryAllFlows.png", width = 1100, height = 650, units = "px", bg = "white")

treemap(posWithinNegFlows, 
        index = c( "Sector", "Categ_impact", "Categ_instrmnt", "label"), # the order determines groups and subgroups
        vSize = "mValue",
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"),
          c("left", "top"), 
          c("right", "bottom")),
        ymod.labels = c(5,0,0),
        position.legend = "bottom",
        border.col = "white",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(28,22,16),
        fontcolor.labels ="white",
        fontsize.title = 33,
        fontsize.legend = 22,
        # palette = c("#D9AA80", "#196C71"),
        palette = c("#B65719", "#4A928F", "#C6D68A", "#4D2D71", "#696B5F", "#FFFFFF"),
        overlap.labels=0.5,   
        title = "Scale of annual nature-negative and nature-positive financing (Billions USD)")

dev.off()

# positive flows ----
pos_flow2

# potential treemap
setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
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
        border.col = "white",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(28,20,18),
        fontcolor.labels ="white",
        fontsize.title = 28,
        palette = c("#C6D68A", "#4A928F"),
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "Annual public and private financial support for nature-positive activities (Billions USD)")

dev.off()

# quick calculation of fig
pos_flow2 %>%
  group_by(Sector) %>%
  summarize(sumsLower = sum(Value_lowerLim), sumsUpper = sum(Value_upperLim))


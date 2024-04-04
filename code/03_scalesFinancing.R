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

#  treemap "nature positive" ----
# note: actually should not say nature positive but rather financing to NbS according to 2023 SFN
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

# after the first version where I was piecing together several data sources, 
# i believe it's best to simply report the UNEP 2023 estimates
# they seem the most complete, researched and up to date
# despite the fact that we're missing upper and lower ranges of estimates in this version of the SFN
# therefore, 
neg_flow <- bd_fin %>% filter(Categ_impact == "Negative", Source == "UNEP 2023 SFN") %>%
  select(c("id", "Sector", "Categ_instrmnt", "Sector_econAct", "Value_lowerLim", "Value_upperLim", 
            "YearData", "NormalizedValue_YY", "Source"))
neg_flow <- neg_flow[which(neg_flow$Sector_econAct != "Total"),]
neg_flow <- neg_flow[-grep("total", neg_flow$Sector_econAct),]
neg_flow %>%
  summarize(total = sum(Value_lowerLim))

# this means that the total of all negative glows according to UNEP 2023 is 6.5 trillion (the almost 7 that they report) 

# However, I do want to supplement these estimates with the illegal flows estimated 
# which are the following:
neg_illeg <- bd_fin %>%
  filter(Categ_impact == "Negative") %>%
  select(c("id", "Sector", "Categ_instrmnt", "Sector_econAct", "Value_lowerLim", "Value_upperLim", 
           "YearData", "NormalizedValue_YY", "Source"))
neg_illeg <- neg_illeg[grep("Illegal", neg_illeg$Categ_instrmnt),]

# make table with all negative flows
neg_flow_clean <- rbind(neg_flow, neg_illeg)
nrow(neg_flow_clean)
neg_flow_clean

# create mean value for plotting
neg_flow_clean$mValue <- rowMeans(cbind(neg_flow_clean$Value_lowerLim, neg_flow_clean$Value_upperLim), na.rm = T)

# create a label that will allow me to paste the range in values, but specify if condition for only when there is a range
neg_flow_clean$label <- NA
makeLabelWithLims <- function(table, labelName){
  n <- grep(labelName, colnames(table))
  labels <- c()
  for(i in 1:nrow(table))
  {
    labels[i] <- paste0(table[i,n],
                        if(table$Value_lowerLim[i] < table$Value_upperLim[i], 
                           
                           
                           na.rm = T){
                          paste0(" ($", table$Value_lowerLim[i], "-", table$Value_upperLim[i], " B)")
                        }else{paste0(" ($", table$mValue[i], " B)")},
                        sep = "\n")
  }
  return(labels)
}
neg_flow_clean$label <- makeLabelWithLims(neg_flow_clean, "Sector_econAct")


# need 5 colors

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


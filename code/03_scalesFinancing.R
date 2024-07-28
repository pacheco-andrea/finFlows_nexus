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

positiveTreemapData <- positiveData %>% filter(Source == "UNEP 2023 SFN") %>%
  select(c("id", "Sector", "Categ_instrmnt", "Sector_econAct", "Value_lowerLim", "Value_upperLim", 
           "YearData", "NormalizedValue_YY", "Source", "meanUSD_Y"))
positiveTreemapData$Sector_econAct <- str_to_sentence(positiveTreemapData$Sector_econAct)
positiveTreemapData$Sector_econAct <- gsub("International (odas and other financial flows)", "ODA", positiveTreemapData$Sector_econAct, fixed = TRUE)
positiveTreemapData$Sector_econAct <- gsub("Total nbs", "Total NbS", positiveTreemapData$Sector_econAct, fixed = TRUE)
positiveTreemapData$Categ_instrmnt <- gsub("Philanthropy/NGOs", "Philanthropy/ NGOs", positiveTreemapData$Categ_instrmnt)
  
positiveTreemapData$label <- NA
positiveTreemapData$label[which(positiveTreemapData$Sector == "Private")] <- paste0(positiveTreemapData$Categ_instrmnt[which(positiveTreemapData$Sector == "Private")],
                                                                                    " ($", positiveTreemapData$meanUSD_Y[which(positiveTreemapData$Sector == "Private")], "B)")
positiveTreemapData$label[which(positiveTreemapData$Sector == "Public")] <- paste0(positiveTreemapData$Sector_econAct[which(positiveTreemapData$Sector == "Public")],
                                                                                    " ($", positiveTreemapData$meanUSD_Y[which(positiveTreemapData$Sector == "Public")], "B)")

setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
svg(filename = "posFlows_versionTranslation.svg", width = 7, height = 3.5, bg = "white")

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
        border.col = "gray20",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(18,13),
        fontcolor.labels ="transparent",
        fontsize.title = 22,
        palette = c("#C6D68A", "#D9AA80"),
        overlap.labels=0.5,   # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        title = "                                      ")
dev.off()

setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
png(filename = "posFlows_versionLabels.png", width = 32, height = 22, units = "cm", res = 300, bg = "white")

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
        border.col = "gray20",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(22,14),
        fontcolor.labels ="gray20",
        fontsize.title = 22,
        palette = c("#C6D68A", "#D9AA80"),
        # force.print.labels = T, 
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
# looked at this in detail, and found that the 110-281 estimate is the total value, so we should just use this
neg_illeg <- neg_illeg[grep("wildlife trade, mining, forestry and fishing", neg_illeg$Sector_econAct),]

# make table with all negative flows
negativeTreemapData <- rbind(neg_flow, neg_illeg)
nrow(negativeTreemapData)
negativeTreemapData$Sector_econAct <- str_to_sentence(negativeTreemapData$Sector_econAct)

# fill in the NAs to prevent error in the labelmaking function
negativeTreemapData$Value_upperLim[which(is.na(negativeTreemapData$Value_upperLim))] <- negativeTreemapData$Value_lowerLim[which(is.na(negativeTreemapData$Value_upperLim))]
# create mean value for plotting
negativeTreemapData$meanUSD_Y <- rowMeans(cbind(negativeTreemapData$Value_lowerLim, negativeTreemapData$Value_upperLim), na.rm = T)

# create a label that will allow me to paste the range in values, but specify if condition for only when there is a range
negativeTreemapData$label <- NA
makeLabelWithLims <- function(table, labelName){
  n <- grep(labelName, colnames(table))
  labels <- c()
  for(i in 1:nrow(table))
  {
    labels[i] <- paste0(table[i,n],
                        if(table$Value_lowerLim[i] < table$Value_upperLim[i]){
                          paste0(" ($", table$Value_lowerLim[i], "-", table$Value_upperLim[i], " B)")
                        }else{paste0(" ($", table$meanUSD_Y[i], " B)")},
                        sep = "\n")
  }
  return(labels)
}
negativeTreemapData$label <- makeLabelWithLims(negativeTreemapData, "Sector_econAct")

# don't really use the negative flows in the chapter (separately)
treemap(negativeTreemapData, 
        index = c("Categ_instrmnt", "label"), 
        vSize = "meanUSD_Y",
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"), 
          c("center", "center")
        ),  
        fontsize.labels = c(14,12),
        fontsize.title = 16,
        palette = "YlOrRd",
        overlap.labels=0.5,   
        title = "Annual public and private financial support for nature-negative activities (Billions USD)")


# treemap positive and negative financial flows ----

# to join positive and negative flows:
# need to add back in these directions of impact
positiveTreemapData$Categ_impact <- "Positive"
negativeTreemapData$Categ_impact <- "Negative"
colnames(positiveTreemapData) == colnames(negativeTreemapData)
allFlows <- rbind(positiveTreemapData, negativeTreemapData)

# try the treemap with all flows
treemap(allFlows, 
        index = c("Categ_impact", "Sector", "label"), 
        vSize = "meanUSD_Y",
        vColor = "manual",
        type = "index",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"),
          c("left", "top"), 
          c("right", "bottom")),
        ymod.labels = c(5,0,0),
        position.legend = "bottom",
        border.col = "gray20",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(28,22,16),
        fontcolor.labels ="gray20",
        fontsize.title = 33,
        fontsize.legend = 22,
        palette = list("Categ_impact" = c("Negative"= "#D9AA80", "Positive"= "#4A928F"),
                       "Sector" = c("Public"= "#BAB0C9", "Private"= "#C6D68A", "Other" = "yellow")),
        overlap.labels=0.5,   
        title = "Scale of annual nature-negative and nature-positive financing (Billions USD)")
# because positive flows are so much smaller than negative flows, 
# plotting the small details of the different instruments via which private flows are dispersed doesn't make sense 
# these details will be unreadable in the treemap
# instead, it's best to create a simplified row/observation for positive flows to add to the negative flow table
# this should illustrate the scale of financing of positive compared to negative
allFlows
nx_col <- c("#799336","#A0AF67","#C6D68A",     
            "#196C71","#4A928F","#A7C6C5",     
            "#4D2D71","#8B78A1","#BAB0C9",    
            "#791E32",
            "#B65719","#C3773E",
            "#D9AA80","#696B5F","#ACABA4","#FFFFFF") 


# edit the positive data accordingly
positiveTreemapData %>%
  group_by(Sector) %>%
  summarize(meanUSD_Y = sum(meanUSD_Y)) 
positiveTreemapData2 <- data.frame(matrix(ncol = ncol(positiveTreemapData), nrow = 2))
names(positiveTreemapData2) <- names(positiveTreemapData)
positiveTreemapData2 <- right_join(positiveTreemapData2, as.data.frame(positiveTreemapData %>%  group_by(Sector) %>%  summarize(meanUSD_Y = sum(meanUSD_Y))))
# add the label once again
positiveTreemapData2$label <- paste0(" ($", positiveTreemapData2$meanUSD_Y, "B)") # dont need to add the category
positiveTreemapData2$Categ_impact <- "Positive"
colnames(positiveTreemapData) == colnames(negativeTreemapData)
allFlows2 <- rbind(positiveTreemapData2, negativeTreemapData)

# i need to make a factor column for the colors
allFlows2$color <- NA
allFlows2$color[which(allFlows2$Categ_impact == "Positive")] <- "pos"
allFlows2$color[which(allFlows2$Categ_impact == "Negative" & allFlows2$Sector == "Private")] <- "neg-pri"
allFlows2$color[which(allFlows2$Categ_impact == "Negative" & allFlows2$Sector == "Public")] <- "neg-pub"
allFlows2$color[which(allFlows2$Categ_impact == "Negative" & allFlows2$Sector == "Other")] <- "neg-oth"
allFlows2$color <- as.factor(allFlows2$color)

setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
png(filename = "summaryAllFlows_versionLabels.png",  width = 32, height = 22, units = "cm", res = 300, bg = "white")

treemap(allFlows2, 
        index = c("Categ_impact", "Sector", "label"), # the order determines groups and subgroups
        vSize = "meanUSD_Y",
        vColor = "color",
        type = "categorical",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"),
          c("left", "top"), 
          c("right", "bottom")),
        ymod.labels = c(0,-.4,-0.01),
        position.legend = "none",
        border.col = "gray20",
        border.lwds = c(1,.5,.05),
        fontsize.labels = c(22,15,12),
        fontcolor.labels ="gray20",
        fontsize.title = 22,
        force.print.labels = T,
        # fontsize.legend = 22,
        # palette = c("#D9AA80", "#196C71"),
        palette = c("#F9E855", "#D9AA80","#C3773E",  "#C6D68A"), #, "#4D2D71", "#696B5F", "#FFFFFF"),
        overlap.labels=0.5,   
        title = "Scale of annual nature-negative and nature-positive financing (Billions USD)")

dev.off()

# version with empty labels
setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
svg(filename = "summaryAllFlows_versionTranslation.svg", width = 7, height = 4, bg = "white")

treemap(allFlows2, 
        index = c("Categ_impact", "Sector", "label"), # the order determines groups and subgroups
        vSize = "meanUSD_Y",
        vColor = "color",
        type = "categorical",
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"),
          c("left", "top"), 
          c("right", "bottom")),
        ymod.labels = c(0,-.2,0),
        position.legend = "none",
        border.col = "black",
        border.lwds = c(1,.08,.07),
        fontsize.labels = c(18,15,12),
        fontcolor.labels ="transparent",
        fontsize.title = 22,
        force.print.labels = T,
        # fontsize.legend = 22,
        # palette = c("#D9AA80", "#196C71"),
        palette = c("#F9E855", "#D9AA80","#C3773E",  "#C6D68A"), #, "#4D2D71", "#696B5F", "#FFFFFF"),
        overlap.labels=0.5,   
        title = " ")

dev.off()


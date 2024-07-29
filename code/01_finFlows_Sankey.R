# Global biodiversity finance flows and the nexus: 

# this script reads in the data compilation on biodiversity financing, cleans and simplifies it
# and then plots the nexiness of current flows - what elements are receiving financing?
# the point of this figure is to communicate the current state of biodiversity-nexus finance flows

# Part of work for Chapter 6 of the IPBES Nexus Assessment
# Author: Andrea Pacheco
# first run: Oct 5th 2023
# last run: April 02 2024


# libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(networkD3)
library(ggplot2)
library(htmlwidgets)
library(webshot2)


# pending issues:
# how to export to png: https://r-graph-gallery.com/159-save-interactive-streamgraph-to-static-image-png.html
# or not png according to new guidelines?

# set directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/"
setwd(paste0(wdmain))

# upon first run:
# source(paste0(wdmain, "code/00_makeData.R"))

# read clean(er) data to explore ranges of fin volumes
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_nexus.csv"))
head(data)
data <- select(data, c("Categ_impact", "Sector", "Categ_instrmnt", "Sector_econAct", "Value_lowerLim",
                 "Value_upperLim", "NormalizedValue_YY", "HowNexusy", "HowNexusy1", "HowNexusy2", "HowNexusy3", "Source"))

# Create subset of only positive flows  to investigate their nexiness ----

# use ONLY the data from UNEP SFN 2023 (most up to date - best choice for comparability)
pos_data <- data %>% filter(Categ_impact == "Positive", Source == "UNEP 2023 SFN")
# remove the rows that sums all of public spending
pos_data <- pos_data[-grep("total$", pos_data$Sector_econAct),]

# create mean value (though less relevant in this version of the SFN)
pos_data$mValue <- rowMeans(cbind(pos_data$Value_lowerLim, pos_data$Value_upperLim), na.rm = T) 
pos_data

# create df for Sankey diagram ----

# clean up and select only data i need
sankeyData <- pos_data %>% select(c("Sector", "Categ_instrmnt", "HowNexusy", "HowNexusy1", "HowNexusy2", "mValue")) # add more nodes later (instruments, more nexus), start with only two levels of nexiness
# make some label fixes: 
sankeyData$Categ_instrmnt <- gsub("Farmer's investments", "Farmer investments", sankeyData$Categ_instrmnt)
# recategorize some instruments
# (these follow what I've also done in the data quality barplots)
sankeyData$Categ_instrmnt <- gsub("Domestic budgets/Taxes", "Taxes", sankeyData$Categ_instrmnt)
sankeyData$Categ_instrmnt <- gsub("Government support/subsidies", "Subsidies", sankeyData$Categ_instrmnt)
sankeyData$Categ_instrmnt <- gsub("Green bonds/loans", "Green bonds", sankeyData$Categ_instrmnt)
sankeyData$Categ_instrmnt <- gsub("Multiple", "Other", sankeyData$Categ_instrmnt)
sankeyData$Categ_instrmnt <- gsub("Voluntary carbon markets", "Carbon markets", sankeyData$Categ_instrmnt)

# the table for Sankey network needs to have sources and targets (two id columns), and one column for nodes (these are the names) 
# this means i need to summarize the data (sum of $ per category) 
# because values should be plotted by total of category (i.e., sector)

sum0 <- sankeyData %>% group_by(Sector, Categ_instrmnt) %>% summarize(mValue = sum(mValue))
colnames(sum0) <- c("source", "target", "value")
sum1 <- sankeyData %>%  group_by(Categ_instrmnt, HowNexusy) %>%  summarise(mValue = sum(mValue)) # summarize total values per sector and biodiv (source 1 - target 1)
colnames(sum1) <- c("source", "target", "value")
sum2 <- sankeyData %>% group_by(HowNexusy, HowNexusy1) %>% summarise(mValue = sum(mValue))
colnames(sum2) <- c("source", "target", "value")
sum3 <- sankeyData %>% group_by(HowNexusy1, HowNexusy2) %>% summarise(mValue = sum(mValue))
colnames(sum3) <- c("source", "target", "value")

sankeyData <- rbind(sum0, sum1, sum2, sum3)
# note, it's necessary to remove the NAs from the flows df. 
# this doesn't remove flows, it just "puts a stopper" in the end nodes
sankeyData <- sankeyData[which(!is.na(sankeyData$target)),]
sankeyData

# create nodes
myNodes <- data.frame(name=c(as.character(sankeyData$source), as.character(sankeyData$target)) %>% unique())
# make group bc this helps set the colors
myNodes$group <- gsub(" ", "-", myNodes$name) 
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sankeyData$IDsource = match(sankeyData$source, myNodes$name)-1
sankeyData$IDtarget = match(sankeyData$target, myNodes$name)-1

# IPBES colors for Sankey ----
# prepare colour scale
# colexample <- 'd3.scaleOrdinal()
# .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"])
# .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

ipbesCols = 'd3.scaleOrdinal() 
.domain(["Private","Public","Carbon-markets","Domestic-budgets","Farmer-investments",
"Impact-investments", "ODA","Offsets","PES","Philanthropy/NGOs",
"Sustainable-supply-chains", "food", "health", "biodiversity", "climate","unknown","water"]) 
.range(["#333333", "#333333", "#BAB0C9", "#333333", "#B65719",  
  "#333333", "#333333", "#C6D68A", "#C6D68A",  "#C6D68A", 
  "#B65719", "#B65719","#791E32", "#C6D68A", "#BAB0C9",  "#ACABA4", "#4A928F"])' # manually edit here according to groups


# Visualize the colors:
length(c("#333333", "#333333", "#BAB0C9", "#333333", "#B65719",  
  "#333333", "#333333", "#C6D68A", "#C6D68A",  "#C6D68A", 
  "#B65719", "#B65719","#791E32", "#C6D68A", "#BAB0C9",  "#ACABA4", "#4A928F"))


# plot Sankey ----
posFlowSankey <- sankeyNetwork(Links = as.data.frame(sankeyData), Nodes = myNodes,
                               Source = "IDsource", Target = "IDtarget",
                               Value = "value", NodeID = "name", 
                               units = "USD Billions", sinksRight=FALSE, 
                               fontFamily = "sans-serif", fontSize=20,
                               NodeGroup = "group", colourScale = ipbesCols, 
                               nodeWidth=40, nodePadding=10)
posFlowSankey

setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
saveWidget(posFlowSankey, file = "BDFin_positiveFlows.html")

# Sankey plot simplified for SPM

# repeat process of cleaning and creating df ready for sankey
# but select only simplified categories:
sankeyData2 <- pos_data %>% select(c("Categ_instrmnt", "HowNexusy", "HowNexusy1", "HowNexusy2", "mValue")) 
# make some label fixes: 
sankeyData2$Categ_instrmnt <- gsub("Farmer's investments", "Farmer investments", sankeyData2$Categ_instrmnt)
# recategorize some instruments
# (these follow what I've also done in the data quality barplots)
sankeyData2$Categ_instrmnt <- gsub("Domestic budgets/Taxes", "Taxes", sankeyData2$Categ_instrmnt)
sankeyData2$Categ_instrmnt <- gsub("Government support/subsidies", "Subsidies", sankeyData2$Categ_instrmnt)
sankeyData2$Categ_instrmnt <- gsub("Green bonds/loans", "Green bonds", sankeyData2$Categ_instrmnt)
sankeyData2$Categ_instrmnt <- gsub("Multiple", "Other", sankeyData2$Categ_instrmnt)
sankeyData2$Categ_instrmnt <- gsub("Voluntary carbon markets", "Carbon markets", sankeyData2$Categ_instrmnt)
# replace the unknown category with
sankeyData2$HowNexusy <- str_to_title(sankeyData2$HowNexusy)
sankeyData2$HowNexusy1 <- str_to_title(sankeyData2$HowNexusy1)
sankeyData2$HowNexusy2 <- str_to_title(sankeyData2$HowNexusy2)
sankeyData2$HowNexusy <- gsub("Unknown", "Undisclosed/Unclear", sankeyData2$HowNexusy)

# summarize the data (sum of $ per category) 
# because values should be plotted by total of category (i.e., sector)
sum1 <- sankeyData2 %>%  group_by(Categ_instrmnt, HowNexusy) %>%  summarise(mValue = sum(mValue)) # summarize total values per sector and biodiv (source 1 - target 1)
colnames(sum1) <- c("source", "target", "value")
sum2 <- sankeyData2 %>% group_by(HowNexusy, HowNexusy1) %>% summarise(mValue = sum(mValue))
colnames(sum2) <- c("source", "target", "value")
sum3 <- sankeyData2 %>% group_by(HowNexusy1, HowNexusy2) %>% summarise(mValue = sum(mValue))
colnames(sum3) <- c("source", "target", "value")

sankeyData2 <- rbind(sum1, sum2, sum3)
# note, it IS necessary to remove the NAs from the flows df. 
# this doesn't remove flows, it just "puts a stopper" in the end nodes
sankeyData2 <- sankeyData2[which(!is.na(sankeyData2$target)),]
sankeyData2

# create nodes
myNodes <- data.frame(name=c(as.character(sankeyData2$source), as.character(sankeyData2$target)) %>% unique())
# make group bc this helps set the colors
myNodes$group <- gsub(" ", "-", myNodes$name) 
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sankeyData2$IDsource = match(sankeyData2$source, myNodes$name)-1
sankeyData2$IDtarget = match(sankeyData2$target, myNodes$name)-1

# make second version of colors (without the public and private, but rather indicating these flows with public/private colors)
ipbesCols2 = 'd3.scaleOrdinal() 
.domain(["Carbon-markets","Domestic-budgets","Farmer-investments",
"Impact-investments", "ODA","Offsets","PES","Philanthropy/NGOs",
"Sustainable-supply-chains", "Food", "Health", "Biodiversity", "Climate","Undisclosed/Unclear","Water"]) 
.range([ "#A7C6C5", "#196C71", "#A7C6C5",  
         "#A7C6C5", "#196C71", "#A7C6C5", "#A7C6C5",  "#A7C6C5", 
         "#A7C6C5", "#B65719","#791E32", "#C6D68A", "#BAB0C9",  "#ACABA4", "#4A928F"])' # manually edit here according to groups


# Visualize the colors:
length(c("#A7C6C5", "#196C71", "#A7C6C5",  
         "#A7C6C5", "#196C71", "#A7C6C5", "#A7C6C5",  "#A7C6C5", 
         "#A7C6C5", "#B65719","#791E32", "#C6D68A", "#BAB0C9",  "#ACABA4", "#4A928F"))


# plot Sankey for SPM ----
posFlowSankey_SPM <- sankeyNetwork(Links = as.data.frame(sankeyData2), Nodes = myNodes,
                               Source = "IDsource", Target = "IDtarget",
                               Value = "value", NodeID = "name", 
                               units = "$ Billions", sinksRight=FALSE, 
                               fontFamily = "sans-serif", fontSize=0,
                               NodeGroup = "group", colourScale = ipbesCols2, 
                               nodeWidth=40, nodePadding=10, height = 1500, width = 3000,
                               sinksRight = F)
posFlowSankey_SPM

setwd("G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/outputs/")
saveWidget(posFlowSankey_SPM, file = "BDFin_positiveFlows_simplified4SPM.html", selfcontained = F)

# export as svg
webshot2::webshot("BDFin_positiveFlows_simplified4SPM.html", file = "test.svg", selector = "body") # doesn't work
# Set the environment variable
Sys.setenv(CHROMOTE_CHROME = "C:/Users/apacheco/AppData/Local/Google/Chrome/Application/chrome.exe")
# export as png
webshot2::webshot("BDFin_positiveFlows_simplified4SPM.html", file = "BDFin_positiveFlows_simplified4SPM.png",
                  vwidth = 1500, vheight = 3000, zoom = 4) # the resolution is controlled via the zoom function


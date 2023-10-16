# Global biodiversity finance flows and the nexus 
# Part of work for Chapter 6 of the IPBES Nexus Assessment
# Author: Andrea Pacheco
# first run: Oct 5th 2023

# libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(networkD3)
library(ggplot2)
library(htmlwidgets)

# pending issues
# how to add middle nodes?
# how to incorporate uncertainties?
# how to export to png: https://r-graph-gallery.com/159-save-interactive-streamgraph-to-static-image-png.html



# directory
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/"
# get simplified data
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))

# nexiness of positive flows ----
pos_flow <- data[which(data$Categ_impact == "Positive"),]
nrow(pos_flow)
# create simplified version for now:
pos_flow2 <- pos_flow[which(pos_flow$Source == "UNEP 2022 (State of Finance)"),]
pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "total"),]
pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "marine"),] # need to exclude marine because its double counting, but it should be mentioned in the text
pos_data <- pos_flow2
table(pos_data$HowNexusy1)
table(pos_data$HowNexusy2) 

# data wrangling for sankey reqs ----
# table needs to have create sources and targets (two id columns), and one column for nodes (these are the names) 

# create mean value 
pos_data$mValue <- rowMeans(cbind(pos_data$Value_lowerLim, pos_data$Value_upperLim)) # (this should be improved later to incorporate uncertainties)
head(pos_data)
# create test data
testData <- pos_data[,c(4,12:13,16)] # add more nodes later (instruments, more nexus), start with only two levels of nexiness
head(testData)
# summarize data because values should be plotted by total of category (i.e., sector)
sum1 <- testData %>%  group_by(Sector, HowNexusy1) %>%  summarise(mValue = sum(mValue)) # summarize total values per sector and biodiv (source 1 - target 1)
# so, how much private, public, (and mixed) goes where?
colnames(sum1) <- c("source", "target", "value")
# sum 2: how much from biodiversity goes also to other nexus elements
sum2 <- testData %>% group_by(HowNexusy1, HowNexusy2) %>% summarise(mValue = sum(mValue))
colnames(sum2) <- c("source", "target", "value")

myData <- rbind(sum1, sum2)
myData <- myData[which(!is.na(myData$target)),]
testData <- myData

# create nodes
myNodes <- data.frame(name=c(as.character(testData$source), as.character(testData$target)) %>% unique())
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
testData$IDsource = match(testData$source, myNodes$name)-1
testData$IDtarget = match(testData$target, myNodes$name)-1

# prepare colour scale
colexample <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) 
.range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

ipbesCols = 'd3.scaleOrdinal() .domain(["Private", "Public", "biodiversity", "unknown", "climate", "food", "food_water", "water"])
.range(["#333333","#333333","#C6D68A","#ACABA4", "#BAB0C9","#B65719","#D5B41F","#4A928F"])' # manually edit here according to groups


testSankey <- sankeyNetwork(Links = testData, Nodes = myNodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            units = "USD Billions", sinksRight=FALSE, 
                            colourScale= ipbesCols, nodeWidth=40, fontSize=13, nodePadding=20)
testSankey

setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/outputs/")
saveWidget(testSankey, file = "testSankey.html")




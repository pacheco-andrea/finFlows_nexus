# Global biodiversity finance flows and the nexus: 

# this script reads in the data compilation on biodiversity financing, cleans and simplifies it
# and then plots the nexiness of current flows - what elements are receiving financing?
# the point of this figure is to communicate the current state of biodiversity-nexus finance flows

# Part of work for Chapter 6 of the IPBES Nexus Assessment
# Author: Andrea Pacheco
# first run: Oct 5th 2023
# last run: March 27th 2024


# libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(networkD3)
library(ggplot2)
library(htmlwidgets)
library(stringr)

# pending issues
# how to add middle nodes?
# how to incorporate uncertainties?
# how to export to png: https://r-graph-gallery.com/159-save-interactive-streamgraph-to-static-image-png.html



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

# read clean(er) data to explore ranges of fin volumes
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))
head(data)




# nexiness of positive flows ----
# pos_flow <- data[which(data$Categ_impact == "Positive"),]
# nrow(pos_flow)
# # create simplified version for now:
# pos_flow2 <- pos_flow[which(pos_flow$Source == "UNEP 2022 (State of Finance)"),]
# pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "total"),]
# pos_flow2 <- pos_flow2[which(pos_flow2$Sector_econAct != "marine"),] # need to exclude marine because its double counting, but it should be mentioned in the text
# pos_data <- pos_flow2
# table(pos_data$HowNexusy)
# table(pos_data$HowNexusy1)
# table(pos_data$HowNexusy2) 
# table(pos_data$HowNexusy3)


# second attempt creating this subset of data ----
# get the most up to date data from UNEP SFN 2023
pos_data <- data %>% filter(Categ_impact == "Positive", Source == "UNEP 2023 SFN")
# remove the row that sums all of public spending
pos_data <- pos_data[which(pos_data$Sector != "Public" | pos_data$Sector_econAct != "total"),]
# also remove the row that sums all of public and private spending
pos_data <- pos_data[which(pos_data$Sector != "Mixed"),]
# create mean value (though less relevant in this version of the SFN)
pos_data$mValue <- rowMeans(cbind(pos_data$Value_lowerLim, pos_data$Value_upperLim), na.rm = T) # (this should be improved later to incorporate uncertainties)
pos_data


# data wrangling for sankey reqs ----
# table needs to have create sources and targets (two id columns), and one column for nodes (these are the names) 

# create test data
testData <- pos_data %>% select(c("Sector", "Categ_instrmnt", "HowNexusy", "HowNexusy1", "mValue")) # add more nodes later (instruments, more nexus), start with only two levels of nexiness
head(testData)
testData$Categ_instrmnt <- gsub("Farmer's investments", "Farmer investments", testData$Categ_instrmnt)
# summarize data because values should be plotted by total of category (i.e., sector)
sum0 <- testData %>% group_by(Sector, Categ_instrmnt) %>% summarize(mValue = sum(mValue))
colnames(sum0) <- c("source", "target", "value")
sum1 <- testData %>%  group_by(Categ_instrmnt, HowNexusy) %>%  summarise(mValue = sum(mValue)) # summarize total values per sector and biodiv (source 1 - target 1)
# so, how much private, public, (and mixed) goes where?
colnames(sum1) <- c("source", "target", "value")
# sum 2: how much from biodiversity goes also to other nexus elements
sum2 <- testData %>% group_by(HowNexusy, HowNexusy1) %>% summarise(mValue = sum(mValue))
colnames(sum2) <- c("source", "target", "value")

myData <- rbind(sum0, sum1, sum2)
# myData <- myData[which(!is.na(myData$target)),]
myData

# create nodes
myNodes <- data.frame(name=c(as.character(myData$source), as.character(myData$target)) %>% unique())
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
myData$IDsource = match(myData$source, myNodes$name)-1
myData$IDtarget = match(myData$target, myNodes$name)-1

# prepare colour scale
colexample <- 'd3.scaleOrdinal()
.domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"])
.range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

ipbesCols = 'd3.scaleOrdinal() 
.domain(["Private","Public","Carbon markets","Domestic budgets","Farmer investments","Impact investment","ODA","Offsets","PES","Philanthropy/NGOs",
"Sustainable supply chains", "biodiversity","climate","food","unknown","water","NA"]) 
.range(["#333333", "#333333", "#BAB0C9", "#333333", "#B65719", "#333333",  "#333333", "#C6D68A", "#C6D68A", "#C6D68A",
  "#B65719", "#C6D68A", "#BAB0C9", "#B65719", "#696B5F", "#4A928F", "#696B5F"])' # manually edit here according to groups

# to see the colors:
c("#333333", "#333333", "#BAB0C9", "#333333", "#B65719", "#333333",  "#333333", "#C6D68A", "#C6D68A", "#C6D68A",
  "#B65719", "#C6D68A", "#BAB0C9", "#B65719", "#696B5F", "#4A928F", "#696B5F")



sankeyNetwork(Links = as.data.frame(myData), Nodes = myNodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              units = "USD Billions", sinksRight=FALSE, 
              fontFamily = "sans-serif", fontSize=20,
              colourScale= ipbesCols, nodeWidth=40, nodePadding=20)


testSankey <- sankeyNetwork(Links = as.data.frame(myData), Nodes = myNodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "value", NodeID = "name", 
                            units = "USD Billions", sinksRight=FALSE, 
                            fontFamily = "sans-serif", fontSize=20,
                            colourScale= ipbesCols, nodeWidth=40, nodePadding=20)
testSankey

setwd("G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/outputs/")
saveWidget(testSankey, file = "testSankey.html")




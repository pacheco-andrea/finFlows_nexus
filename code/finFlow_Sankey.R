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

# directory
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/analyses/finFlows_nexus/"

# get simplified data
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))

# start with how nexusy positive flows are:
pos_data <- data[which(data$Categ_impact == "Positive"),]
nrow(pos_data)
table(pos_data$HowNexusy)
pos_data[55:60,]

# how would I make this work for a sankey or alluvial-type figure? ----

# example here:
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
data # this is what my data would have to look like for the sankey

# convert to long format
data_long <- data %>%
  rownames_to_column %>%
  tidyr::gather(key = 'key', value = 'value', -rowname) %>% 
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)


# basically, this means i have to create sources and targets - the problem is i need to have overlapping targets
# and I've also coded direct and indirect mentions for each of the elements see:

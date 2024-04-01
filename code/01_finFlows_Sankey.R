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
library(cowplot)


# pending issues:
# incorporate other data - mainly flows that are not biodiversity? from other sources?
# how to incorporate uncertainties?
# how to export to png: https://r-graph-gallery.com/159-save-interactive-streamgraph-to-static-image-png.html



# set directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/"

# # upon first run, read data on ALL volumes of financial flows to biodiversity
# bd_fin <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows.csv")) # this data describes all kinds of financial flows for biodiversity finance
# # clean this data:
# head(bd_fin)
# bd_fin2 <- select(bd_fin, -c("Descr.Method", "CertaintyDetail", "SourceDetail"))# remove the description col for ease of viewing the data in R
# # standardize the case of certain variables
# unique(bd_fin2$Sector_econAct)
# head(bd_fin2)
# nrow(bd_fin2) == length(unique(bd_fin$id))
# unlist(lapply(bd_fin2, class))
# setwd(paste0(wdmain, "data/"))
# write.csv(bd_fin2, "BD_allFinanceFlows_simplified.csv", row.names = F)

# read clean(er) data to explore ranges of fin volumes
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))
head(data)

# Create subset of only positive flows  to investigate their nexiness ----

# to begin, use ONLY the data from UNEP SFN 2023 (most up to date)
pos_data <- data %>% filter(Categ_impact == "Positive", Source == "UNEP 2023 SFN")
# remove the row that sums all of public spending
pos_data <- pos_data[which(pos_data$Sector != "Public" | pos_data$Sector_econAct != "total"),]
# also remove the row that sums all of public and private spending
pos_data <- pos_data[which(pos_data$Sector != "Mixed"),]
# create mean value (though less relevant in this version of the SFN)
pos_data$mValue <- rowMeans(cbind(pos_data$Value_lowerLim, pos_data$Value_upperLim), na.rm = T) # (this should be improved later to incorporate uncertainties)
pos_data

# create df for Sankey diagram ----
# table needs to have create sources and targets (two id columns), and one column for nodes (these are the names) 
sankeyData <- pos_data %>% select(c("Sector", "Categ_instrmnt", "HowNexusy", "HowNexusy1", "mValue")) # add more nodes later (instruments, more nexus), start with only two levels of nexiness
# make some label fixes:
sankeyData$Categ_instrmnt <- gsub("Farmer's investments", "Farmer investments", sankeyData$Categ_instrmnt)
# summarize data because values should be plotted by total of category (i.e., sector)
sum0 <- sankeyData %>% group_by(Sector, Categ_instrmnt) %>% summarize(mValue = sum(mValue))
colnames(sum0) <- c("source", "target", "value")
sum1 <- sankeyData %>%  group_by(Categ_instrmnt, HowNexusy) %>%  summarise(mValue = sum(mValue)) # summarize total values per sector and biodiv (source 1 - target 1)
# so, how much private, public, (and mixed) goes where?
colnames(sum1) <- c("source", "target", "value")
# sum 2: how much from biodiversity goes also to other nexus elements
sum2 <- sankeyData %>% group_by(HowNexusy, HowNexusy1) %>% summarise(mValue = sum(mValue))
colnames(sum2) <- c("source", "target", "value")

sankeyData <- rbind(sum0, sum1, sum2)
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

# prepare colour scale
# colexample <- 'd3.scaleOrdinal()
# .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"])
# .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

ipbesCols = 'd3.scaleOrdinal() 
.domain(["Private","Public","Carbon-markets","Domestic-budgets","Farmer-investments",
"Impact-investment","ODA","Offsets","PES","Philanthropy/NGOs",
"Sustainable-supply-chains", "food","climate","biodiversity","unknown","water"]) 
.range([ "#333333", "#333333", "#BAB0C9", "#333333", "#B65719",  
  "#333333", "#333333", "#C6D68A", "#C6D68A",  "#C6D68A", 
  "#B65719", "#B65719", "#BAB0C9", "#C6D68A", "#FFFFFF", "#4A928F"])' # manually edit here according to groups
# Visualize the colors:
c("#333333", "#333333", "#BAB0C9", "#333333", "#B65719",  
  "#333333", "#333333", "#C6D68A", "#C6D68A",  "#C6D68A", 
  "#B65719", "#B65719", "#BAB0C9", "#C6D68A", "#FFFFFF", "#4A928F")


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


# Synthesize and visualize other data that is NOT UNEP 2023 SFN?? ----
# (still focused on positive flows)

# get all data that is positive bd finance flows 
pos_data_older <- data %>% filter(Categ_impact == "Positive") %>%
  select(-c("id", "Category", "Categ_impact", "Unit..USD.YY.", "Certainty"))
# create mean column
pos_data_older$meanUSD_Y <- rowMeans(cbind(pos_data_older$Value_lowerLim, pos_data_older$Value_upperLim), na.rm = T)
# view the data that should be summarizing these
summData <- unique(rbind(pos_data_older[grep("total", pos_data_older$Sector_econAct),], pos_data_older %>% filter(Sector == "Mixed")))
summData

# make barplots of how much bd financing reported per source
ggplot(summData, aes(x = Sector, y = meanUSD_Y)) +
  geom_bar(stat = "identity") +
  labs(title = "my title", x = "sector", y = "USD Billions annually") +
  facet_wrap(~ Source)
# this is all misleading bc there's lots of double counting here
# but reed 2020 source can be excluded from these summary sources
# particularly because when i check the details these data are from 2009-2013, so older than 10 years
# also, what is going on with the 2021 SFN??
summData %>% filter(Source == "UNEP 2021 SFN") # it's the almost 600B that goes to climate finance + private climate data

# visualize ALL the positive flows rows: ----

# version 1:
pos_data_older %>% 
  filter(Source != "Reed 2020", 
         Source != "Bass, Dithrich, and Mudaliar, 2018 (GIIN)",
         Source != "Busch 2021",
         Source != "CPI 2018",
         Source != "Gallo-Caijao 2018") %>%
  ggplot(aes(x = Sector, y = meanUSD_Y)) +
  geom_bar(stat = "identity") +
  labs(title = "Making sense of financial flows by source", x = "sector", y = "USD Billions annually") +
  coord_cartesian(y = c(0, 800)) + # bc otherwise finance watch & Dasgupta take over the y axis
  facet_wrap(~ Source)

# clearly, i need to fix data that's not adding up (due to double-counting): 

# UNEP SFNs ----

# UNEP 2021 ----
unepData <- pos_data_older[grep("UNEP", pos_data_older$Source),]
# sum the values except for the mixed
unepData2021 <- unepData %>% 
  filter(Source == "UNEP 2021 SFN") %>% 
  filter(Sector != "Mixed") %>%
  filter(Sector_econAct != "total") %>% # don't double count the sum
  filter(Sector_econAct != "climate") %>% # don't count this bc for some reason they didn't include climate-specific in their accounting of the SFN 2021
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
unepData2021 # yes - this is the correct value that sums up to 133B
unepData2021$year <- 2021
unepData2021
# version of unep 2021 which includes the funding that goes specifically to climate
unepData2021_climateIncl <- unepData %>%  filter(Source == "UNEP 2021 SFN") %>% filter(Sector_econAct != "total")
unepData2021_climateIncl <- unepData2021_climateIncl[-which(unepData2021_climateIncl$Sector == "Private" & unepData2021_climateIncl$Sector_econAct == "climate"),]
unepData2021_climateIncl %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))

# I went digging to find out more about this climate data bc of that reviewer comment:
# in sum - unep 2021 included CPI data on climate financing - but it didn't for 2022 nor 2023
# i've looked up the newest CPI 2023 and the climate financing in total is over 1 trillion now 
# this is great to climate, but IMO it would drastically change the figure and drive the conversation elsewhere
# hence - I have decided to deal with this in the text, rather than the main sankey figure


#  UNEP 2022 ----
# these are the only data for which we have actual ranges i was able to extract from the report
# here, can see that the total sums should be 82.4 - 227 (mean = 154.7)

unepData2022 <- unepData %>% 
  filter(Source == "UNEP 2022 SFN") %>%
  filter(Sector_econAct != "total") %>%
  # exclude the marine row bc this was just highlighted apart, but is implicitly included in accounting across categories
  filter(Sector_econAct != "marine") %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
unepData2022$year <- 2022
unepData2022

# UNEP 2023 ----
# should sum to 200
unepData2023 <- unepData %>% 
  filter(Source == "UNEP 2023 SFN") %>%
  filter(Sector != "Mixed") %>%
  filter(Sector_econAct != "total") %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
unepData2023$year <- 2023
unepData2023

# summarize UNEP data 
summUNEP <- rbind(unepData2021, unepData2022, unepData2023)
summUNEP$Source <- "UNEP"

# OECD data ----

oecd <- pos_data_older[grep("OECD", pos_data_older$Source),]

# OECD 2020 (a comprehensive overview) ----
# i've finally understood that they do not include any of the impact investment numbers in their accounting
oecd2020 <- oecd %>% filter(Source == "OECD 2020") %>%
  filter(Categ_instrmnt != "Impact investment") %>%
  filter(Sector_econAct != "total") %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
oecd2020$year <- 2020
oecd2020
# make another copy where i do consider impact investment as a part of private finance
oecd2020_II <- oecd %>% filter(Source == "OECD 2020") %>%
  # filter(Categ_instrmnt != "Impact investment") %>%
  filter(Sector_econAct != "total") %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
oecd2020_II$year <- 2020
oecd2020_II

# OECD 2021 (biodiversity natural capital and economy) ---
# it seems to me that this report is more focused on the variety of instruments 
# rather than accounting for total numbers
# this makes it hard to be sure that these sums are accurate...

oecd2021 <- oecd %>% filter(Source == "OECD 2021")
# i need to get rid of this row because it is only the taxes from G7 countries (excluding canada), and so it would double-count
oecd2021 <- oecd2021[-which(oecd2021$Categ_instrmnt == "Taxes" & oecd2021$Value_lowerLim == 2.2),]
# out of the green bonds, i only want to keep the total estimate (257)  
oecd2021 <- oecd2021[-which(oecd2021$Categ_instrmnt == "Green bonds" & oecd2021$Value_lowerLim != 257.7),]
oecd2021 <- oecd2021 %>% 
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
oecd2021$year <- 2021 # private and mixed sectors are now a lot larger than domestic pubic budgets 
# because they include things like the value of ecotourism markets, the global value of green bond markets, expenditure towards restoration projects
# and btw, the public sector is also larger bc this includes the revenue generated from bd-targeted taxes


# OECD 2022 (climate finance provided and mobilised...) ----
oecd2022 <- oecd %>% filter(Source == "OECD 2022") # this is just the total provided and mobilized by developed countries for climate action 
# in developing countries in 2020 (which was, 16.7 billion short of the 2020 goal of 100B)

# summarize OECD
oecd2020
oecd2020_II
oecd2021
# decided to exclude 2022 bc that's just about climate
summOECD <- rbind(oecd2020_II, oecd2021) # and include the estimate that includes impact investment bc everything else does too
summOECD$Source <- "OECD"

# Deutz 2020 (Paulson Institute)
deutz <- pos_data_older %>% filter(Source == "Deutz 2020") %>%
  filter(Sector_econAct != "total") %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
deutz # the mixed category here indicated public-private mix - which can be compared to the OECD 2021 data
deutz$year <- 2020
deutz$Source <- "Deutz"
# summarize these totals ----
summUNEP
summOECD

totalsSummarized <- rbind(summUNEP, summOECD, deutz)

# Total financing by Sector and Source ----
totalsSummarizedPlot <- totalsSummarized %>%
  ggplot(aes(x = year, y = totalUSD, fill = Sector)) +
  geom_col(position = "dodge") +
  geom_errorbar(data = totalsSummarized[which(totalsSummarized$totalUSD_L < totalsSummarized$totalUSD_U),],
                aes(ymin = totalUSD_L  , ymax = totalUSD_U   ), color = "gray25", width = 0.2, position = position_dodge(width = 0.7)) +
    labs(title = "Estimated financial flows to Nature by source", x = element_blank(), y = "USD Billions annually") +
  scale_fill_manual(values = c("Private" = "#A0AF67", "Public" = "#C3773E", "Mixed" = "#8B78A1")) +  
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source)
totalsSummarizedPlot

# write out this part
setwd(paste0(wdmain, "outputs/"))
svg(filename = "EstimatedFinancialFlowsbySource.svg", width = 12, height = 4)
totalsSummarizedPlot
dev.off()

# CONTINUE WITH MAKING THIS A SUPER GRAPH ----
# THAT INCLUDES CERTAINTY - AND MAYBE SECTORS?

a <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "UNEP 2021 SFN") %>% 
  filter(Sector != "Mixed") %>%
  filter(Sector_econAct != "total") %>% # don't double count the sum
  filter(Sector_econAct != "climate")

b <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "UNEP 2022 SFN") %>%
  filter(Sector_econAct != "total") %>%
  # exclude the marine row bc this was just highlighted apart, but is implicitly included in accounting across categories
  filter(Sector_econAct != "marine")

c <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "UNEP 2023 SFN") %>%
  filter(Sector != "Mixed") %>%
  filter(Sector_econAct != "total")

d <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "OECD 2020") %>%
  filter(Categ_instrmnt != "Impact investment") %>%
  filter(Sector_econAct != "total")

# e <- pos_data_older %>% 
#   filter(Source == "OECD 2020") %>%
#   # filter(Categ_instrmnt != "Impact investment") %>%
#   filter(Sector_econAct != "total")

e <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "OECD 2021")
# i need to get rid of this row because it is only the taxes from G7 countries (excluding canada), and so it would double-count
e <- e[-which(e$Categ_instrmnt == "Taxes" & e$Value_lowerLim == 2.2),]
# out of the green bonds, i only want to keep the total estimate (257)  
e <- e[-which(e$Categ_instrmnt == "Green bonds" & e$Value_lowerLim != 257.7),]

f <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "Deutz 2020") %>%
  filter(Sector_econAct != "total")

positiveData <- rbind(a,b,c,d,e,f)
positiveData$meanUSD_Y <- rowMeans(cbind(positiveData$Value_lowerLim, positiveData$Value_upperLim), na.rm = T)


# make the certainty column numeric
unique(positiveData$Certainty)
nx_col <- c("#799336","#A0AF67","#C6D68A",     
            "#196C71","#4A928F","#A7C6C5",     
            "#4D2D71","#8B78A1","#BAB0C9",    
            "#791E32",
            "#B65719","#C3773E",
            "#D9AA80","#696B5F","#ACABA4","#FFFFFF") 

positiveData %>%
  ggplot(aes(x = NormalizedValue_YY, y = meanUSD_Y, fill = HowNexusy)) +
  geom_bar(stat = "identity") +
  labs(title = "Making sense of financial flows by source", x = "Year", y = "USD Billions annually") +
  # add the uncertainty bars for unep 2021
  # scale_fill_manual(values = c("Private" = "#A0AF67", "Public" = "#C3773E", "Mixed" = "#8B78A1")) +  # Custom fill colors
  scale_fill_manual(values = nx_col) + 
  facet_wrap(~Source, nrow = 1)

# make different plots to highlight the quality of these data

# Public, private, and mixed sources of financing over the years
sectorPlot <- positiveData %>%
  ggplot(aes(x = NormalizedValue_YY, y = meanUSD_Y, fill = Sector)) +
  geom_col(position = "dodge", width = 0.7) +
  # add the uncertainty bars for unep 2021 - though maybe this would be more appropriate for the a
  geom_errorbar(data = positiveData[which(positiveData$Source == "UNEP 2022 SFN"),],
                aes(ymin = Value_lowerLim , ymax = Value_upperLim ), color = "gray25", width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Public, private, and public-private mixes of financial flows to Nature", x = element_blank(), y = "USD Billions annually") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) + # there were 2015 and 2016 values, but so tiny barely able to see
  coord_cartesian(xlim = c(2017, 2023)) +
  scale_fill_manual(values = c("Private" = "#A0AF67", "Public" = "#C3773E", "Mixed" = "#8B78A1")) +  # Custom fill colors
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source, ncol = 1) 
sectorPlot

# Certainty of data reported over the years
certaintyPlot <- positiveData %>%
  ggplot(aes(x = NormalizedValue_YY, y = meanUSD_Y, fill = Certainty)) +
  geom_col(position = "dodge", width = 0.7) +
  # # add the uncertainty bars for unep 2021 - though maybe this would be more appropriate for the a
  # geom_errorbar(data = positiveData[which(positiveData$Source == "UNEP 2022 SFN"),],
  #               aes(ymin = Value_lowerLim , ymax = Value_upperLim ), color = "gray25", width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Certainty of financial flows to Nature reported", x = element_blank(), y = "USD Billions annually") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) + # there were 2015 and 2016 values, but so tiny barely able to see
  coord_cartesian(xlim = c(2017, 2023)) +
  scale_fill_manual(values = c("low" = "#C6D68A", "medium" = "#D9AA80", "high" = "#196C71", "unknown" = "#ACABA4", "quantified" = "#EDD018")) +  # Custom fill colors
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source, ncol = 1)
certaintyPlot

# instruments

my_nx_col <- c("#799336","#A0AF67","#C6D68A",     
               "#196C71","#4A928F","#A7C6C5",     
               "#4D2D71","#8B78A1","#BAB0C9",    
               "#791E32","#d14765","#f0c2cc",
               "#B65719","#C3773E","#D9AA80",
               "#ffffff", "#696B5F","#ACABA4","gray15",
               "#D5B41F", "#EDD018", "#F9E855") 

instrumentsPlot <- positiveData %>%
  ggplot(aes(x = NormalizedValue_YY, y = meanUSD_Y, fill = Categ_instrmnt)) +
  geom_col(position = "dodge", width = 0.7) +
  # # add the uncertainty bars for unep 2021 - though maybe this would be more appropriate for the a
  # geom_errorbar(data = positiveData[which(positiveData$Source == "UNEP 2022 SFN"),],
  #               aes(ymin = Value_lowerLim , ymax = Value_upperLim ), color = "gray25", width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Financial flows to Nature by sector ", x = element_blank(), y = "USD Billions annually") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) + # there were 2015 and 2016 values, but so tiny barely able to see
  coord_cartesian(xlim = c(2017, 2023)) +
  scale_fill_manual(values = my_nx_col) +  # Custom fill colors
  facet_wrap(~Source, ncol = 1)
instrumentsPlot

plot_grid(totalsSummarizedPlot, certaintyPlot, instrumentsPlot, ncol = 3)

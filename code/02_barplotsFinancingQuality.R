# Global biodiversity finance flows and the nexus: 

# this script reads in the data compilation on biodiversity financing, 
# the aim is to synthesize and visualize the data compilation overall to illustrate the quality and details
# (still focused on positive flows)

# Part of work for Chapter 6 of the IPBES Nexus Assessment
# Author: Andrea Pacheco
# first run: Oct 5th 2023
# last run: April 2 2024

library(dplyr)
library(ggplot2)
library(cowplot)

# set directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/finFlows_nexus/"

# read clean(er) data 
data <- read.csv(paste0(wdmain, "data/BD_allFinanceFlows_simplified.csv"))
head(data)
# add the column that is the mean
data$meanUSD_Y <- rowMeans(cbind(data$Value_lowerLim, data$Value_upperLim), na.rm = T)

# # get all data that is positive bd finance flows 
# pos_data_older <- data %>% filter(Categ_impact == "Positive") %>%
#   select(-c("id", "Category", "Categ_impact", "Unit..USD.YY.", "Certainty"))

data %>% 
  filter(Categ_impact == "Positive") 

# Make the data selections ----
# i need to go through - almost manually - and determine which flows to take into account
# because otherwise, there are double-counting issues from the compilation - and other similar errors

# version 1:

# UNEP 2021 ----

# sum the values except for the mixed
unepData2021 <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "UNEP 2021 SFN") %>% 
  filter(Sector != "Mixed") %>%
  filter(Sector_econAct != "total") %>% # don't double count the sum
  filter(Sector_econAct != "climate") # don't count this bc for some reason they didn't include climate-specific in their accounting of the SFN 2021

unepData2021_totals <- unepData2021 %>% 
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
unepData2021 # yes - this is the correct value that sums up to 133B
unepData2021_totals$year <- 2021
unepData2021_totals

# Note - I went digging to find out more about this climate data bc of that reviewer comment:
# in sum - unep 2021 included CPI data on climate financing - but it didn't for 2022 nor 2023
# i've looked up the newest CPI 2023 and the climate financing in total is over 1 trillion now 
# this is great to climate, but IMO it would drastically change the figure and drive the conversation elsewhere
# hence - I have decided to deal with this in the text, rather than the main figures

#  UNEP 2022 ----
# these are the only data for which we have actual ranges i was able to extract from the report
# here, can see that the total sums should be 82.4 - 227 (mean = 154.7)

unepData2022 <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "UNEP 2022 SFN") %>%
  filter(Sector_econAct != "total") %>%
  # exclude the marine row bc this was just highlighted apart, but is implicitly included in accounting across categories
  filter(Sector_econAct != "marine") 

unepData2022_totals <- unepData2022 %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
unepData2022_totals$year <- 2022
unepData2022_totals

# UNEP 2023 ----
# should sum to 200
unepData2023 <- data %>% 
  filter(Categ_impact == "Positive") %>%
  filter(Source == "UNEP 2023 SFN") %>%
  filter(Sector != "Mixed") %>%
  filter(Sector_econAct != "total") 

unepData2023_totals <- unepData2023 %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
unepData2023_totals$year <- 2023
unepData2023_totals

# summarize UNEP data 
summUNEP <- rbind(unepData2021_totals, unepData2022_totals, unepData2023_totals)
summUNEP$Source <- "UNEP"

# OECD data ----

oecd <- data %>% 
  filter(Categ_impact == "Positive") 
oecd <- oecd[grep("OECD", oecd$Source),]

# OECD 2020 (a comprehensive overview) ----
# i've finally understood that they do not include any of the impact investment numbers in their accounting
oecd2020 <- oecd %>% filter(Source == "OECD 2020") %>%
  filter(Categ_instrmnt != "Impact investment") %>%
  filter(Sector_econAct != "total")
oecd2020_totals <- oecd2020 %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
oecd2020_totals$year <- 2020
oecd2020_totals
# make another copy where i do consider impact investment as a part of private finance
oecd2020_II <- oecd %>% filter(Source == "OECD 2020") %>%
  # filter(Categ_instrmnt != "Impact investment") %>%
  filter(Sector_econAct != "total")
oecd2020_II_totals <- oecd2020_II %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
oecd2020_II_totals$year <- 2020
oecd2020_II_totals

# OECD 2021 (biodiversity natural capital and economy) ----
# it seems to me that this report is more focused on the variety of instruments 
# rather than accounting for total numbers
# this makes it hard to be sure that these sums are accurate...

oecd2021 <- oecd %>% filter(Source == "OECD 2021")
# i need to get rid of this row because it is only the taxes from G7 countries (excluding canada), and so it would double-count
oecd2021 <- oecd2021[-which(oecd2021$Categ_instrmnt == "Taxes" & oecd2021$Value_lowerLim == 2.2),]
# out of the green bonds, i only want to keep the estimate of 10.3 because the 257 is ALL green bonds, and the 10 is just what is for BD
oecd2021 <- oecd2021[-which(oecd2021$Categ_instrmnt == "Green bonds" & oecd2021$Value_lowerLim != 10.308),]

oecd2021_totals <- oecd2021 %>% 
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
oecd2021_totals$year <- 2021 # private and mixed sectors are now a lot larger than before
# because they include things like the value of ecotourism markets, the global value of green bond markets, expenditure towards restoration projects
# and btw, the public sector is also larger bc this includes the revenue generated from bd-targeted taxes


# OECD 2022 (climate finance provided and mobilised...) ----
oecd2022 <- oecd %>% filter(Source == "OECD 2022") 
# this is just the total provided and mobilized by developed countries for climate action 
# in developing countries in 2020 (which was, 16.7 billion short of the 2020 goal of 100B)

# summarize OECD
oecd2020_totals
oecd2020_II_totals
oecd2021_totals
# decided to exclude 2022 bc that's just about climate
# and include the estimate that includes impact investment bc other sources do too
summOECD <- rbind(oecd2020_II_totals, oecd2021_totals) 
summOECD$Source <- "OECD"

# Deutz 2020 (Paulson Institute) ----
deutz <- data %>% 
  filter(Categ_impact == "Positive") %>% 
  filter(Source == "Deutz 2020") %>%
  filter(Sector_econAct != "total")

deutz_totals <- deutz %>%
  group_by(Sector) %>%
  summarize(totalUSD = sum(meanUSD_Y), totalUSD_L = sum(Value_lowerLim), totalUSD_U = sum(Value_upperLim))
deutz_totals # the mixed category here indicated public-private mix - which can be compared to the OECD 2021 data
deutz_totals$year <- 2020
deutz_totals$Source <- "Paulson Institute et al."

# summarize these totals ----
summUNEP
summOECD
deutz_totals
totalsSummarized <- rbind(summUNEP, summOECD, deutz_totals)

# Total financing by Sector and Source ----
totalsSummarizedPlot <- totalsSummarized %>%
  ggplot(aes(x = year, y = totalUSD, fill = Sector)) +
  geom_col(position = "dodge") +
  geom_errorbar(data = totalsSummarized[which(totalsSummarized$totalUSD_L < totalsSummarized$totalUSD_U),],
                aes(ymin = totalUSD_L  , ymax = totalUSD_U   ), color = "gray25", width = 0.2, position = position_dodge(width = 0.7)) +
  labs(title = "Estimated financial flows to Nature by source", x = element_blank(), y = "$ Billions annually") +
  scale_fill_manual(values = c("Private" = "#A0AF67", "Public" = "#C3773E", "Mixed" = "#8B78A1")) +  
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source)
totalsSummarizedPlot

# write out this part
setwd(paste0(wdmain, "outputs/"))
svg(filename = "EstimatedFinancialFlowsbySource.svg", width = 12, height = 4)
totalsSummarizedPlot
dev.off()

# SUPER GRAPH that includes all positive flows----
# illustrate the various categories that these data have
# however, i've decided to deal with this data differently given the gaps and inconsistencies
data2 <- rbind(unepData2021, unepData2022, unepData2023, oecd2020_II, oecd2021, deutz)
summary(data2)

# attempt 2 at the super graph: ----
positiveData <- data2
positiveData$Source2 <- gsub(" ", "", stringr::str_match(positiveData$Source, "^(\\D+)(\\d+)")[,2])
positiveData$Source2 <- gsub("Deutz", "Paulson Institute et al.", positiveData$Source2)
positiveData$Year2 <- as.integer(stringr::str_match(positiveData$Source, "^(\\D+)(\\d+)")[,3])

# Public, private, and mixed sources of financing over the years ----

# summ the fin flows before to make it easier for ggplot
sectorPlotData <- positiveData %>%
  group_by(Source2, Year2, Sector) %>%
  summarize(meanUSD_Y = sum(meanUSD_Y), Value_lowerLim = sum(Value_lowerLim), Value_upperLim = sum(Value_upperLim))

sectorPlot <- sectorPlotData  %>%
  ggplot(aes(x = Year2, y = meanUSD_Y, fill = Sector)) +
  geom_col(position = "dodge") +
  # the error bars need to be the sum of the rows that fulfill those conditions
  geom_errorbar(data = sectorPlotData[which(sectorPlotData$Value_lowerLim  < sectorPlotData$Value_upperLim),],
                aes(ymin = Value_lowerLim, ymax = Value_upperLim), color = "gray25", width = 0.2, position = position_dodge(width = 0.8)) +
  labs(title = "Estimated financial flows to Nature by source", x = element_blank(), y = "$ Billions annually") +
  # scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023)) +
  # coord_cartesian(xlim = c(2020, 2023)) +
  scale_fill_manual(values = c("Private" = "#A0AF67", "Public" = "#C3773E", "Mixed" = "#8B78A1")) +  # Custom fill colors
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source2) 
sectorPlot

# check its the same as before:
plot_grid(totalsSummarizedPlot, sectorPlot, nrow = 2) #yes!

# write out a copy
setwd(paste0(wdmain, "outputs/"))
svg(filename = "EstimatedFinancialFlowsbySource_v2.svg", width = 12, height = 4)
sectorPlot
dev.off()


#  certainty ----
certaintyPlotData <- positiveData %>%
  group_by(Source2, Year2, Certainty) %>%
  summarize(meanUSD_Y = sum(meanUSD_Y), Value_lowerLim = sum(Value_lowerLim), Value_upperLim = sum(Value_upperLim))

certaintyPlotData$Certainty <- factor(certaintyPlotData$Certainty, levels = c("quantified", "high", "medium", "low", "unknown"))

certaintyPlot <-  certaintyPlotData %>%
  ggplot(aes(x = Year2, y = meanUSD_Y, fill = Certainty)) +
  geom_col() +
  labs(title = "Reported certainty levels for estimated financial flows to Nature", x = element_blank(), y = "$ Billions annually") +
  scale_fill_manual(values = c("low" = "#B65719", "medium" = "#D9AA80", "high" = "#F9E855", "unknown" = "#ACABA4", "quantified" = "#D5B41F")) +  
  # scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023)) +
  # coord_cartesian(xlim = c(2020, 2023)) +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source2)
certaintyPlot

setwd(paste0(wdmain, "outputs/"))
svg(filename = "EstimatedFinancialFlowsbyCertainty.svg", width = 12, height = 4)
certaintyPlot
dev.off()

# EDITS to CATEGORIES OF INSTRUMENTS 20.08.2024 ----

positiveData[which(positiveData$Categ_instrmnt == "Domestic budgets/Taxes"),] # only that one from Paulson institute
positiveData$Categ_instrmnt <- gsub("Domestic budgets/Taxes", "Domestic budgets", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("Government support/subsidies", "Subsidies", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("Green bonds/loans", "Green bonds", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("Multiple", "Other", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("Voluntary carbon markets", "Ecosystem services markets\n(including carbon)", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("Carbon markets", "Ecosystem services markets\n(including carbon)", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("PES", "Ecosystem services markets\n(including carbon)", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("International budgets", "ODA", positiveData$Categ_instrmnt)
positiveData$Categ_instrmnt <- gsub("Private equity impact spending", "Impact investment", positiveData$Categ_instrmnt)
length(unique(positiveData$Categ_instrmnt))

instrumentsPlotData <- positiveData %>%
  group_by(Source2, Year2, Categ_instrmnt) %>%
  summarize(meanUSD_Y = sum(meanUSD_Y), Value_lowerLim = sum(Value_lowerLim), Value_upperLim = sum(Value_upperLim))
instrumentsPlotData$Categ_instrmnt

my_nx_col <- c("#C6D68A","#C3773E","#799336",     
               "#196C71","#4A928F","#A7C6C5",     
               "#4D2D71","#8B78A1","#BAB0C9",    
               "#791E32","#ACABA4","#EDD018",
               "#d14765","#D9AA80","#f0c2cc") 

# could make the the categories a factor that would be ordered by the size of financing - but leave this for later

instrumentsPlot <-  instrumentsPlotData %>%
  ggplot(aes(x = Year2, y = meanUSD_Y, fill = Categ_instrmnt)) +
  geom_col() +
  labs(title = "Estimated financial flows to Nature disaggregated by instrument", x = element_blank(), y = "$ Billions annually", fill = "Instrument") +
  scale_fill_manual(values = my_nx_col) +  
  # scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023)) +
  # coord_cartesian(xlim = c(2020, 2023)) +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "gray80"))+
  facet_wrap(~Source2)
instrumentsPlot 

# write out 
setwd(paste0(wdmain, "outputs/"))
svg(filename = "EstimatedFinancialFlowsbyInstruments.svg", width = 12, height = 4)
instrumentsPlot
dev.off()


# One large graph with all three views on these data: ----
setwd(paste0(wdmain, "outputs/"))
png(filename = "DataQualityPlot.png", res = 300, width = 32, height = 37, units = "cm")

plot_grid(sectorPlot, certaintyPlot, instrumentsPlot, nrow = 3, 
          rel_widths = c(1,1,1.2), labels = c("A", "B", "C"), align = "hv")
dev.off()

# this data should also be written so that it can be used to make the treemaps
setwd(paste0(wdmain, "data/"))
write.csv(positiveData, "positiveFinancialFlows_clean.csv", row.names = F)

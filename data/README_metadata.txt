# Latest update of data on 28.09.2023
# Data gathering, collection, categorization by Andrea Pacheco (pacheco.gracia@gmail.com)

#### BD_allFinanceFlows: data table on the volumes of biodiversity financing
Categ: (BD_fin) all data here are in the biodiversity financing category
Categ_impact: (positive, negative, mixed) whether the financial flow impacts biodiversity positively (i.e. biodiv conservation) or negatively (actively harming biodiversity), or mixed, can have both kinds of impact (usually because it's an estimate of global financial flows)
Sector: (public, private, or mixed)
Categ_instrmnt: here are many categories of instruments used to disperse financing, from domestic budgets and taxes to subsidies, to impact investment or green bonds. 
Sector_econact: (agriculture, tourism, fisheries, fossil fuels, forestry, water, or total). This refers to the sector of economic activity. Total estimates have tried to take all sectors into account. 
Value_lowerLim, Value_upperLim: the ANNUAL amount estimated for this unit of observation. where, available both upper and lower limits included. where not available, only lower limit filled in, as most estimates of these figures state these are likely underestimated values.
Unit (USD/YY): (billions USD) the unit has been kept standard as billions of USD
NormalizedValue_YY: the year which the report says this money has been normalized for inflation. When this was not clear from the text of the report, the year of the publishing of the report was used. This should be used to standardize/normalize all values to 202(4?) values for the publishing of the assessment.

#### BD_positiveFinanceFlows-NexusCodes: subset of the data table on the volumes of biodiversity financing to only positive flows. 
Variables follow BD_allFinanceFlows, but 5 more columns included for each nexus element (biodiversity, food, water, health, climate). 
I categorized 



#### BD_gap: data table on the gaps in biodiversity financing 
Categ: (BD_gap) meaning all data here are about the biodiversity financing gap
Sector: (public, private, mixed) describes whether financing comes from public (governments) or private sectors, or a mix of both
Value_lowerLim, Value_upperLim: the ANNUAL amount estimated for this unit of observation. where, available both upper and lower limits included. where not available, only lower limit filled in, as most estimates of these figures state these are likely underestimated values.
Unit (USD/YY): (billions USD) the unit has been kept standard as billions of USD
NormalizedValue_YY: the year which the report says this money has been normalized for inflation. When this was not clear from the text of the report, the year of the publishing of the report was used. This should be used to standardize/normalize all values to 202(4?) values for the publishing of the assessment.
Categ_gap: refers to if the estimate is the total (total) amount needed to fulfill current needs, or if the estimate refers to the gap (gap): the difference between the amount currently spent and the amount needed
Categ_temp: whether the estimate was for current or future needs


### update Feb 2024
I'm trying to (pragmatically) think through what needs to be done -
the latest State of Finance for Nature essentially does a giant treemap, as a summary of the report at the beginning. 
What are our options?
- replicate
- expand
- nexify-it - but how?

1. document the data they've added - which will be of use in the sankey diagram as well

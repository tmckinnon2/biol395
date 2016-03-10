# Analysis of experimental exclosure data
# Read in data
setwd("~/Desktop/insect-exclosure")
exclosure <- read.table("exclosure_experiment_2012.txt", sep= '\t', quote="\"", header=TRUE)

# Remove Unnecessary Columns
library("dplyr")
topdown <- filter(exclosure, Year == 2012)
topdown1<- select(topdown, -LitterDepth, -TapeExposed_in, -Strip_length_in, -Destroyed., -circle, -Year)

# Add Column combining StateRouteStop and Station
topdown1$StateRouteStop_Station<- paste0(topdown1$StateRouteStop, topdown1$Station)

# Gather Arthropod Size Data into a Single Column
library(tidyr)
topdown2 <- gather(topdown1, Size, Abundance, X0.2mm:X.20mm)
topdown3 <- topdown2[topdown2$Abundance >0, ]

# Identify All Dates for Which There Are VFX Samples 
trapdates = data.frame(table(topdown3[, c('TrapType', 'Date')]))
trapdates1 <- filter(trapdates, trapdates$Freq>0)
trapdates3 <- filter(trapdates1,trapdates1$TrapType == "VFX")

#Subset Dataset to Dates VFX is Present
topdown4 <- filter(topdown3, Date == "5/20/2012"|Date == "5/21/2012"|Date == "5/22/2012"
                   |Date == "5/24/2012"|Date == "5/25/2012"|Date == "5/26/2012"|Date == "5/27/2012"
                   |Date == "5/28/2012"|Date == "5/29/2012"|Date == "5/30/2012"|Date == "6/19/2012"
                   |Date == "6/20/2012"|Date == "6/21/2012"|Date == "6/22/2012"|Date == "6/23/2012"
                   |Date == "6/24/2012"|Date == "6/25/2012"|Date == "6/26/2012"|Date == "6/27/2012"
                   |Date == "7/2/2012")

#Create List of VFX Entries 
topdown5 <- filter(topdown4, TrapType == "VFX")
topdown6 <- select(topdown5, StateRouteStop_Station, Date)

#Create a Unique Identifier Column to Filter by Date and Locations in Overall Dataset that Match Those in VFX
topdown6$identifier <- paste0(topdown6$StateRouteStop_Station, topdown6$Date)
topdown4$identifier <- paste0(topdown4$StateRouteStop_Station, topdown4$Date)
topdown7 <- filter(topdown4, topdown4$identifier %in% topdown6$identifier)

#Count Number of Observations For Each Date/Location
observationfrequency <- count(topdown7, identifier)








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

# Subset Full Dataset to those Dates 
topdown4 <- filter(topdown3, Date == "5/20/2012"|Date == "5/21/2012"|Date == "5/22/2012"
       |Date == "5/24/2012"|Date == "5/25/2012"|Date == "5/26/2012"|Date == "5/27/2012"
       |Date == "5/28/2012"|Date == "5/29/2012"|Date == "5/30/2012"|Date == "6/19/2012"
       |Date == "6/20/2012"|Date == "6/21/2012"|Date == "6/22/2012"|Date == "6/23/2012"
       |Date == "6/24/2012"|Date == "6/25/2012"|Date == "6/26/2012"|Date == "6/27/2012"
       |Date == "7/2/2012")
#Subset Dataset to Only Include Observations at Locations When both VF and VFX are Present 



#toy example of using gather() from tidyr package
library(tidyr)
foo = topdown1[, c(2, 11:16)]
foo2 = gather(foo, size, abundance, X0.2mm:X.20mm)
foo3 = foo2[foo2$abundance > 0, ]

dim(foo2)
dim(foo3)
gather(topdown1, size, abundance, X0.2mm:X.20mm)

# Counting number of records per category or combination of categories
table(topdown1$Station)
data.frame(table(topdown1$Station))

trapcount = data.frame(table(topdown1[, c('TrapType', 'Date',)]))
trapcount = trapcount[trapcount$Freq > 0,]




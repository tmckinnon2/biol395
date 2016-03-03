# Analysis of experimental exclosure data
# Read in data
setwd("~/Desktop/insect-exclosure")
exclosure <- read.table("exclosure_experiment_2012.txt", sep= '\t', quote="\"", header=TRUE)

# Remove Unnecessary Columns
library("dplyr")
topdown <- filter(exclosure, Year == 2012)
topdown1<- select(topdown, -LitterDepth, -TapeExposed_in, -Strip_length_in, -Destroyed., -circle, -Year)

# Add Column with Total # of Arthropods
topdown1$sumarth = rowSums(topdown1[, c("X0.2mm","X2.5mm","X5.10mm", "X10.20mm", "X.20mm")])

# Add Column combining StateRouteStop and Station
topdown1$StateRouteStop_Station<- paste0(topdown$StateRouteStop, topdown$Station)

# Find all rows with an observation in more than 1 size category
arth0.2 <- filter(topdown1, X0.2mm >0, X2.5mm > 0 | X5.10mm > 0 | X10.20mm > 0 | X.20mm > 0)
arth2.5 <- filter(topdown1, X2.5mm >0, X5.10mm > 0 | X10.20mm > 0 | X.20mm > 0)
arth5.10 <- filter(topdown1,  X5.10mm > 0, X10.20mm > 0 | X.20mm > 0)
arth10.20 <- filter(topdown1,   X10.20mm > 0, X.20mm > 0)


#toy example of using gather() from tidyr package
library(tidyr)
foo = topdown1[, c(2, 11:16)]
foo2 = gather(foo, size, abundance, X0.2mm:X.20mm)
foo3 = foo2[foo2$abundance > 0, ]

dim(foo2)
dim(foo3)

# Counting number of records per category or combination of categories
table(topdown1$Station)
data.frame(table(topdown1$Station))

trapcount = data.frame(table(topdown1[, c('TrapType', 'Date')]))
trapcount = trapcount[trapcount$Freq > 0,]

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
topdown1$StateRouteStop_Station<- with(topdown1, paste0(StateRouteStop, Station))

# Find all rows with an observation in more than 1 size category
arth0.2 <- filter(topdown1, X0.2mm >0, X2.5mm > 0 | X5.10mm > 0 | X10.20mm > 0 | X.20mm > 0)
arth2.5 <- filter(topdown1, X2.5mm >0, X5.10mm > 0 | X10.20mm > 0 | X.20mm > 0)
arth5.10 <- filter(topdown1,  X5.10mm > 0, X10.20mm > 0 | X.20mm > 0)
arth10.20 <- filter(topdown1,   X10.20mm > 0, X.20mm > 0)



# Analysis of experimental exclosure data

# Read in data
setwd("~/Desktop/insect-exclosure")
exclosure <- read.table("exclosure_experiment_2012.txt", sep= '\t', quote="\"", header=TRUE)
# Remove Unnecessary Columns
library("dplyr")
exclosure1 <- select(exclosure, -Strip_length_in)

# Subset to data collected in Circle 3, with Visual Surveys, in 2012
exclosure2 <- filter(exclosure1, circle == "3" & TrapType == "VF")
exclosure3 <- filter(exclosure2, Year == 2012)

# Calculate arthropod density by Station
  #New Column with sum arthropod data
    #ArthropodCount <- select(exclosure3, 14:18)
    exclosure3$sumarth = rowSums(exclosure[, c('X0.2', 'X2.5')])
  arthdensity = aggregate(exclosure3$sumarth, b
                          y = list(Station = exclosure3$Station), 
                          FUN = mean)




  


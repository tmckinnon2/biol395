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

#Add Back "0" Observations from Original Data
no_observations <- filter(topdown1, X0.2mm==0 & X2.5mm==0 & X5.10mm==0 & X10.20mm==0 & X.20mm==0)
no_observations1 <- select(no_observations, -X0.2mm, -X2.5mm, -X5.10mm, -X10.20mm, -X.20mm)
no_observations1$Abundance = 0
no_observations1$Size = NA
class(no_observations1$Size)="character"
class(no_observations1$Abundance)="integer"
complete_observations <- union(no_observations1,topdown3)


# Identify All Dates for Which There Are VFX Samples 
trapdates = data.frame(table(complete_observations[, c('TrapType', 'Date')]))
trapdates1 <- filter(trapdates, trapdates$Freq>0)
trapdates3 <- filter(trapdates1,trapdates1$TrapType == "VFX")

#Create List of VFX Entries 
topdown5 <- filter(complete_observations, TrapType == "VFX")
topdown6 <- select(topdown5, StateRouteStop_Station, Date)

#Create an Identifier Column to Filter by Date and Locations in Overall Dataset that Match Those in VFX
topdown6$identifier <- paste0(topdown6$StateRouteStop_Station, topdown6$Date)
complete_observations$identifier <- paste0(complete_observations$StateRouteStop_Station, complete_observations$Date)
topdown7 <- filter(complete_observations, complete_observations$identifier %in% topdown6$identifier)

#Add unique survey ID
topdown7$surveyID<- paste0(topdown7$StateRouteStop, topdown7$Date, topdown7$Station, 
                               topdown7$TrapType)

#Remove Observations with Missing Pair Data
topdown8 <- topdown7[topdown7$identifier!="82902433B5/22/2012",]

#Include Only Insects Large enough for Bird Consumption
topdown9 <- filter(topdown8, Size== c("X2.5mm", "X5.10mm", 
                   "X10.20mm","X.20mm"))

#Summarise Observations by for All Arthropods
grouped_all <- topdown9 %>% group_by(TrapType, StateRouteStop, Station, Date)
mean_abundance_all <- (summarise(grouped_all, mean(Abundance)))
mean_abundance_all$surveyID<- paste0(mean_abundance_all$StateRouteStop, mean_abundance_all$Date, 
                                              mean_abundance_all$Station, mean_abundance_all$TrapType)

#Summarise Observations for Relevant Orders ("Bird Food Arthropods") 
food_arthropods <- filter(topdown9, Order == c("LEPL", "COLE", "ARAN", "HETE", "ORTH", "AUCH"))
grouped_food <- food_arthropods %>% group_by(TrapType, StateRouteStop, Station, Date)
mean_abundance_food <- (summarise(grouped_food, mean(Abundance)))
mean_abundance_food$surveyID<- paste0(mean_abundance_food$StateRouteStop, mean_abundance_food$Date, 
                                              mean_abundance_food$Station, mean_abundance_food$TrapType)

#Summarise Observations for Caterpillars
caterpillars <-filter(topdown8, Order == "LEPL")
grouped_caterpillars <- caterpillars %>% group_by(TrapType, StateRouteStop, Station, Date)
mean_abundance_caterpillars <- (summarise(grouped_caterpillars, mean(Abundance)))
mean_abundance_caterpillars$surveyID<- paste0(mean_abundance_caterpillars$StateRouteStop, mean_abundance_caterpillars$Date, 
                                              mean_abundance_caterpillars$Station, mean_abundance_caterpillars$TrapType)

#Create Unique Surveys Dataframe
unique_surveys<-unique(topdown7[, c("StateRouteStop", "Date", "Station", "TrapType")])
unique_surveys$surveyID<- paste0(unique_surveys$StateRouteStop, unique_surveys$Date, unique_surveys$Station, 
                                       unique_surveys$TrapType)
unique_surveys_count <- data.frame(table(unique_surveys[, c("StateRouteStop", "Date", "Station", "TrapType", "surveyID")]))
unique_surveys_count = unique_surveys_count[unique_surveys_count$Freq> 0,]

#Merge Summary Observations for 3 Food Group Types with Total Possible Survey Dates
meanarthabundance <- merge(unique_surveys_count, mean_abundance_caterpillars, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
meanarthabundance1<- select(meanarthabundance, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -Date.y)
colnames(meanarthabundance1)[colnames(meanarthabundance1)=="mean(Abundance)"] <- "mean_abundance_caterpillars"

meanarthabundance2 <- merge(meanarthabundance1, mean_abundance_all, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
meanarthabundance3<- select(meanarthabundance2, -TrapType, -StateRouteStop, -Station, -Date)
colnames(meanarthabundance3)[colnames(meanarthabundance3)=="mean(Abundance)"] <- "mean_abundance_all"

meanarthabundance4 <- merge(meanarthabundance3, mean_abundance_food, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
meanarthabundance5<- select(meanarthabundance4, -TrapType, -StateRouteStop, -Station, -Date)
colnames(meanarthabundance5)[colnames(meanarthabundance5)=="mean(Abundance)"] <- "mean_abundance_food"

#Spread VF and VFX data into separate columns
meanarthabundance6 <- select(meanarthabundance5, -surveyID)

#Code to Look at Data Errors
dataframename$TrapType [df$RecordID %in% c(10631,10632,10633)]

#Dr. Hurlbert's Code for Finding Frequency of Plant species
stationplants = unique(topdown1[, c('StateRouteStop', 'Station', 'TrapType', 'Date', 'TreeSpecies')])
plantspCount = data.frame(table(stationplants[, c('StateRouteStop', 'Station', 'TrapType', 'TreeSpecies')]))
plantspCount = plantspCount[plantspCount$Freq > 0,]
plantspCount = plantspCount[order(plantspCount$StateRouteStop, plantspCount$Station, plantspCount$TrapType),]





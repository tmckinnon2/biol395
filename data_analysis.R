# Analysis of experimental exclosure data
# Read in data
setwd("~/Desktop/insect-exclosure")
exclosure <- read.table("exclosure_experiment_2012_TM_corrected.txt", sep= '\t', quote="\"", header=TRUE)

# Remove Unnecessary Columns 
library("dplyr")
topdown <- filter(exclosure, Year == 2012)
topdown1<- select(topdown, -LitterDepth, -TapeExposed_in, -Strip_length_in, -circle, -Year)

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


# Narrow down to stateroutestop-stations with sampling in both visits 1 and 3
topdown8 <- filter(topdown7, VisitNumber %in% c(1,3))
uniqSiteVisits = unique(topdown8[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber')])
visitcount = data.frame(table(uniqSiteVisits[, c('StateRouteStop', 'Station')]))
goodstations = visitcount[visitcount$Freq == 4,] #Expect 2 recs per TrapType times 2 diff dates = 4
goodstations$StateRouteStop_Station = paste(goodstations$StateRouteStop, goodstations$Station, sep = '')
topdown9 = topdown8[topdown8$StateRouteStop_Station %in% goodstations$StateRouteStop_Station, ]

#Include Only Insects Large enough for Bird Consumption
topdown10 <- filter(topdown9, Size %in% c("X2.5mm", "X5.10mm", 
                                         "X10.20mm","X.20mm"))
###########################################################

#Summarise Observations by for All Arthropods
grouped_all <- topdown10 %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
total_all <- (summarise(grouped_all, sum(Abundance)))
total_all$surveyID<- paste0(total_all$StateRouteStop, total_all$VisitNumber,
                                       total_all$Station, total_all$TrapType)


#Summarise Observations for Relevant Orders ("Bird Food Arthropods") 
food_arthropods <- filter(topdown10, Order %in% c("LEPL", "COLE", "ARAN", "HETE", "ORTH", "AUCH"))
grouped_food <- food_arthropods %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
total_food <- (summarise(grouped_food, sum(Abundance)))
total_food$surveyID<- paste0(total_food$StateRouteStop, total_food$VisitNumber, 
                                              total_food$Station, total_food$TrapType)

#Summarise Observations for Caterpillars
caterpillars <-filter(topdown9, Order == "LEPL")
grouped_caterpillars <- caterpillars %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
total_caterpillars <- (summarise(grouped_caterpillars, sum(Abundance)))
total_caterpillars$surveyID<- paste0(total_caterpillars$StateRouteStop, total_caterpillars$VisitNumber, 
                                              total_caterpillars$Station, total_caterpillars$TrapType)

#Create Unique Surveys Dataframe
unique_surveys<-unique(topdown9[, c("StateRouteStop", "VisitNumber", "Station", "TrapType")])
unique_surveys$surveyID<- paste0(unique_surveys$StateRouteStop, unique_surveys$VisitNumber, unique_surveys$Station, 
                                       unique_surveys$TrapType)
unique_surveys_count <- data.frame(table(unique_surveys[, c("StateRouteStop", "VisitNumber", "Station", "TrapType", "surveyID")]))
unique_surveys_count = unique_surveys_count[unique_surveys_count$Freq> 0,]


#Merge Summary Obsv. for 3 Food Types with Unique Surveys to Create 3 New Dataframes
all_abundance <- merge(unique_surveys_count, total_all, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
all_abundance1<- select(all_abundance, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(all_abundance1) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_all")
all_abundance1[is.na(all_abundance1)] <- 0


food_abundance <- merge(unique_surveys_count, total_food, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
food_abundance1<- select(food_abundance, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(food_abundance1) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_food")
food_abundance1[is.na(food_abundance1)] <- 0


caterpillar_abundance <- merge(unique_surveys_count, total_caterpillars, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
caterpillar_abundance1<- select(caterpillar_abundance, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(caterpillar_abundance1) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_caterpillars")
caterpillar_abundance1[is.na(caterpillar_abundance1)] <- 0


#Spread VF and VFX to Columns in 3 Food Type Datasets
all_abundance2 <- spread(all_abundance1, TrapType, total_all)
food_abundance2 <- spread(food_abundance1, TrapType, total_food)
caterpillar_abundance2 <-spread(caterpillar_abundance1, TrapType, total_caterpillars)

#Run Wilcoxon Test for 3 Food Type Datasets Using VF and VFX direct comparison
wilcox.test(x=all_abundance2$VF,y=all_abundance2$VFX, paired=TRUE)
wilcox.test(x=food_abundance2$VF,y=food_abundance2$VFX, paired=TRUE)
wilcox.test(x=caterpillar_abundance2$VF,y=caterpillar_abundance2$VFX, paired=TRUE)

#Spread Visit 1 and Visit 3 for 3 Food Type Datasets and Create Difference Column
all_time <- spread(all_abundance1,VisitNumber, total_all)
names(all_time) = c('StateRouteStop','Station', "TrapType", "Visit1", "Visit3")
all_time$visit_dif<-all_time$Visit3-all_time$Visit1

food_time <- spread(food_abundance1, VisitNumber, total_food)
names(food_time) = c('StateRouteStop','Station', "TrapType", "Visit1", "Visit3")
food_time$visit_dif<-food_time$Visit3-food_time$Visit1

caterpillar_time <- spread(caterpillar_abundance1, VisitNumber, total_caterpillars)
names(caterpillar_time) = c('StateRouteStop','Station', "TrapType", "Visit1", "Visit3")
caterpillar_time$visit_dif<-caterpillar_time$Visit3-caterpillar_time$Visit1

#Run wilcox_test
wilcox_test(visit_dif ~ TrapType, data=all_time)
wilcox_test(visit_dif ~ TrapType, data=food_time)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time)

#Code to Look at Data Errors
dataframename$TrapType [df$RecordID %in% c(10631,10632,10633)]

#Dr. Hurlbert's Code for Finding Frequency of Plant species
stationplants = unique(topdown1[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber', 'TreeSpecies')])
plantspCount = data.frame(table(stationplants[, c('StateRouteStop', 'Station', 'TrapType', 'TreeSpecies')]))
plantspCount = plantspCount[plantspCount$Freq > 0,]
plantspCount = plantspCount[order(plantspCount$StateRouteStop, plantspCount$Station, plantspCount$TrapType),]





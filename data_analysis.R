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


#Summarise Observations for Relevant Orders ("Bird food Arthropods") 
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


#Merge Summary Obsv. for 3 food Types with Unique Surveys to Create 3 New Dataframes
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


#Spread VF and VFX to Columns in 3 food Type Datasets
all_abundance2 <- spread(all_abundance1, TrapType, total_all)
food_abundance2 <- spread(food_abundance1, TrapType, total_food)
caterpillar_abundance2 <-spread(caterpillar_abundance1, TrapType, total_caterpillars)

#Run Wilcoxon Test for 3 food Type Datasets Using VF and VFX direct comparison
wilcox.test(x=all_abundance2$VF,y=all_abundance2$VFX, paired=TRUE)
wilcox.test(x=food_abundance2$VF,y=food_abundance2$VFX, paired=TRUE)
wilcox.test(x=caterpillar_abundance2$VF,y=caterpillar_abundance2$VFX, paired=TRUE)

#Spread Visit 1 and Visit 3 for 3 food Type Datasets and Create Difference Column
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
library("coin")
wilcox_test(visit_dif ~ TrapType, data=all_time)
wilcox_test(visit_dif ~ TrapType, data=food_time)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time)

#Subset Food Type Datasets to Include Only Data where TreeSpecies 
#is the same for VF and VFX
stationplants = unique(topdown9[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber', 'TreeSpecies')])
plantspCount = data.frame(table(stationplants[, c('StateRouteStop', 'Station', 'TrapType', 'TreeSpecies')]))
plantspCount = plantspCount[plantspCount$Freq > 0,]
plantspCount = plantspCount[order(plantspCount$StateRouteStop, plantspCount$Station, plantspCount$TrapType),]
plantspCount1 <- filter(plantspCount, Freq==2) 
plantspCount2 <- spread(plantspCount1, key=TrapType, value=TreeSpecies)
plantspCount3 <- filter(plantspCount2, VF==VFX)
plantspCount3$ID <- paste(plantspCount3$StateRouteStop, plantspCount3$Station)

paired_all <- all_time
paired_all$ID <- paste(paired_all$StateRouteStop, paired_all$Station)
paired_all1 <- filter(paired_all, paired_all$ID %in% plantspCount3$ID)

paired_food <- food_time
paired_food$ID <- paste(paired_food$StateRouteStop, paired_food$Station)
paired_food1 <- filter(paired_food, paired_food$ID %in% plantspCount3$ID)

paired_caterpillar <- caterpillar_time
paired_caterpillar$ID <- paste(paired_caterpillar$StateRouteStop, paired_caterpillar$Station)
paired_caterpillar1 <- filter(paired_caterpillar, paired_caterpillar$ID %in% plantspCount3$ID)

#Spread VF and VFX for 3 food Type Datasets w/ Visit Numbers 
##All food Group
all_time1 <- select(all_time, -Visit3, -visit_dif)
all_Visit1<- spread(all_time1, TrapType, Visit1)
names(all_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
all_Visit1$ID <- paste(all_Visit1$StateRouteStop, all_Visit1$Station)

all_time2 <- select(all_time, -Visit1, -visit_dif)
all_Visit3<- spread(all_time2, TrapType, Visit3)
names(all_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
all_Visit3$ID <- paste(all_Visit3$StateRouteStop, all_Visit3$Station)

all_time3 <- merge(all_Visit1, all_Visit3, by.x="ID", by.y="ID")
all_time4 <- select(all_time3, -StateRouteStop.y, -Station.y, -ID)
names(all_time4) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

##Bird food Group
food_time1 <- select(food_time, -Visit3, -visit_dif)
food_Visit1<- spread(food_time1, TrapType, Visit1)
names(food_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
food_Visit1$ID <- paste(food_Visit1$StateRouteStop, food_Visit1$Station)

food_time2 <- select(food_time, -Visit1, -visit_dif)
food_Visit3<- spread(food_time2, TrapType, Visit3)
names(food_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
food_Visit3$ID <- paste(food_Visit3$StateRouteStop, food_Visit3$Station)

food_time3 <- merge(food_Visit1, food_Visit3, by.x="ID", by.y="ID")
food_time4 <- select(food_time3, -StateRouteStop.y, -Station.y, -ID)
names(food_time4) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

##Caterpillar food Group
caterpillar_time1 <- select(caterpillar_time, -Visit3, -visit_dif)
caterpillar_Visit1<- spread(caterpillar_time1, TrapType, Visit1)
names(caterpillar_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
caterpillar_Visit1$ID <- paste(caterpillar_Visit1$StateRouteStop, caterpillar_Visit1$Station)

caterpillar_time2 <- select(caterpillar_time, -Visit1, -visit_dif)
caterpillar_Visit3<- spread(caterpillar_time2, TrapType, Visit3)
names(caterpillar_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
caterpillar_Visit3$ID <- paste(caterpillar_Visit3$StateRouteStop, caterpillar_Visit3$Station)

caterpillar_time3 <- merge(caterpillar_Visit1, caterpillar_Visit3, by.x="ID", by.y="ID")
caterpillar_time4 <- select(caterpillar_time3, -StateRouteStop.y, -Station.y, -ID)
names(caterpillar_time4) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

#Example Change in Abundance Over Time Graph (for one StateRouteStop-Station)
ex_graph_data <-filter(all_abundance2, StateRouteStop == "8890236", Station== "3B")
ex_graph_data$VisitNumber = as.numeric(as.character(ex_graph_data$VisitNumber))
plot(x=ex_graph_data$VisitNumber, y=ex_graph_data$VFX, xlim=range(c(0,4)), ylim=range(c(1,5)), 
            xlab="Visit Number", ylab="Arthropod Abundance", type="b", col="red", main="Example Change In Arthropod Abundance Over Visits")
points(x=ex_graph_data$VisitNumber, y=ex_graph_data$VF, type="b", col="blue")

# Plotting with a for loop
all_abundance2$StateRouteStopStation = paste(all_abundance2$StateRouteStop, all_abundance2$Station, sep = '')

uniqStations = unique(all_abundance2$StateRouteStopStation)
temp <- filter(all_abundance2, StateRouteStop == as.numeric(substr(uniqStations[1], 1, 7)), 
               Station == substr(uniqStations[1], 8, 9))
plot(temp$VisitNumber, temp$VFX, xlim=c(0,4), ylim=c(0, 20), xaxt = "n", 
     xlab="Visit Number", ylab="Arthropod Abundance", type="b", col="red")
points(x=temp$VisitNumber, y=temp$VF, type="b", col="blue")

for (station in uniqStations[2:length(uniqStations)]) {
  temp <- filter(all_abundance2, StateRouteStop == as.numeric(substr(station, 1, 7)), 
                 Station == substr(station, 8, 9))
  points(temp$VisitNumber, temp$VFX, type="b", col="red")
  points(x=temp$VisitNumber, y=temp$VF, type="b", col="blue")
}

#Example Differences Graph
ex_graph_data1 <- filter(all_time, StateRouteStop == "8890236", Station== "3B")
barplot(height=ex_graph_data1$visit_dif, xlab="Trap Type", ylab="Change in Abundance", 
        main="Change in Abundance from Visit 1 to Visit 3", names.arg=c("VF", "VFX"), col="green")







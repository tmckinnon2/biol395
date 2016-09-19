# Analysis of experimental exclosure data from Southern Appalachians 2012

#Load Required Packages
library(dplyr)
library(tidyr)
library(coin)
library(lattice)
library(maps)

# Read in data
setwd("~/Desktop/insect-exclosure")
topdown <- read.table("exclosure_experiment_2012_TM_corrected.txt", sep= '\t', quote="\"", header=TRUE)

# Remove Unnecessary Columns 
topdown1<- select(topdown, -LitterDepth, -TapeExposed_in, -Strip_length_in, -circle, -Year)

# Add Column combining StateRouteStop and Station
topdown1$StateRouteStop_Station<- paste0(topdown1$StateRouteStop, topdown1$Station)

# Gather Arthropod Size Data into a Single Column
topdown2 <- gather(topdown1, Size, Abundance, X0.2mm:X.20mm)
topdown3 <- topdown2[topdown2$Abundance >0, ]

#Add Back "0" Observations from Original Data
##Identify "0" Observations from Original Data
no_observations <- dplyr::filter(topdown1, X0.2mm==0 & X2.5mm==0 & X5.10mm==0 & X10.20mm==0 & X.20mm==0)
##Format "0" Observations to Match topdown3
no_observations1 <- select(no_observations, -X0.2mm, -X2.5mm, -X5.10mm, -X10.20mm, -X.20mm)
no_observations1$Abundance = 0
no_observations1$Size = NA
class(no_observations1$Size)="character"
class(no_observations1$Abundance)="integer"

##Add Back "0" Observations to topdown3 to Create Complete Dataset
complete_observations <- dplyr::union(no_observations1, topdown3)

#Create List of Surveys with  VFX Entries 
topdown5 <- dplyr::filter(complete_observations, TrapType == "VFX")
topdown6 <- select(topdown5, StateRouteStop_Station, Date)

#Create an Identifier Column to filter by Date and Locations in Overall Dataset that Match Those in VFX
topdown6$identifier <- paste0(topdown6$StateRouteStop_Station, topdown6$Date)
complete_observations$identifier <- paste0(complete_observations$StateRouteStop_Station, complete_observations$Date)
topdown7 <- dplyr::filter(complete_observations, complete_observations$identifier %in% topdown6$identifier)

#Add Unique Survey ID
topdown7$surveyID<- paste0(topdown7$StateRouteStop, topdown7$Date, topdown7$Station, 
                               topdown7$TrapType)

# Narrow down to StateRouteStop-stations with Complete Sampling in Both Visits 1 and 3
##Filter to Visits 1 and 3, when both VFX and VF surveys were conducted
topdown8 <- dplyr::filter(topdown7, VisitNumber %in% c(1,3))
##Identify all unique StateRouteStop Station Visits
uniqSiteVisits <- unique(topdown8[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber')])
##Identify Stations for which there are 4 Unique Visits- signifies that data is present in both 
##VF and VFX for Visits 1 and 3
visitcount <- data.frame(table(uniqSiteVisits[, c('StateRouteStop', 'Station')]))
goodstations <- visitcount[visitcount$Freq == 4,] 
##Narrow Down topdown8 to Only Include StateRouteStop-stations with Complete Sampling in Both Visits 1 and 3
goodstations$StateRouteStop_Station <- paste(goodstations$StateRouteStop, goodstations$Station, sep = '')
topdown9 <- topdown8[topdown8$StateRouteStop_Station %in% goodstations$StateRouteStop_Station, ]

#Include Only Insects Large enough for Bird Consumption
topdown10 <- dplyr::filter(topdown9, Size %in% c("X5.10mm", 
                                         "X10.20mm","X.20mm"))
#Summarise Observations for 3 Food Types
#Summarise Observations for All Arthropods (Arthropods greater than 5 mm)
grouped_all_12 <- topdown10 %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
total_all_12 <- (dplyr::summarise(grouped_all_12, sum(Abundance)))
total_all_12$surveyID<- paste0(total_all_12$StateRouteStop, total_all_12$VisitNumber,
                                       total_all_12$Station, total_all_12$TrapType)


#Summarise Observations for Relevant Orders ("Bird food Arthropods") 
food_arthropods_12 <- dplyr::filter(topdown10, Order %in% c("LEPL", "COLE", "ARAN", "HETE", "ORTH", "AUCH"))
grouped_food_12 <- food_arthropods_12 %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
total_food_12 <- (dplyr::summarise(grouped_food_12, sum(Abundance)))
total_food_12$surveyID<- paste0(total_food_12$StateRouteStop, total_food_12$VisitNumber, 
                                              total_food_12$Station, total_food_12$TrapType)

#Summarise Observations for Caterpillars
caterpillars_12 <-dplyr::filter(topdown9, Order == "LEPL")
grouped_caterpillars_12 <- caterpillars_12 %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
total_caterpillars_12 <- (dplyr::summarise(grouped_caterpillars_12, sum(Abundance)))
total_caterpillars_12$surveyID<- paste0(total_caterpillars_12$StateRouteStop, total_caterpillars_12$VisitNumber, 
                                              total_caterpillars_12$Station, total_caterpillars_12$TrapType)

#Create Unique Surveys Dataframe
unique_surveys<-unique(topdown9[, c("StateRouteStop", "VisitNumber", "Station", "TrapType")])
unique_surveys$surveyID<- paste0(unique_surveys$StateRouteStop, unique_surveys$VisitNumber, unique_surveys$Station, 
                                       unique_surveys$TrapType)
unique_surveys_count <- data.frame(table(unique_surveys[, c("StateRouteStop", "VisitNumber", "Station", "TrapType", "surveyID")]))
unique_surveys_count = unique_surveys_count[unique_surveys_count$Freq> 0,]


#Create 3 New Dataframes Merging List of Unique Surveys with Summary Observations 
#for Each Unique Survey for Each of the 3 Food Types
all_abundance_12 <- merge(unique_surveys_count, total_all_12, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
all_abundance1_12<- select(all_abundance_12, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(all_abundance1_12) <- c('StateRouteStop','VisitNumber', "Station", "TrapType","total_all")
all_abundance1_12[is.na(all_abundance1_12)] <- 0

food_abundance_12 <- merge(unique_surveys_count, total_food_12, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
food_abundance1_12<- select(food_abundance_12, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(food_abundance1_12) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_food")
food_abundance1_12[is.na(food_abundance1_12)] <- 0

caterpillar_abundance_12 <- merge(unique_surveys_count, total_caterpillars_12, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
caterpillar_abundance1_12<- select(caterpillar_abundance_12, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(caterpillar_abundance1_12) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_caterpillars")
caterpillar_abundance1_12[is.na(caterpillar_abundance1_12)] <- 0


#Spread VF and VFX to Columns in 3 Food Type Datasets
all_abundance2_12 <- spread(all_abundance1_12, TrapType, total_all)
food_abundance2_12 <- spread(food_abundance1_12, TrapType, total_food)
caterpillar_abundance2_12 <-spread(caterpillar_abundance1_12, TrapType, total_caterpillars)

#Spread Visit 1 and Visit 3 for 3 Food Type Datasets and Create Difference Column to Format for Wilcox Test
all_time_12 <- spread(all_abundance1_12,VisitNumber, total_all)
names(all_time_12) = c('StateRouteStop','Station', "TrapType", "Visit1", "Visit3")
all_time_12$visit_dif<-all_time_12$Visit3-all_time_12$Visit1

food_time_12 <- spread(food_abundance1_12, VisitNumber, total_food)
names(food_time_12) = c('StateRouteStop','Station', "TrapType", "Visit1", "Visit3")
food_time_12$visit_dif<-food_time_12$Visit3-food_time_12$Visit1

caterpillar_time_12 <- spread(caterpillar_abundance1_12, VisitNumber, total_caterpillars)
names(caterpillar_time_12) = c('StateRouteStop','Station', "TrapType", "Visit1", "Visit3")
caterpillar_time_12$visit_dif<-caterpillar_time_12$Visit3-caterpillar_time_12$Visit1

#Run wilcox_test
wilcox_test(visit_dif ~ TrapType, data=all_time_12)
wilcox_test(visit_dif ~ TrapType, data=food_time_12)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time_12)

#Subset Food Type Datasets to Include Only Observations where TreeSpecies 
#is the Same for VF and VFX
##Identify Unique Combinations of Tree Species, StateRouteStop, Station, TrapType, and Visit Number that Exist in Data
stationplants <- unique(topdown9[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber', 'TreeSpecies')])
plantspCount <- data.frame(table(stationplants[, c('StateRouteStop', 'Station', 'TrapType', 'TreeSpecies')]))
plantspCount <- plantspCount[plantspCount$Freq > 0,]
plantspCount <- plantspCount[order(plantspCount$StateRouteStop, plantspCount$Station, plantspCount$TrapType),]
##Identify Combinations with Freq=2 (tree species for a treatment within a station is the same for both visits) 
plantspCount1 <- dplyr::filter(plantspCount, Freq==2) 
plantspCount2 <- spread(plantspCount1, key=TrapType, value=TreeSpecies)
##Identify Combinations with the Same Tree Species for VFX and VF
plantspCount3 <- dplyr::filter(plantspCount2, VF==VFX)
plantspCount3$ID <- paste(plantspCount3$StateRouteStop, plantspCount3$Station)
##Subset Existing Food Type Datasets to Create 3 New Food Type Datasets with Only Observations from Stations 
##Where VFX and VF are the Same ("Paired Stations")
paired_all <- all_time_12
paired_all$ID <- paste(paired_all$StateRouteStop, paired_all$Station)
paired_all1 <- dplyr::filter(paired_all, paired_all$ID %in% plantspCount3$ID)

paired_food <- food_time_12
paired_food$ID <- paste(paired_food$StateRouteStop, paired_food$Station)
paired_food1 <- dplyr::filter(paired_food, paired_food$ID %in% plantspCount3$ID)

paired_caterpillar <- caterpillar_time_12
paired_caterpillar$ID <- paste(paired_caterpillar$StateRouteStop, paired_caterpillar$Station)
paired_caterpillar1 <- dplyr::filter(paired_caterpillar, paired_caterpillar$ID %in% plantspCount3$ID)

#Run Wilcox Test on Paired Stations Data for 3 Food Types
wilcox_test(visit_dif ~ TrapType, data=paired_all1)
wilcox_test(visit_dif ~ TrapType, data=paired_food1)
wilcox_test(visit_dif ~ TrapType, data=paired_caterpillar1)

#Calculate Average Difference Between Change in VFX and Change in VF for Paired Stations
##All Food Group Data Reshaping
paired_all2 <- select(paired_all1, -Visit3, -visit_dif, -ID)
paired_all_Visit1<- spread(paired_all2, TrapType, Visit1)
names(paired_all_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
paired_all_Visit1$ID <- paste(paired_all_Visit1$StateRouteStop, paired_all_Visit1$Station)

paired_all3 <- select(paired_all1, -Visit1, -visit_dif, -ID)
paired_all_Visit3<- spread(paired_all3, TrapType, Visit3)
names(paired_all_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
paired_all_Visit3$ID <- paste(paired_all_Visit3$StateRouteStop, paired_all_Visit3$Station)

paired_all4 <- merge(paired_all_Visit1, paired_all_Visit3, by.x="ID", by.y="ID")
paired_all5 <- select(paired_all4, -StateRouteStop.y, -Station.y, -ID)
names(paired_all5) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

paired_all5$VF_dif <- paired_all5$Visit3VF-paired_all5$Visit1VF
paired_all5$VFX_dif <- paired_all5$Visit3VFX-paired_all5$Visit1VFX
paired_all5$VFX_VF_dif <- paired_all5$VFX_dif-paired_all5$VF_dif

##Bird Food Group Data Reshaping
paired_food2 <- select(paired_food1, -Visit3, -visit_dif, -ID)
paired_food_Visit1<- spread(paired_food2, TrapType, Visit1)
names(paired_food_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
paired_food_Visit1$ID <- paste(paired_food_Visit1$StateRouteStop, paired_food_Visit1$Station)

paired_food3 <- select(paired_food1, -Visit1, -visit_dif, -ID)
paired_food_Visit3<- spread(paired_food3, TrapType, Visit3)
names(paired_food_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
paired_food_Visit3$ID <- paste(paired_food_Visit3$StateRouteStop, paired_food_Visit3$Station)

paired_food4 <- merge(paired_food_Visit1, paired_food_Visit3, by.x="ID", by.y="ID")
paired_food5 <- select(paired_food4, -StateRouteStop.y, -Station.y, -ID)
names(paired_food5) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

paired_food5$VF_dif <- paired_food5$Visit3VF-paired_food5$Visit1VF
paired_food5$VFX_dif <- paired_food5$Visit3VFX-paired_food5$Visit1VFX
paired_food5$VFX_VF_dif <- paired_food5$VFX_dif-paired_food5$VF_dif

##Caterpillar Food Group Data Reshaping
paired_caterpillar2 <- select(paired_caterpillar1, -Visit3, -visit_dif, -ID)
paired_caterpillar_Visit1<- spread(paired_caterpillar2, TrapType, Visit1)
names(paired_caterpillar_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
paired_caterpillar_Visit1$ID <- paste(paired_caterpillar_Visit1$StateRouteStop, paired_caterpillar_Visit1$Station)

paired_caterpillar3 <- select(paired_caterpillar1, -Visit1, -visit_dif, -ID)
paired_caterpillar_Visit3<- spread(paired_caterpillar3, TrapType, Visit3)
names(paired_caterpillar_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
paired_caterpillar_Visit3$ID <- paste(paired_caterpillar_Visit3$StateRouteStop, paired_caterpillar_Visit3$Station)

paired_caterpillar4 <- merge(paired_caterpillar_Visit1, paired_caterpillar_Visit3, by.x="ID", by.y="ID")
paired_caterpillar5 <- select(paired_caterpillar4, -StateRouteStop.y, -Station.y, -ID)
names(paired_caterpillar5) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

paired_caterpillar5$VF_dif <- paired_caterpillar5$Visit3VF-paired_caterpillar5$Visit1VF
paired_caterpillar5$VFX_dif <- paired_caterpillar5$Visit3VFX-paired_caterpillar5$Visit1VFX
paired_caterpillar5$VFX_VF_dif <- paired_caterpillar5$VFX_dif-paired_caterpillar5$VF_dif

#Averages of Difference in Change in VFX over time and VF over time for 3 Food Types for Paired Stations
paired_all_avg <- summarize(paired_all5, mean(VFX_VF_dif))
paired_food_avg <- summarize(paired_food5, mean(VFX_VF_dif))
paired_caterpillar_avg <- summarize(paired_caterpillar5, mean(VFX_VF_dif))

#Calculate Average Difference Between Change in VFX and Change in VF for All Complete Stations
##All Food Group Reshaping
all_time1_12 <- select(all_time_12, -Visit3, -visit_dif)
all_Visit1<- spread(all_time1_12, TrapType, Visit1)
names(all_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
all_Visit1$ID <- paste(all_Visit1$StateRouteStop, all_Visit1$Station)

all_time2_12 <- select(all_time_12, -Visit1, -visit_dif)
all_Visit3<- spread(all_time2_12, TrapType, Visit3)
names(all_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
all_Visit3$ID <- paste(all_Visit3$StateRouteStop, all_Visit3$Station)

all_time3_12 <- merge(all_Visit1, all_Visit3, by.x="ID", by.y="ID")
all_time4_12 <- select(all_time3_12, -StateRouteStop.y, -Station.y, -ID)
names(all_time4_12) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

all_time4_12$VF_dif<- all_time4_12$Visit3VF-all_time4_12$Visit1VF
all_time4_12$VFX_dif<- all_time4_12$Visit3VFX-all_time4_12$Visit1VFX
all_time4_12$VFX_VF_dif<-all_time4_12$VFX_dif-all_time4_12$VF_dif

##Bird Food Group Reshaping
food_time1_12 <- select(food_time_12, -Visit3, -visit_dif)
food_Visit1<- spread(food_time1_12, TrapType, Visit1)
names(food_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
food_Visit1$ID <- paste(food_Visit1$StateRouteStop, food_Visit1$Station)

food_time2_12 <- select(food_time_12, -Visit1, -visit_dif)
food_Visit3<- spread(food_time2_12, TrapType, Visit3)
names(food_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
food_Visit3$ID <- paste(food_Visit3$StateRouteStop, food_Visit3$Station)

food_time3_12 <- merge(food_Visit1, food_Visit3, by.x="ID", by.y="ID")
food_time4_12 <- select(food_time3_12, -StateRouteStop.y, -Station.y, -ID)
names(food_time4_12) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

food_time4_12$VF_dif<- food_time4_12$Visit3VF-food_time4_12$Visit1VF
food_time4_12$VFX_dif<- food_time4_12$Visit3VFX-food_time4_12$Visit1VFX
food_time4_12$VFX_VF_dif<-food_time4_12$VFX_dif-food_time4_12$VF_dif

##Caterpillar Food Group Reshaping
caterpillar_time1_12 <- select(caterpillar_time_12, -Visit3, -visit_dif)
caterpillar_Visit1<- spread(caterpillar_time1_12, TrapType, Visit1)
names(caterpillar_Visit1) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX")
caterpillar_Visit1$ID <- paste(caterpillar_Visit1$StateRouteStop, caterpillar_Visit1$Station)

caterpillar_time2_12 <- select(caterpillar_time_12, -Visit1, -visit_dif)
caterpillar_Visit3<- spread(caterpillar_time2_12, TrapType, Visit3)
names(caterpillar_Visit3) <- c("StateRouteStop", "Station", "Visit3VF", "Visit3VFX")
caterpillar_Visit3$ID <- paste(caterpillar_Visit3$StateRouteStop, caterpillar_Visit3$Station)

caterpillar_time3_12 <- merge(caterpillar_Visit1, caterpillar_Visit3, by.x="ID", by.y="ID")
caterpillar_time4_12 <- select(caterpillar_time3_12, -StateRouteStop.y, -Station.y, -ID)
names(caterpillar_time4_12) <- c("StateRouteStop", "Station", "Visit1VF", "Visit1VFX", "Visit3VF", "Visit3VFX")

caterpillar_time4_12$VF_dif<- caterpillar_time4_12$Visit3VF-caterpillar_time4_12$Visit1VF
caterpillar_time4_12$VFX_dif<- caterpillar_time4_12$Visit3VFX-caterpillar_time4_12$Visit1VFX
caterpillar_time4_12$VFX_VF_dif<-caterpillar_time4_12$VFX_dif-caterpillar_time4_12$VF_dif

##Average difference in Change in VFX andchange in VF for All Complete Stations
all_avg <- summarize(all_time4_12, mean(VFX_VF_dif))
food_avg <- summarize(food_time4_12, mean(VFX_VF_dif))
caterpillar_avg <- summarize(caterpillar_time4_12, mean(VFX_VF_dif))

###Graphing
#Example Change in Abundance Over Time Graph (for one StateRouteStop-Station)
ex_graph_data <-dplyr::filter(all_abundance2_12, StateRouteStop == "8890236", Station== "3B")
ex_graph_data$VisitNumber <- as.numeric(as.character(ex_graph_data$VisitNumber))
plot(x=ex_graph_data$VisitNumber, y=ex_graph_data$VFX, xlim=range(c(0,4)), ylim=range(c(1,5)), 
            xlab="Visit Number", ylab="Arthropod Abundance", type="b", col="red", main="Sample Change In Arthropod Abundance Over Time")
points(x=ex_graph_data$VisitNumber, y=ex_graph_data$VF, type="b", col="blue")


#Histogram for All 3 Food Types 
#Showing the difference between change in VFX over time and change in VF over time
histogram(all_time4_12$VFX_VF_dif, breaks=30, xlim=c(-20,15), 
          xlab="Difference in Change in Arth Density between VFX and VF",
          main="All Arthropods >2.0 mm")

histogram(food_time4_12$VFX_VF_dif, breaks=30, 
          xlab="Difference in Change in Arth Density between VFX and VF", 
          main="Bird Food Arthropods")

histogram(caterpillar_time4_12$VFX_VF_dif, breaks=10, ylab="Percent of Total",
          xlab="Difference in Change in Arth Density between VFX and VF",
          main="Caterpillars")

#Make Map of StateRouteStop locations
##Read in BBS Lat-Long Data
lat_longs <- read.table("BBS_stop_latlongs.txt", sep= '\t', quote="\"", header=TRUE)
##Filter data to only include StateRouteStop locations where the exclosure experiment occurred
exclosure_lat_longs<- dplyr::filter(lat_longs, Stateroutestop %in% c("2704132", "6302205", "6303117", 
                                                              "6390627", "6390644", "6390909", 
                                                              "6390944", "6391108", "8204219", 
                                                              "8290243", "8290339", "8290344", 
                                                              "8890009", "8890029", "8890236"))
##Map lat-long data
map('state', xlim = c(-85, -75), ylim = c(33, 40))
points(exclosure_lat_longs$Longitude,exclosure_lat_longs$Latitude, pch = 4, col ='green', 
       cex = 1, bg="black", main="Survey Site Locations")

##Trying out analyzing by tree species
#uniqTree <- unique(topdown9$TreeSpecies)
#uniqTrees <-as.character(uniqTree)
#topdown9$TreeSpecies <- as.character(topdown9$TreeSpecies)
#trees= c()
#for (tree in uniqTrees) {
#  print(dplyr::filter(topdown9, TreeSpecies == tree))}
#treeSp= data.frame(trees)



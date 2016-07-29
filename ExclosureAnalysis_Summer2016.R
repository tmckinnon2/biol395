#Analysis of Exclosure Data Summer 2016
#Open necessary packages
library(dplyr)
library(tidyr)
# Read in data
setwd("~/Desktop/insect-exclosure")
all_surveys <- read.csv('tbl_surveys.csv', header=F)
all_orders <- read.csv('tbl_orders.csv', header=F)
all_surveyTrees <- read.csv("tbl_surveyTrees.csv", header=T)

# Rename columns and remove unecessary ones
names(all_surveys) = c("surveyID", "siteID", "userID", 
                       "circle", "survey", "timeStart", 
                       "timeSubmit", "temperatureMin", "temperatureMax",
                       "siteNotes", "plantSpecies", "herbivory", 
                       "leavePhoto", "isValid", "Status",
                       "surveyType", "leafCount", "source")
names(all_orders) = c("orderID", "surveyID", "orderArthropod", 
                      "orderLength", "orderNotes", "orderCount",
                      "insectPhoto", "timeStamp", "isValid")
names(all_surveyTrees) = c("siteID", "circle", "survey", "surveyTrees")

all_surveys1= select(all_surveys, -timeSubmit, -Status, -leavePhoto, -source)
all_orders1 = select(all_orders, -insectPhoto, -timeStamp, -isValid)


#Add columns with arth order count and official plant species
all_data <- merge (all_surveys1, all_orders1, 
                   by.x = "surveyID", 
                   by.y = "surveyID", all.x= TRUE)
###Create Identifier Column for Specific Survey locations
all_data$identifier <- paste0(all_data$siteID, 
                              all_data$circle, 
                              all_data$survey)
all_surveyTrees$identifier <- paste0(all_surveyTrees$siteID, 
                          all_surveyTrees$circle, 
                          all_surveyTrees$survey)
all_data1 <- merge(all_data, all_surveyTrees, 
                   by.x = "identifier", 
                   by.y= "identifier", all.x= TRUE)
all_data2 <- select(all_data1, -siteID.y, -circle.y, -survey.y, -plantSpecies) 
names(all_data2) <- c("identifier", "surveyID", "siteID", "userID", 
                      "circle", "survey","timeStart","temperatureMin", 
                      "temperatureMax","siteNotes","herbivory","isValid",
                      "surveyType","leafCount","orderID","orderArthropod",
                      "orderLength", "orderNotes",
                      "orderCount", "surveyTrees")

#Identify Exclosure Surveys 
exclosures <-filter(all_data2, grepl("EXCLOSURE", siteNotes))
exclosures$TrapType <- "VFX"
exclosures$identifier <- paste0(exclosures$siteID, exclosures$circle, exclosures$survey)

#Identify visual control surveys (identify paired surveys and remove 
#beat sheet surveys)
visual_surveys <- filter(all_data2, leafCount == "50")
ex_pairs_allvisuals <- filter(visual_surveys, identifier 
                            %in% exclosures$identifier)


#Create dataframe with surveys from paired controls and exclosures on dates exclosures 
#were surveyed
ex_pairs <-filter(ex_pairs_allvisuals, grepl("2016-05-11", timeStart) |
                                       grepl("2016-05-12", timeStart) | 
                                       grepl("2016-05-16", timeStart) | 
                                       grepl("2016-05-18", timeStart) |
                                       grepl("2016-06-23", timeStart) | 
                                       grepl("2016-06-24", timeStart))
#Add column marking exclosures
ex_pairs1 <- merge(ex_pairs, exclosures, by.x = "orderID", 
                                         by.y= "orderID",
                                         all.x = TRUE)
ex_pairs2 <- select(ex_pairs1, -identifier.y, -surveyID.y, -siteID.y, -userID.y, 
                    -circle.y, -survey.y, -timeStart.y, -temperatureMin.y,
                    -temperatureMax.y, -siteNotes.y, -herbivory.y, -isValid.y, 
                    -surveyType.y, -leafCount.y, -orderArthropod.y, -orderLength.y, 
                    -orderNotes.y, -orderCount.y, -surveyTrees.y)
names(ex_pairs2) <- c("OrderID", "identifier", "surveyID", "siteID", "userID", "circle",
                      "survey", "timeStart", "temperatureMin", "temperatureMax",
                      "siteNotes", "herbivory", "isValid",
                      "surveyType", "leafCount", "orderArthropod", "orderLength",
                      "orderNotes", "orderCount", "surveyTrees", "TrapType")

ex_pairs2["TrapType"][is.na(ex_pairs2["TrapType"])] <- "VF"
ex_pairs2["orderCount"][is.na(ex_pairs2["orderCount"])] <- 0


#Add dates column ***not a neat method, need to get something else to work***
May11 <- filter(ex_pairs2, grepl("2016-05-11", timeStart))
May11$date <- "2016-05-11"
May11$VisitNumber <- "1"

May12 <- filter(ex_pairs2, grepl("2016-05-12", timeStart))
May12$date <- "2016-05-12"
May12$VisitNumber <- "1"

May16 <- filter(ex_pairs2, grepl("2016-05-16", timeStart))
May16$date <- "2016-05-16"
May16$VisitNumber <- "2"

May18 <- filter(ex_pairs2, grepl("2016-05-18", timeStart))
May18$date <- "2016-05-18"
May18$VisitNumber <- "2"

June23 <- filter(ex_pairs2, grepl("2016-06-23", timeStart))
June23$date <- "2016-06-23"
June23$VisitNumber <- "3"

June24 <- filter(ex_pairs2, grepl("2016-06-24", timeStart))
June24$date <- "2016-06-24"
June24$VisitNumber <- "3"

ex_pairs3 <- bind_rows(May11, May12, May16, 
                       May18, June23, June24)


#Summarise observations for 3 food types (start 2012 reshaping analysis code)
#Summarise Observations for All Arthropods (Arthropods greater than 2 mm)
grouped_all <- ex_pairs3 %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_all <- (summarise(grouped_all, sum(orderCount)))
total_all$surveyID<- paste0(total_all$siteID, total_all$VisitNumber,
                            total_all$circle, total_all$survey, total_all$TrapType)


#Summarise Observations for Relevant Orders great than 5 mm ("Bird food Arthropods") 
food_arthropods <- filter(ex_pairs3, orderLength > "4", orderArthropod %in% c("Caterpillars (Lepidoptera larvae)", 
                                                       "Beetles (Coleoptera)", 
                                                       "Spiders (Araneae; NOT daddy longlegs!)", 
                                                       "True Bugs (Heteroptera)", 
                                                       "Grasshoppers, Crickets (Orthoptera)", 
                                                       "Leaf hoppers and Cicadas (Auchenorrhyncha)"))
grouped_food <- food_arthropods %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_food <- (summarise(grouped_food, sum(orderCount)))
total_food$surveyID<- paste0(total_food$siteID, total_food$VisitNumber,
                                total_food$circle, total_food$survey,total_food$TrapType)

#Summarise Observations for Caterpillars
caterpillar <-filter(ex_pairs3, orderArthropod == "Caterpillars (Lepidoptera larvae)")
grouped_caterpillar <- caterpillar %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_caterpillar <- (summarise(grouped_caterpillar, sum(orderCount)))
total_caterpillar$surveyID<- paste0(total_caterpillar$siteID, total_caterpillar$VisitNumber,
                                total_caterpillar$circle, total_caterpillar$survey,total_caterpillar$TrapType)

#Create Unique Surveys Dataframe
unique_surveys<-unique(ex_pairs3[, c("TrapType", "siteID", "circle", "survey", "VisitNumber")])
unique_surveys$surveyID<- paste0(unique_surveys$siteID, unique_surveys$VisitNumber, 
                                    unique_surveys$circle, unique_surveys$survey,unique_surveys$TrapType)
unique_surveys_count <- data.frame(table(unique_surveys[, c("TrapType", "siteID", 
                                                               "circle", "survey", 
                                                               "VisitNumber", "surveyID")]))
unique_surveys_count = unique_surveys_count[unique_surveys_count$Freq> 0,]

#Create 3 New Dataframes Merging List of Unique Surveys with Summary Observations 
#for Each Unique Survey for Each of the 3 Food Types
all_abundance <- merge(unique_surveys_count, total_all,
                       by.x="surveyID",
                       by.y = "surveyID", 
                       all.x = TRUE)
all_abundance1<- select(all_abundance, -Freq, -TrapType.y, -siteID.y, -circle.y,
                        -survey.y, -VisitNumber.y, -surveyID)
names(all_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_all")
all_abundance1["total_all"][is.na(all_abundance1["total_all"])] <- 0


food_abundance <- merge(unique_surveys_count, total_food,
                       by.x="surveyID",
                       by.y = "surveyID", 
                       all.x = TRUE)
food_abundance1<- select(food_abundance, -Freq, -TrapType.y, -siteID.y, -circle.y,
                         -survey.y, -VisitNumber.y, -surveyID)
names(food_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_food")
food_abundance1["total_food"][is.na(food_abundance1["total_food"])] <- 0

#Remove outliers
food_time <-filter(caterpillar_abundance1, total_caterpillar > 6)
caterpilar_time <- filter(food_abundance1, total_food > 6)

caterpillar_abundance <- merge(unique_surveys_count, total_caterpillar,
                        by.x="surveyID",
                        by.y = "surveyID", 
                        all.x = TRUE)
caterpillar_abundance1<- select(caterpillar_abundance, -Freq, -TrapType.y, -siteID.y, 
                         -circle.y,  -survey.y, -VisitNumber.y, -surveyID)
names(caterpillar_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_caterpillar")
caterpillar_abundance1["total_caterpillar"][is.na(caterpillar_abundance1["total_caterpillar"])] <- 0

#Remove outliers
caterpillar_time <-filter(caterpillar_abundance1, total_caterpillar < 6)


#Spread Visit 1 and Visit 3 for 3 Food Type Datasets and Create Difference Column to Format for Wilcox Test
all_time <- spread(all_abundance1, VisitNumber, total_all)
names(all_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
all_time$visit_dif<-all_time$Visit3-all_time$Visit2

food_time <- spread(food_abundance1, VisitNumber, total_food)
names(food_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
food_time$visit_dif<-food_time$Visit3-food_time$Visit2

caterpillar_time <- spread(caterpillar_time, VisitNumber, total_caterpillar)
names(caterpillar_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
caterpillar_time$visit_dif<-caterpillar_time$Visit3-caterpillar_time$Visit2


#Run wilcox_test
library("coin")
wilcox_test(visit_dif ~ TrapType, data=all_time)
wilcox_test(visit_dif ~ TrapType, data=food_time)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time) 

#Run analyses on dif between end arth density b/w treatment and control (independent of 2012 comparison)
wilcox_test(Visit3~ TrapType, data=food_time)
wilcox_test(Visit3~ TrapType, data=caterpillar_time)

##Reshape dataframe for visualization
food_time1 <- select(food_time, -Visit1, -Visit2, -visit_dif)
food_Visit3<- spread(food_time1, TrapType, Visit3)
names(food_Visit3) <- c("siteID", "circle", "survey", "Visit3VF", "Visit3VFX")
food_Visit3$Visit3Dif <- (food_Visit3$Visit3VFX - food_Visit3$Visit3VF)
food_Visit3$identifier <- paste0(food_Visit3$siteID, food_Visit3$circle, food_Visit3$survey)
food_final<-merge(food_Visit3, all_surveyTrees, by.x = "identifier", by.y="identifier")
food_final <- select(food_final, -siteID.y, -circle.y, -survey.y)
names(food_final)<- c("identifier", "siteID", "circle", "survey", "Visit3VF", "Visit3VFX", "Visit3Dif", "treeSp")

caterpillar_time1 <- select(caterpillar_time, -Visit1, -Visit2, -visit_dif)
caterpillar_Visit3<- spread(caterpillar_time1, TrapType, Visit3)
names(caterpillar_Visit3) <- c("siteID", "circle", "survey", "Visit3VF", "Visit3VFX")
caterpillar_Visit3$Visit3Dif <- caterpillar_Visit3$Visit3VFX - caterpillar_Visit3$Visit3VF
caterpillar_Visit3$identifier <- paste0(caterpillar_Visit3$siteID, caterpillar_Visit3$circle, caterpillar_Visit3$survey)
caterpillar_final<-merge(caterpillar_Visit3, all_surveyTrees, by.x = "identifier", by.y="identifier")
caterpillar_final <- select(caterpillar_final, -siteID.y, -circle.y, -survey.y)
names(caterpillar_final)<- c("identifier", "siteID", "circle", "survey", "Visit3VF", "Visit3VFX", "Visit3Dif", "treeSp")

##Visualize exclosure data vs. control data
#Boxplot of difference in final arth densities by site
boxplot(food_final$Visit3Dif ~food_final$siteID, main="Arth Density for Relevant Orders by Site", 
        ylab="FinalVisit Difference in Arth Density (treatment-control)", new=T)
boxplot(caterpillar_final$Visit3Dif ~caterpillar_final$siteID, main="Caterpillar Density by Site", ylab="FinalVisit Difference in Arth Density (treatment-control)", new=T)

#Boxplot of difference in final arth densities by plant species
boxplot(food_final$Visit3Dif ~food_final$treeSp, main="Arth Density for Relevant Orders by Tree Species", 
        ylab="FinalVisit Difference in Arth Density (treatment-control)", new=T)
boxplot(caterpillar_final$Visit3Dif ~caterpillar_final$treeSp, main="Caterpillar Density by Tree Species", 
        ylab="FinalVisit Difference in Caterpillar Density (treatment-control)", new=T)

boxplot(food_final$Visit3Dif,food_final$treeSp=="American beech", main="Arth Density for Relevant Orders by Tree Species", 
        ylab="FinalVisit Difference in Arth Density (treatment-control)")
boxplot(food_final$Visit3Dif,food_final$treeSp=="Box elder", main="Arth Density for Relevant Orders by Tree Species", 
        ylab="FinalVisit Difference in Arth Density (treatment-control)")

#*********************************************************************************************
##Group by plant species
##Add back plant species info
all_time$identifier <- paste0(all_time$siteID, 
                              all_time$circle, 
                              all_time$survey)
all_sp <-merge(all_time, all_surveyTrees, 
                    by.x= "identifier", 
                    by.y="identifier", all.x= TRUE)
all_sp1<- select(all_sp, -identifier, -siteID.y,-survey.y, -circle.y)
names(all_sp1) <- c("TrapType", "siteID", "circle", 
                   "survey", "Visit1", "Visit2", 
                   "Visit3", "visit_dif", "surveyTrees")

#subset data frame by plant species
all_BG_Spicebush <- filter(all_sp1, siteID == "8892356", surveyTrees=="Spicebush")
all_BG_Sugar <- filter(all_sp1, siteID == "8892356", surveyTrees=="Sugar maple")
all_BG_Beech <- filter(all_sp1, siteID == "8892356", surveyTrees=="American beech")
all_PR_Sweet <- filter(all_sp1, siteID == "117", surveyTrees=="Sweet gum")
all_PR_Box <- filter(all_sp1, siteID == "117", surveyTrees=="Box elder")
all_PR_Red <- filter(all_sp1, siteID == "117", surveyTrees=="Red maple")













#****************Analyze change in herbivory**********
unique_herbivory<-unique(ex_pairs3[, c("TrapType", "siteID", "circle", "survey", "VisitNumber", "herbivory")])

#Spread Visit 1 and Visit 3 for Herbivory and Create Difference Column to Format for Wilcox Test
herb <- spread(unique_herbivory, VisitNumber, herbivory)
names(herb) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
herb$visit_dif<-herb$Visit3-herb$Visit2
herb$TrapType <- as.factor(herb$TrapType)

#Run Wilcox test
wilcox_test(visit_dif ~ TrapType, data=herb)#class issue

#*****Attempts at "ifelse" code to revisit
if (grepl("5/11/16",ex_pairs2$timeStart)) {ex_pairs2$date = "5/11/16"}
else {if (grepl("5/12/16",ex_pairs2$timeStart)) {ex_pairs2$date<-"5/12/16"
} else {if (grepl("5/16/16",ex_pairs2$timeStart)) {ex_pairs2$date<-"5/16/16"
} else {if (grepl("5/18/16",ex_pairs2$timeStart)) {ex_pairs2$date<-"5/18/16"
} else {if (grepl("6/23/16",ex_pairs2$timeStart)) {ex_pairs2$date<-"6/23/16"
} else {if (grepl("6/24/16",ex_pairs2$timeStart)) {ex_pairs2$date<-"6/24/16"
} else ex_pairs2$date <- "NA"}}}}} 


ex_pairs2$date <- (ifelse((grepl("5/11/16",ex_pairs2$timeStart)), "5/11/16", "NA")
                   ifelse((grepl("5/12/16",ex_pairs2$timeStart)), "5/12/16", "NA")
                   ifelse((grepl("5/16/16",ex_pairs2$timeStart)), "5/16/16", "NA")
                   ifelse((grepl("5/18/16",ex_pairs2$timeStart)), "5/18/16", "NA")
                   ifelse((grepl("6/23/16",ex_pairs2$timeStart)), "6/23/16", "NA")
                   ifelse((grepl("6/24/16",ex_pairs2$timeStart)), "6/24/16", "NA"))
#***Use substring on earlier text**** substr()



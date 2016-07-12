#Open necessary packages
library(dplyr)
library(tidyr)
# Read in data
setwd("~/Desktop/insect-exclosure")
all_surveys <- read.table("tbl_surveys_2016.txt", 
                          sep= '\t', 
                          quote="\"", 
                          header=TRUE)
all_orders <- read.table("tbl_orders_2016.txt", 
                         sep= '\t', 
                         quote="\"",
                         header=TRUE)


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
all_surveys1= select(all_surveys, -timeSubmit, -Status, -leavePhoto, -source)
all_orders1 = select(all_orders, -insectPhoto, -timeStamp, -isValid)

#Merge Survey and Arth Data
all_data <- merge (all_surveys1, all_orders1, 
                   by.x = "surveyID", 
                   by.y = "surveyID", all.x= TRUE)

#Identify Exclosure Surveys 
exclosures <-filter(all_data, grepl("EXCLOSURE", siteNotes))
exclosures$TrapType <- "VFX"
exclosures$identifier <- paste0(exclosures$siteID, 
                                exclosures$circle,
                                exclosures$survey)

#Identify visual control surveys (identify paired surveys and remove 
#beat sheet surveys)

all_data$identifier <- paste0(all_data$siteID, 
                              all_data$circle, 
                              all_data$survey)
visual_surveys <- filter(all_data, leafCount == "50")
ex_pairs_allvisuals <- filter(visual_surveys, identifier 
                            %in% exclosures$identifier)


#Create dataframe with surveys from paired controls and exclosures on dates exclosures 
#were surveyed
ex_pairs <-filter(ex_pairs_allvisuals, grepl("5/11/16", timeStart) |
                                       grepl("5/12/16", timeStart) | 
                                       grepl("5/16/16", timeStart) | 
                                       grepl("5/18/16", timeStart) |
                                       grepl("6/23/16", timeStart) | 
                                       grepl("6/24/16", timeStart))
#Add column marking exclosures
ex_pairs1 <- merge(ex_pairs, exclosures, by.x = "orderID", 
                                         by.y= "orderID",
                                         all.x = TRUE)
ex_pairs2 <- select(ex_pairs1, -c(21:38), -identifier.y)
names(ex_pairs2) <- c("OrderID", "surveyID", "siteID", "userID", "circle",
                      "survey", "timeStart", "temperatureMin", "temperatureMax",
                      "siteNotes", "plantSpecies", "herbivory", "isValid",
                      "surveyType", "leafCount", "orderArthropod", "orderLength",
                      "orderNotes", "orderCount", "identifier", "TrapType")

ex_pairs2["TrapType"][is.na(ex_pairs2["TrapType"])] <- "VF"
ex_pairs2["orderCount"][is.na(ex_pairs2["orderCount"])] <- 0


#Add dates column ***not a neat method, need to get something else to work***
May11 <- filter(ex_pairs2, grepl("5/11/16", timeStart))
May11$date <- "5/11/16"
May11$VisitNumber <- "1"

May12 <- filter(ex_pairs2, grepl("5/12/16", timeStart))
May12$date <- "5/12/16"
May12$VisitNumber <- "1"

May16 <- filter(ex_pairs2, grepl("5/16/16", timeStart))
May16$date <- "5/16/16"
May16$VisitNumber <- "2"

May18 <- filter(ex_pairs2, grepl("5/18/16", timeStart))
May18$date <- "5/18/16"
May18$VisitNumber <- "2"

June23 <- filter(ex_pairs2, grepl("6/23/16", timeStart))
June23$date <- "6/23/16"
June23$VisitNumber <- "3"

June24 <- filter(ex_pairs2, grepl("6/24/16", timeStart))
June24$date <- "6/24/16"
June24$VisitNumber <- "3"

ex_pairs3 <- bind_rows(May11, May12, May16, 
                       May18, June23, June24)


#Summarise observations for 3 food types (start 2012 reshaping analysis code)
#Summarise Observations for All Arthropods (Arthropods greater than 2 mm)
grouped_all <- ex_pairs3 %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_all <- (summarise(grouped_all, sum(orderCount)))
total_all$surveyID<- paste0(total_all$siteID, total_all$VisitNumber,
                            total_all$circle, total_all$survey,
                            total_all$TrapType)


#Summarise Observations for Relevant Orders ("Bird food Arthropods") 
food_arthropods <- filter(ex_pairs3, orderArthropod %in% c("Caterpillars (Lepidoptera larvae)", 
                                                       "Beetles (Coleoptera)", 
                                                       "Spiders (Araneae; NOT daddy longlegs!)", 
                                                       "True Bugs (Heteroptera)", 
                                                       "Grasshoppers, Crickets (Orthoptera)", 
                                                       "Leaf hoppers and Cicadas (Auchenorrhyncha)"))
grouped_food <- food_arthropods %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_food <- (summarise(grouped_food, sum(orderCount)))
total_food$surveyID<- paste0(total_food$siteID, total_food$VisitNumber,
                                total_food$circle, total_food$survey,
                                total_food$TrapType)

#Summarise Observations for Caterpillars
caterpillar <-filter(ex_pairs3, orderArthropod == "Caterpillars (Lepidoptera larvae)")
grouped_caterpillar <- caterpillar %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_caterpillar <- (summarise(grouped_caterpillar, sum(orderCount)))
total_caterpillar$surveyID<- paste0(total_caterpillar$siteID, total_caterpillar$VisitNumber,
                                total_caterpillar$circle, total_caterpillar$survey,
                                total_caterpillar$TrapType)

#Create Unique Surveys Dataframe
unique_surveys<-unique(ex_pairs3[, c("TrapType", "siteID", "circle", "survey", "VisitNumber")])
unique_surveys$surveyID<- paste0(unique_surveys$siteID, unique_surveys$VisitNumber, 
                                    unique_surveys$circle, unique_surveys$survey,
                                    unique_surveys$TrapType)
unique_surveys_count <- data.frame(table(unique_surveys[, c("TrapType", 
                                                               "siteID", 
                                                               "circle", 
                                                               "survey", 
                                                               "VisitNumber", "surveyID")]))
unique_surveys_count = unique_surveys_count[unique_surveys_count$Freq> 0,]

#Create 3 New Dataframes Merging List of Unique Surveys with Summary Observations 
#for Each Unique Survey for Each of the 3 Food Types
all_abundance <- merge(unique_surveys_count, total_all,
                       by.x="surveyID",
                       by.y = "surveyID", 
                       all.x = TRUE)
all_abundance1<- select(all_abundance, -Freq,
                          -TrapType.y, 
                          -siteID.y, 
                          -circle.y, 
                          -survey.y, 
                          -VisitNumber.y, 
                          -surveyID)
names(all_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_all")



food_abundance <- merge(unique_surveys_count, total_food,
                       by.x="surveyID",
                       by.y = "surveyID", 
                       all.x = TRUE)
food_abundance1<- select(food_abundance, -Freq,
                                         -TrapType.y, 
                                         -siteID.y, 
                                         -circle.y, 
                                         -survey.y, 
                                         -VisitNumber.y, 
                                         -surveyID)
names(food_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_food")

caterpillar_abundance <- merge(unique_surveys_count, total_caterpillar,
                        by.x="surveyID",
                        by.y = "surveyID", 
                        all.x = TRUE)
caterpillar_abundance1<- select(caterpillar_abundance, -Freq,
                         -TrapType.y, 
                         -siteID.y, 
                         -circle.y, 
                         -survey.y, 
                         -VisitNumber.y, 
                         -surveyID)
names(caterpillar_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_caterpillar")


#Spread TrapType column in 3 Food Type Datasets
all_abundance2 <- spread(all_abundance1, TrapType, total_all)
food_abundance2 <- spread(food_abundance1, TrapType, total_food)
caterpillar_abundance2 <-spread(caterpillar_abundance1, TrapType, total_caterpillar)

#Spread Visit 1 and Visit 3 for 3 Food Type Datasets and Create Difference Column to Format for Wilcox Test
all_time <- spread(all_abundance1, VisitNumber, total_all)
names(all_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
all_time$visit_dif<-all_time$Visit3-all_time$Visit2

food_time <- spread(food_abundance1, VisitNumber, total_food)
names(food_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
food_time$visit_dif<-food_time$Visit3-food_time$Visit2

caterpillar_time <- spread(caterpillar_abundance1, VisitNumber, total_caterpillar)
names(caterpillar_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
caterpillar_time$visit_dif<-caterpillar_time$Visit3-caterpillar_time$Visit2

#Run wilcox_test
library("coin")
wilcox_test(visit_dif ~ TrapType, data=all_time)
wilcox_test(visit_dif ~ TrapType, data=food_time)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time) #how do I fix this error

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




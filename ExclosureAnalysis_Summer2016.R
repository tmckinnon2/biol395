##Need to incorporate something into script to remove beat sheet surveys
#Open necessary packages
library(dplyr)
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
                   by.y = "surveyID")

#Identify Exclosure Surveys 
exclosures <-filter(all_data, grepl("EXCLOSURE", siteNotes))
exclosures$exclosure <- "yes"
exclosures$identifier <- paste0(exclosures$siteID, 
                                exclosures$circle,
                                exclosures$survey)

#Identify control surveys
all_data$identifier <- paste0(all_data$siteID, 
                              all_data$circle, 
                              all_data$survey)
ex_pairs_alldates <- filter(all_data, identifier 
                            %in% exclosures$identifier)

#Create dataframe with surveys from paired controls and exclosures on dates exclosures were surveyed
ex_pairs <-filter(ex_pairs_alldates, grepl("5/11/16", timeStart) |
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
ex_pairs2[is.na(ex_pairs2)] <- "no"
names(ex_pairs2) <- c("OrderID", "surveyID", "siteID", "userID", "circle",
                      "survey", "timeStart", "temperatureMin", "temperatureMax",
                      "siteNotes", "plantSpecies", "herbivory", "isValid",
                      "surveyType", "leafCount", "orderArthropod", "orderLength",
                      "orderNotes", "orderCount", "identifier", "exclosure")

#Add dates column ***not a neat method, need to get "ifelse" to work***
May11 <- filter(ex_pairs2, grepl("5/11/16", timeStart))
May11$date <- "5/11/16"

May12 <- filter(ex_pairs2, grepl("5/12/16", timeStart))
May12$date <- "5/12/16"

May16 <- filter(ex_pairs2, grepl("5/16/16", timeStart))
May16$date <- "5/16/16"

May18 <- filter(ex_pairs2, grepl("5/18/16", timeStart))
May18$date <- "5/18/16"

June23 <- filter(ex_pairs2, grepl("6/23/16", timeStart))
June23$date <- "6/23/16"

June24 <- filter(ex_pairs2, grepl("6/24/16", timeStart))
June24$date <- "6/24/16"

ex_pairs3 <- bind_rows(May11, May12, May16, 
                       May18, June23, June24)

#Subset into NCBG and PR DataFrames
PR<- filter(ex_pairs3, siteID == "117")
NCBG <- filter(ex_pairs3, siteID == "8892356")


#Summarise observations for 3 food types (start 2012 reshaping analysis code)
#PR
#Summarise Observations for All Arthropods (Arthropods greater than 2 mm)
grouped_all_PR <- PR %>% group_by(exclosure, siteID, circle, survey, date)
total_all_PR <- (summarise(grouped_all_PR, sum(orderCount)))
total_all_PR$surveyID<- paste0(total_all_PR$siteID, total_all_PR$date,
                            total_all_PR$circle, total_all_PR$survey,
                            total_all_PR$exclosure)


#Summarise Observations for Relevant Orders ("Bird food Arthropods") 
food_arthropods_PR <- filter(PR, orderArthropod %in% c("Caterpillars (Lepidoptera larvae)", 
                                                       "Beetles (Coleoptera)", 
                                                       "Spiders (Araneae; NOT daddy longlegs!)", 
                                                       "True Bugs (Heteroptera)", 
                                                       "Grasshoppers, Crickets (Orthoptera)", 
                                                       "Leaf hoppers and Cicadas (Auchenorrhyncha)"))
grouped_food_PR <- food_arthropods_PR %>% group_by(exclosure, siteID, circle, survey, date)
total_food_PR <- (summarise(grouped_food_PR, sum(orderCount)))
total_food_PR$surveyID<- paste0(total_food_PR$siteID, total_food_PR$date,
                                total_food_PR$circle, total_food_PR$survey,
                                total_food_PR$exclosure)

#Summarise Observations for Caterpillars
caterpillars_PR <-filter(PR, orderArthropod == "Caterpillars (Lepidoptera larvae)")
grouped_caterpillars_PR <- caterpillars_PR %>% group_by(exclosure, siteID, circle, survey, date)
total_caterpillars_PR <- (summarise(grouped_caterpillars_PR, sum(orderCount)))
total_caterpillars_PR$surveyID<- paste0(total_caterpillars_PR$siteID, total_caterpillars_PR$date,
                                total_caterpillars_PR$circle, total_caterpillars_PR$survey,
                                total_caterpillars_PR$exclosure)

#Create Unique Surveys Dataframe
unique_surveys_PR<-unique(PR[, c("exclosure", "siteID", "circle", "survey", "date")])
unique_surveys_PR$surveyID<- paste0(unique_surveys_PR$siteID, unique_surveys_PR$date, 
                                    unique_surveys_PR$circle, unique_surveys_PR$survey,
                                    unique_surveys_PR$exclosure)
unique_surveys_count_PR <- data.frame(table(unique_surveys_PR[, c("exclosure", 
                                                               "siteID", 
                                                               "circle", 
                                                               "survey", 
                                                               "date", "surveyID")]))
unique_surveys_count_PR = unique_surveys_count_PR[unique_surveys_count_PR$Freq> 0,]

#Create 3 New Dataframes Merging List of Unique Surveys with Summary Observations 
#for Each Unique Survey for Each of the 3 Food Types
all_abundance_PR <- merge(unique_surveys_count_PR, total_all_PR,
                                                   by.x="surveyID",
                                                   by.y = "surveyID", 
                                                   all.x = TRUE)
all_abundance1_PR<- select(all_abundance_PR, -Freq,
                                             -exclosure.y, 
                                             -siteID.y, 
                                             -circle.y, 
                                             -survey.y 
                                             -date.y, 
                                             -surveyID)
names(all_abundance1) <- c('siteID','date', "Station", "TrapType","total_all")
all_abundance1[is.na(all_abundance1)] <- 0

food_abundance <- merge(unique_surveys_count, total_food, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
food_abundance1<- select(food_abundance, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(food_abundance1) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_food")
food_abundance1[is.na(food_abundance1)] <- 0

caterpillar_abundance <- merge(unique_surveys_count, total_caterpillars, by.x="surveyID", by.y = "surveyID", all.x = TRUE)
caterpillar_abundance1<- select(caterpillar_abundance, -Freq,-TrapType.y, -StateRouteStop.y, -Station.y, -VisitNumber.y, -surveyID)
names(caterpillar_abundance1) = c('StateRouteStop','VisitNumber', "Station", "TrapType","total_caterpillars")
caterpillar_abundance1[is.na(caterpillar_abundance1)] <- 0


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

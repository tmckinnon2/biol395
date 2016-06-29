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
ex_pairs1 <- merge(ex_pairs, exclosures, by.x = "orderID", by.y= "orderID", all.x = TRUE)
ex_pairs2 <- select(ex_pairs1, -c(21:38), -identifier.y)
ex_pairs2[is.na(ex_pairs2)] <- "no"
names(ex_pairs2) <- c("OrderID", "surveyID", "siteID", "userID", "circle",
                      "survey", "timeStart", "temperatureMin", "temperatureMax",
                      "siteNotes", "plantSpecies", "herbivory", "isValid",
                      "surveyType", "leafCount", "orderArthropod", "orderLength",
                      "orderNotes", "orderCount", "identifier", "exclosure")
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
#Subset into NCBG and PR DataFrames
PR<- filter(ex_pairs2, siteID == "117")
NCBG <- filter(ex_pairs2, siteID == "8892356")




#Summarise observations for 3 food types
#PR
#Summarise Observations for All Arthropods (Arthropods greater than 2 mm)
grouped_all <- PR %>% group_by(TrapType, StateRouteStop, Station, VisitNumber)
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


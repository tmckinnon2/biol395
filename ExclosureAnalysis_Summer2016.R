# Read in data
setwd("~/Desktop/insect-exclosure")
all_surveys <- read.table("tbl_surveys.txt", 
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

#Subset into NCBG and PR DataFrames
PR<- filter(all_data, siteID == "117")
NCBG <- filter(all_data, siteID == "8892356")

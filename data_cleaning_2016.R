setwd("~/Desktop/insect-exclosure/caterpillars-count-analysis/2016")
################################################
# Script for reading in and cleaning data from
# Caterpillars Count! data exported from the
# phpMyAdmin project site.
# Data include:#
      # tbl_surveys: date, site, time, and temperature of all survey events
      #
      # tbl_orders: arthropod observed, their length, and count associated with
      #             each survey event
      
      # Load required libraries
      library(plyr)
      library(dplyr)
      library(lubridate)
      library(stringr)

      # Source summary_functions.r if have not already
      source("summary_functions_2016.r")
      
      # Read in data
      tempsurveys = read.csv('tbl_surveys.csv', header=F)
      orders = read.csv('tbl_orders.csv', header=F)
      
      names(tempsurveys) = c('surveyID', 'site', 'userID', 'circle', 'survey', 'dateStart',
                             'dateSubmit', 'tempMin', 'tempMax', 'notes', 'plantSp',
                             'herbivory', 'photo', 'isValid', 'status', 'surveyType',
                             'leafCount', 'source')
      names(orders) = c('recordID', 'surveyID', 'arthropod', 'length', 'notes',
                        'count', 'photo', 'time', 'isValid')
      
      # Only include valid entires in surveys
      surveys = tempsurveys[tempsurveys$isValid == 1,]
      
      # Convert 'survey' field to character from factor
      surveys$survey = as.character(surveys$survey)
      
      # Create effortByDay dataframe for use in summary functions
      surveys$date = as.character(as.POSIXlt(word(surveys$dateStart, 1, sep = " "), format = "%Y-%m-%d")) #careful: if you open the csv before reading in the date format will be changed and this won't work
      effortByDay = data.frame(table(surveys[, c('site', 'date')]))
      names(effortByDay) = c('site', 'date', 'numSurveys') #numSurveys is creating a new column?
      effortByDay = effortByDay[effortByDay$numSurveys!=0, ]
      effortByDay$date = as.POSIXlt(effortByDay$date, format = "%Y-%m-%d")
      effortByDay$julianday = yday(effortByDay$date)
      tempyear <- substring(effortByDay$date, 1, 4)
      effortByDay$year = tempyear 
      
      # Merge orders and surveys table
      orders2 = merge(surveys, orders, by = 'surveyID', all.x = T)
      
      orders2$date = as.POSIXlt(word(orders2$dateStart, 1, sep = " "), format = "%Y-%m-%d")
      orders2$julianday = yday(orders2$date)
      
      orders3 = orders2[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                            'plantSp','herbivory', 'surveyType', 'leafCount', 'arthropod','length',
                            'count','notes.y','notes.x')]
      
      # Add column with arthopod order code
      arthcodes = read.csv('arth_codes.csv', header=T)
      arthcodes1 = arthcodes[, c('ArthCode', 'DataName')]
      names(arthcodes1) = c('arthCode', 'arthropod')
      cleandata <- merge(orders3, arthcodes1, all.x = TRUE, sort = FALSE)
      cleandata <- cleandata[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                                 'plantSp','herbivory', 'surveyType', 'leafCount','arthropod','arthCode','length',
                                 'count','notes.y','notes.x')]
      cleandata <- cleandata[order(cleandata$date),]
      
      # Add a column indicating if the leaves were wet
      cleandata$wetLeaves = c()
      tempwet <- sort(c(grep("wet leaves", cleandata$notes.x), grep("Wet leaves", cleandata$notes.x), 
                        grep("very dewy", cleandata$notes.x), grep("Wet Leaves", cleandata$notes.x)))
      cleandata$wetLeaves = rep('no', nrow(cleandata))
      cleandata$wetLeaves[tempwet] = 'yes'
      
      # Fix date class
      cleandata$date = as.character(cleandata$date)
      
      # Add a year column
      tempdate <- substring(cleandata$date, 1, 4)
      cleandata$year = tempdate
      
      # Create list of unique survey events
      events = unique(cleandata[, c("surveyID", "userID", "date", "year", "julianday", "site", "circle", "survey")])
      
      # Change arthCodes to 'NONE' that were previously NA
      cleandata$arthCode[is.na(cleandata$arthCode)] = "NONE"
      
      # Take out large caterpillar colonies
      cleandata1 <- cleandata[!(cleandata$arthCode == "LEPL" & cleandata$count > 10),]
      # or
      #cleandata$count[cleandata$arthCode == "LEPL" & cleandata$count > 5] = 5
      
      # Cleaning beat sheets (PR and BG) and isolating # leaves into a new column
      beatsheet_pre2016 <- cleandata1[grep("BEAT SHEET", cleandata1$notes.x), ] 
      beatsheet_post2016 <- cleandata1[((cleandata1$leafCount != "50") & (cleandata1$year>= "2016")) | (cleandata1$surveyType=="Beat_Sheet"),] #separates beatsheets in to their own dataframe
      
      leavesNumTemp0 <- word(beatsheet_pre2016$notes.x, -1, sep = "BEAT SHEET; ")
      leavesNumTemp <- word(leavesNumTemp0, -1, sep = "= ")
      leavesNumTemp1 <- word(leavesNumTemp, -1, sep = "Leaves  ")
      leavesNumTemp2 <- word(leavesNumTemp1, -1, sep = "Leaves=")
      leavesNumTemp3 <- word(leavesNumTemp2, 1, sep = ";")
      leavesNumTemp4 <- word(leavesNumTemp3, 1, sep = ",")
      leavesNumTemp5 <- gsub(" ", "", leavesNumTemp4)
      leavesNumTemp6 <- gsub("\n", "", leavesNumTemp5)
      leavesNumTemp7 <- gsub("Unknown", NA, leavesNumTemp6)
      leavesNumTemp8 <- gsub("unknown", NA, leavesNumTemp7)
      beatsheet_pre2016$leavesNum <- as.numeric(leavesNumTemp8)
      beatsheet_post2016$leavesNum = NA
      beatsheet<-rbind(beatsheet_pre2016, beatsheet_post2016)
      beatsheet$surveyType <- "Beat_Sheet"
      cleandata1$leavesNum = NA
      visualdata <- cleandata1[!cleandata1$surveyID %in% beatsheet$surveyID, ]
      cleandata2 <- rbind(visualdata, beatsheet)
      names(cleandata2) <- c("surveyID", "userID", "site", "survey", "circle", "date",
                           "julianday", "plantSp", "herbivory", "surveyType", "leafCount", "arthropod", "arthCode",
                           "length", "count", "notes.y", "notes.x", "wetLeaves", "year", "leavesNum")
      cleandata2["surveyType"][is.na(cleandata2["surveyType"])] <- "Visual"
     
      #-----------------------------------------------------------------------------------------------------------------
      
      ## Calculating biomass and adding this as a column to the clean data
      
      
      # Create empty biomass vector

      cleandata3 = cleandata2[!is.na(cleandata2$surveyID),]
      
      cleandata3$biomass = numeric(length=nrow(cleandata3))
      
      # y = a(x)^b
      # Read in arthropod regression data with slope = b and intercept = log(a)
      reg.data.temp <- read.csv('arth_regression.csv', header = T, sep = ',')
      # Calculate a (the coefficient)
      reg.data.temp$coefficient <- 10^(reg.data.temp$intercept)
      
      # Create list of arthropod orders (by code)
      arthlist <- as.character(reg.data.temp$arthCode) # arthcodes from data_cleaning.R
      
       # Merge reg.data.temp and arthlist so NAs will be calculated
      reg.data <- merge(reg.data.temp, arthcodes, by.x = 'arthCode', by.y = 'ArthCode', all = T)
     
       # For loop for calculating biomass for each observation
      # For loop for calculating biomass for each observation
      for (ord in arthlist) {
        b = reg.data[reg.data$arthCode == ord,]$slope
        a = reg.data[reg.data$arthCode == ord,]$coefficient
        cleandata3$biomass[cleandata3$arthCode == ord] <- (a*(cleandata3$length[cleandata3$arthCode == ord])^(b))*(cleandata3$count[cleandata3$arthCode == ord])
      }
      
      
      # Orders with regression data:
      regorders <- as.vector(reg.data.temp$arthCode)
      
      #Removing exclosure trees (only 2016)    
      finaldata <-filter(cleandata3, !(grepl("EXCLOSURE", notes.x)))
      
      # Subsetting cleandata now that it has the biomass column included
      finaldata.pr <- finaldata[finaldata$site == 117 & finaldata$year == 2016,]
      finaldata.bg <- finaldata[finaldata$site == 8892356 & finaldata$year == 2016,]
      
      amsurvey.pr <- surveySubset(finaldata.pr, subset = "visual am", minLength = 5)
      pmsurvey.pr <- surveySubset(finaldata.pr, subset = "visual pm", minLength = 5)
      beatsheet.pr <- surveySubset(finaldata.pr, subset = "beat sheet", minLength = 5)
      volunteer.pr <- surveySubset(finaldata.pr, subset = "volunteer", minLength = 5)
     
      amsurvey.bg <- surveySubset(finaldata.bg, subset = "visual am", minLength = 5)
      beatsheet.bg <- surveySubset(finaldata.bg, subset = "beat sheet", minLength = 5) 
     
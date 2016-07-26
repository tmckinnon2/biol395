################################################
# Script for general plotting of data from
# Caterpillars Count! data exported from the
# phpMyAdmin project site.
# ****************Need to go through and add effortByDay argument**************

# Run summary_functions.r and data_cleaning.R
source('summary_functions.r')
source('data_cleaning_2016.R')

#################
#---- Prairie Ridge by day----
#################

#dev.off()

# Caterpillars only, mean density
PRam.lepl = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
PRbs.lepl = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c( 'blue', 'orange'))

# Caterpillars only, fraction of surveys with at least one caterpillar
#PRam.1lepl = meanDensityByDay(labsurvey, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(135, 250))
#PRbs.1lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#      col = c('blue', 'orange', 'red', 'green'))

# All orders, mean density
#PRam.all = meanDensityByDay(labsurvey, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#PRbs.all = meanDensityByDay(beatsheet, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#PRpm.all = meanDensityByDay(repsurvey, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
#PRvol.all = meanDensityByDay(volsurvey, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult = meanDensityByDay(amsurvey.pr, ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, 
                             xlim = c(135, 250), ylim = c(0, .8))
PRbs.mult = meanDensityByDay(beatsheet.pr, ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))


#----------------------------------------------------------------------------------------------
#---- Prairie Ridge by week----
# Plots as above but averaged per week instead of by day

# Caterpillars only, mean density ##this one isn't working... no uses objects from summary functions, but summaryfunctions is pulling the data from the beatsheet line of code that I took out of data_cleaing
PRam.lepl.wk = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
PRbs.lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

# Caterpillars only, fraction of surveys with at least one caterpillar
PRam.1lepl.wk = meanDensityByWeek(amsurvey.pr, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(20, 35), ylim = c(0, 0.3))
PRbs.1lepl.wk = meanDensityByWeek(beatsheet.pr, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

# All orders, mean density
#PRam.all.wk = meanDensityByWeek(labsurvey, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#PRbs.all.wk = meanDensityByWeek(beatsheet, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#PRpm.all.wk = meanDensityByWeek(repsurvey, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
#PRvol.all.wk = meanDensityByWeek(volsurvey, "All", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult.wk = meanDensityByWeek(labsurvey, ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                 plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, xlim = c(20, 35),
                                 ylim = c(0.1, 1))
PRbs.mult.wk = meanDensityByWeek(beatsheet, ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                 plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)

legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))
#----------------------------------------------------------------------------------------------------------------------------


####################
#---- Botanical Garden by day ----
####################

# Plot our morning surveys and our beat sheet surveys for the botanical garden
# Caterpillars only, mean density
BGam.lepl = meanDensityByDay(visualsurveybg, "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

# Caterpillars only, fraction of surveys with at least one caterpillar
#BGam.1lepl = meanDensityByDay(visualsurveybg, "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(135, 250))
#BGbs.1lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# All orders, mean density
#BGam.all = meanDensityByDay(visualsurveybg, "All", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#BGbs.all = meanDensityByDay(beatsheet, "All", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
BGam.mult = meanDensityByDay(visualsurveybg, ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, 
                             plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.mult = meanDensityByDay(beatsheet, ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, 
                             plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

#----------------------------------------------------------------------------------------------------------------------------
#---- Botanical Garden by week----
# Plots as above but averaged per week instead of by day

# Plot our morning surveys and our beat sheet surveys for the botanical garden
# Caterpillars only, mean density
BGam.lepl.wk = meanDensityByWeek(visualsurveybg, "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

# Caterpillars only, fraction of surveys with at least one caterpillar
#BGam.1lepl.wk = meanDensityByWeek(visualsurveybg, "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(135, 250))
#BGbs.1lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# All orders, mean density
#BGam.all.wk = meanDensityByWeek(visualsurveybg, "All", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#BGbs.all.wk = meanDensityByWeek(beatsheet, "All", inputYear = 2016, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
BGam.mult.wk = meanDensityByWeek(visualsurveybg, ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, 
                                 plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.mult.wk = meanDensityByWeek(beatsheet, ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, 
                                 plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

#----------------------------------------------------------------------------------------------------------------------------

#################
#---- Frass ----
#################

# Working with frass data
frass = frassData(open = T)

# If does not open, read from git
frass = read.csv('data/frass.csv', header=T)

# Convert date class
frass$Date.Set = as.Date(frass$Date.Set, format = "%m/%d/%Y")
frass$Date.Collected = as.Date(frass$Date.Collected, format = "%m/%d/%Y")
frass$Time.Set = as.character(frass$Time.Set)
frass$jday.Set = julianDayTime(frass$Date.Set, frass$Time.Set)
frass$Time.Collected = as.character(frass$Time.Collected)
frass$jday.Collected = julianDayTime(frass$Date.Collected, frass$Time.Collected)

frass$frass.mg.d = frass$Frass.mass..mg./(frass$jday.Collected - frass$jday.Set)

frass$jday = floor(frass$jday.Collected)
frass$week = floor(frass$jday/7) + 1

meanFrassByDay = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$jday), function(x) mean(x, na.rm=T))
meanFrassByWeek = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$week), function(x) mean(x, na.rm=T))
names(meanFrassByWeek) = c('site', 'week', 'frass.mg.d')
names(meanFrassByDay) = c('site', 'julianday', 'frass.mg.d')

PRfrassD = subset(meanFrassByDay, site == 'Prairie Ridge')
PRfrassW = subset(meanFrassByWeek, site == 'Prairie Ridge')

frassplot = function(site, frassdata, color = 'black', new = T) {
  temp = subset(frassdata, site == site)
  if (new) {
    plot(temp$julianday, temp$frass.mg.d, xlab = "Julian day", ylab = "Mean frass (mg / trap / day)",
         type = 'b', col = color, ylim = range(frassdata$frass.mg.d), xlim = range(frassdata$julianday))
  } else {
    points(temp$julianday, temp$frass.mg.d, type = 'b', col = color)
  }
}


#---- Prairie Ridge fraction of surveys LEPL----


# Prairie Ridge fraction of surveys with caterpillars plot
#pdf('plots/PR_LEPL_frac_by_week_wFrass.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2, mfrow = c(1,1))
plot(c(20,35), c(0, 0.25), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(amsurvey.pr, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet.pr, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(pmsurvey.pr, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volunteer.pr, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

par(new = T)
plot(PRfrassW$week, PRfrassW$frass.mg.d, type = 'b', col = 'darkgreen', lwd = 1, xlim = c(20, 35), ylim = c(1,7),
     xlab = "", xaxt = "n", ylab = "", yaxt = "n")
axis(4, 1:7, cex.axis = 1.2)
mtext("Frass (mg/d)", 4, line = 2.5, cex = 1.5)
legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers', 'Frass'),
       lwd = c(3, 3, 3, 3, 1), lty = c(rep('solid', 3), 'dashed', 'solid'),
       col = c('blue', 'skyblue', 'red', 'red', 'darkgreen'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
dev.off()

# As above but without frass
pdf('plots/PR_LEPL_frac_by_week.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2)
plot(c(20,35), c(0, 0.24), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2016, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers'),
       lwd = c(3, 3, 3, 3), lty = c(rep('solid', 3), 'dashed'),
       col = c('blue', 'skyblue', 'red', 'red'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
dev.off()

#-------------------------------------------------------------------------------------------------------

#---- Merge mean densities into one dataframe ----

# Just caterpillars:
# Prairie Ridge
PRall.lepl1 = merge(PRam.lepl[,c('julianday','meanDensity')], PRbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.lepl1) = c('julianday','density_am','density_bs')
PRall.lepl2 = merge(PRall.lepl1, PRpm.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl2)[4] = 'density_pm'
PRall.lepl = merge(PRall.lepl2, PRvol.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl)[5] = 'density_vol'
# Botanical Garden
BGall.lepl = merge(BGam.lepl[,c('julianday','meanDensity')], BGbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(BGall.lepl) = c('julianday','density_am','density_bs')

# Selected orders:
# Prairie Ridge
PRall.mult1 = merge(PRam.mult[,c('julianday','meanDensity')], PRbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.mult1) = c('julianday','density_am','density_bs')
PRall.mult2 = merge(PRall.mult1, PRpm.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult2)[4] = 'density_pm'
PRall.mult = merge(PRall.mult2, PRvol.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult)[5] = 'density_vol'
# Botanical Garden
BGall.mult = merge(BGam.mult[,c('julianday','meanDensity')], BGbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(BGall.mult) = c('julianday','density_am','density_bs')





#-----------------------------------------------------------------------------------------------------
#---- Plotting for powerpoint for Chris Goforth ----

par(mfrow = c(1,1), mar = c(4,4,3,2), oma = c(1,1,0,0))

# Mean density by day

PRam.lepl = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             ylim = c(0,.15), xaxt='n', ann=FALSE)
PRbs.lepl = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.lepl = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.lepl = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean density", 2, line = 2.5, cex = 1.5)
title(main = 'Caterpillars - Mean Density')

# Temporary fix of caterpillar colony data classified as "OTHER":
volunteer.pr <- volunteer.pr[!(volunteer.pr$arthCode == "NONE" & volunteer.pr$count > 10),]

PRam.all = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = 'All', inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2, 
                            ylim = c(0.05,.9), xaxt='n', ann=FALSE)
PRbs.all = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "All", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.all = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "All", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.all = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "All", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean density", 2, line = 2.5, cex = 1.5)
title(main = 'All Arthropods - Mean Density')


# Biomass by day

PRam.leplbm = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = T, color = 'blue', minLength = 5, lwd = 2, 
                               ylim = c(0,.6), xaxt='n', ann=FALSE)
PRbs.leplbm = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.leplbm = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.leplbm = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean biomass", 2, line = 2.5, cex = 1.5)
title(main = 'Caterpillars - Mean Biomass')

PRam.allbm = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = T, color = 'blue', minLength = 5, lwd = 2, 
                              ylim = c(0,16), xaxt='n', ann=FALSE)
PRbs.allbm = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.allbm = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.allbm = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean biomass", 2, line = 2.5, cex = 1.5)
title(main = 'Multiple Orders - Mean Biomass')



#---- Ellipse chart, bivariates, correlation matrices ----

## Make a giant dataset with all mean densities, biomasses, and frass for by day
# (for standard julian days from core morning survey days)
everything1 = merge(PRam.all[, c(1,8)], PRbs.all[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything2 = merge(everything1, PRpm.all[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
names(everything2) = c('julianday', 'am all density', 'bs all density', 'pm all density')
everything3 = merge(everything2, PRvol.all[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything4 = merge(everything3, PRam.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
names(everything4) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density')
everything5 = merge(everything4, PRbs.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything6 = merge(everything5, PRpm.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
names(everything6) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density')
everything7 = merge(everything6, PRvol.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything8 = merge(everything7, PRam.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything8) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                       'vol lepl density', 'am selected bm')
everything9 = merge(everything8, PRbs.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything10 = merge(everything9, PRpm.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything10) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm')
everything11 = merge(everything10, PRvol.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything12 = merge(everything11, PRam.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything12) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm',
                        'vol selected bm', 'am lepl bm')
everything13 = merge(everything12, PRbs.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything14 = merge(everything13, PRpm.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything14) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm',
                        'vol selected bm', 'am lepl bm', 'bs lepl bm', 'pm lepl bm')
everything15 = merge(everything14, PRvol.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything16 = merge(everything15, PRfrassD[,c(3,2)], by = 'julianday', all.x = T, all.y = F) 
names(everything16) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm',
                        'vol selected bm', 'am lepl bm', 'bs lepl bm', 'pm lepl bm', 
                        'vol lepl bm', 'frass')
corevery = everything16

## Make a correlation chart
round(cor(corevery[,c(2:18)], use = 'pairwise.complete.obs'),2)

# Smaller, more readable subsets of that correlation chart
# Just all arthropods mean density
all.dens = round(cor(corevery[,c(2:5)], use = 'pairwise.complete.obs'),2)
#write.csv(all.dens, 'all.dens.csv')
# Just caterpillar mean density
lepl.dens = round(cor(corevery[,c(6:9,18)], use = 'pairwise.complete.obs'),2)
#write.csv(lepl.dens, 'lepl.dens.csv')
# Just selected arthropods biomass
arth.bm = round(cor(corevery[,c(10:13)], use = 'pairwise.complete.obs'),2)
#write.csv(arth.bm, 'arth.bm.csv')
# Just caterpillar biomass
lepl.bm = round(cor(corevery[,c(14:17,18)], use = 'pairwise.complete.obs'),2)
#write.csv(lepl.bm, 'lepl.bm.csv')


## Make an ellipse chart

# Libraries
library(ellipse)
library(RColorBrewer)

ellipsedata=round(cor(corevery[,c(2:17)], use = 'pairwise.complete.obs'),2)
ellipsedataA=round(cor(corevery[,c(2:9,18)], use = 'pairwise.complete.obs'),2)
ellipsedataB=round(cor(corevery[,c(10:18)], use = 'pairwise.complete.obs'),2)

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "BrBG")
my_colors=colorRampPalette(my_colors)(101)

# Plot ellipse chart
par(mfrow = c(1,1), mar = c(1,3,3,1), oma = c(1,1,0,0))
plotcorr(ellipsedata, col=my_colors[ellipsedata*50+51] , mar=c(1,1,1,1), cex.lab = 0.75)
plotcorr(ellipsedataA, col=my_colors[ellipsedataA*50+51] , mar=c(1,1,1,1), cex.lab = 0.75)
plotcorr(ellipsedataB, col=my_colors[ellipsedataB*50+51] , mar=c(1,1,1,1), cex.lab = 0.75)


## Bivariate plots

# Density
par(mfrow = c(1,1), mar = c(4,4,3,2), oma = c(1,1,0,0))
plot(corevery$`am all density`, corevery$`pm all density`, xlab = 'AM density', ylab = 'PM density', 
     main = "PM vs. AM All Arthropod Density By Day")
abline(0,1)
plot(corevery$`am lepl density`, corevery$`pm lepl density`, xlab = 'AM density', ylab = 'PM density', 
     main = "PM vs. AM Caterpillar Density By Day")
abline(0,1)
plot(corevery$`am all density`, corevery$`bs all density`, xlab = 'AM density', ylab = 'Beat sheet density', 
     main = "Beat Sheet vs. AM All Arthropod Density By Day")
abline(0,1)
plot(corevery$`am lepl density`, corevery$`bs lepl density`, xlab = 'AM density', ylab = 'Beat sheet density', 
     main = "Beat Sheet vs. AM Caterpillar Density By Day")
abline(0,1)
plot(corevery$`pm all density`, corevery$`vol all density`, xlab = 'PM density', ylab = 'Volunteer density', 
     main = "Volunteer vs. PM All Arthropod Density By Day")
abline(0,1)
plot(corevery$`pm lepl density`, corevery$`vol lepl density`, xlab = 'PM density', ylab = 'Volunteer density', 
     main = "Volunteer vs. PM Caterpillar Density By Day")
abline(0,1)

# Biomass
par(mfrow = c(1,1), mar = c(4,4,3,2), oma = c(1,1,0,0))
plot(corevery$`am selected bm`, corevery$`pm selected bm`, xlab = 'AM biomass', ylab = 'PM biomass', 
     main = "PM vs. AM All Arthropod Biomass By Day")
abline(0,1)
plot(corevery$`am lepl bm`, corevery$`pm lepl bm`, xlab = 'AM biomass', ylab = 'PM biomass', 
     main = "PM vs. AM Caterpillar Biomass By Day")
abline(0,1)
plot(corevery$`am selected bm`, corevery$`bs selected bm`, xlab = 'AM biomass', ylab = 'Beat sheet biomass', 
     main = "Beat Sheet vs. AM All Arthropod Biomass By Day")
abline(0,1)
plot(corevery$`am lepl bm`, corevery$`bs lepl bm`, xlab = 'AM biomass', ylab = 'Beat sheet biomass', 
     main = "Beat Sheet vs. AM Caterpillar Biomass By Day")
abline(0,1)
plot(corevery$`pm selected bm`, corevery$`vol selected bm`, xlab = 'PM biomass', ylab = 'Volunteer biomass', 
     main = "Volunteer vs. PM All Arthropod Biomass By Day")
abline(0,1)
plot(corevery$`pm lepl bm`, corevery$`vol lepl bm`, xlab = 'PM biomass', ylab = 'Volunteer biomass', 
     main = "Volunteer vs. PM Caterpillar Biomass By Day")
abline(0,1)


#---- Pie charts ----
par(mfrow = c(2,2), mar = c(1,1,1,1))

# Volunteers (need to fix NAs)
slices <- c(sum(volunteer.pr$count[volunteer.pr$arthCode == 'ARAN'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'AUCH'], na.rm = T), 
            sum(volunteer.pr$count[volunteer.pr$arthCode == 'COLE'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'DIPT'], na.rm = T),
            sum(volunteer.pr$count[volunteer.pr$arthCode == 'LEPL'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'OPIL'], na.rm = T),
            sum(volunteer.pr$count[volunteer.pr$arthCode == 'ORTH'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'HEMI'], na.rm = T))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Volunteer Counts", 
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))

# Lab pm
slices <- c(sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'ARAN']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'AUCH']), 
            sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'COLE']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'DIPT']),
            sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'LEPL']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'OPIL']),
            sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'ORTH']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'HEMI']))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Lab PM Counts",
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))

# Lab am
slices <- c(sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'ARAN']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'AUCH']), 
            sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'COLE']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'DIPT']),
            sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'LEPL']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'OPIL']),
            sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'ORTH']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'HEMI']))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Lab AM Counts",
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))

# Beat Sheet
slices <- c(sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'ARAN']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'AUCH']), 
            sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'COLE']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'DIPT']),
            sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'LEPL']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'OPIL']),
            sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'ORTH']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'HEMI']))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Beat Sheet Counts",
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))



#-----------------------------------------------------------------------------------------------------------------------

par(mfrow = c(1,1), mar = c(4,4,3,2), oma = c(1,1,0,0))

#---- By week plotting, etc. for powerpoint ----

PRam.leplwk = meanDensityByWeek(amsurvey.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                                ylim = c(0,.3), xaxt='n', ann=FALSE)
PRbs.leplwk = meanDensityByWeek(beatsheet.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.leplwk = meanDensityByWeek(pmsurvey.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.leplwk = meanDensityByWeek(volunteer.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1.5, cex = 1.5)
mtext("Mean density", 2, line = 2.5, cex = 1.5)
title(main = 'Caterpillars - Mean Density By Week')

# Temporary fix of caterpillar colony data classified as "OTHER":
volunteer.pr <- volunteer.pr[!(volunteer.pr$arthCode == "NONE" & volunteer.pr$count > 10),]

# More editing to do with volunteer data (all eggs and caterpillars)
#volunteer.pr[(volunteer.pr$arthCode == "NONE" & volunteer.pr$count > 10),]

PRam.allwk = meanDensityByWeek(amsurvey.pr, ordersToInclude = 'All', inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2, 
                               ylim = c(0.7,2.3), xaxt='n', ann=FALSE)
PRbs.allwk = meanDensityByWeek(beatsheet.pr, ordersToInclude = "All", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.allwk = meanDensityByWeek(pmsurvey.pr, ordersToInclude = "All", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.allwk = meanDensityByWeek(volunteer.pr, ordersToInclude = "All", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1.5, cex = 1.5)
mtext("Mean density", 2, line = 2.5, cex = 1.5)
title(main = 'All Arthropods - Mean Density By Week')


# Biomass by week
# need to fix
PRam.leplbmwk = meanDensityByWeek(amsurvey.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = T, color = 'blue', minLength = 5, lwd = 2, 
                                  ylim = c(0,.6), xaxt='n', ann=FALSE)
PRbs.leplbmwk = meanDensityByWeek(beatsheet.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.leplbmwk = meanDensityByWeek(pmsurvey.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.leplbmwk = meanDensityByWeek(volunteer.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1.5, cex = 1.5)
mtext("Mean biomass", 2, line = 2.5, cex = 1.5)
title(main = 'Caterpillars - Mean Biomass By Week')

# need to fix
PRam.allbmwk = meanDensityByWeek(amsurvey.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = T, color = 'blue', minLength = 5, lwd = 2, 
                                 ylim = c(0,16), xaxt='n', ann=FALSE)
PRbs.allbmwk = meanDensityByWeek(beatsheet.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.allbmwk = meanDensityByWeek(pmsurvey.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.allbmwk = meanDensityByWeek(volunteer.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1.5, cex = 1.5)
mtext("Mean biomass", 2, line = 2.5, cex = 1.5)
title(main = 'Multiple Orders - Mean Biomass By Week')

## Make a giant dataset with all mean densities, biomasses, and frass for by WEEK
# (from core morning survey weeks)
everyweek1 = merge(PRam.allwk[, c(1,5)], PRbs.allwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
everyweek2 = merge(everyweek1, PRpm.allwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
names(everyweek2) = c('week', 'am all density', 'bs all density', 'pm all density')
everyweek3 = merge(everyweek2, PRvol.allwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
everyweek4 = merge(everyweek3, PRam.leplwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
names(everyweek4) = c('week', 'am all density', 'bs all density', 'pm all density', 
                      'vol all density', 'am lepl density')
everyweek5 = merge(everyweek4, PRbs.leplwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
everyweek6 = merge(everyweek5, PRpm.leplwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
names(everyweek6) = c('week', 'am all density', 'bs all density', 'pm all density', 
                      'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density')
everyweek7 = merge(everyweek6, PRvol.leplwk[,c(1,5)], by = 'week', all.x = T, all.y = F)
everyweek8 = merge(everyweek7, PRfrassW[,c(2,3)], by = 'week', all.x = T, all.y = F)
names(everyweek8) = c('week', 'am all density', 'bs all density', 'pm all density', 
                      'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                      'vol lepl density', 'frass')


corweek = everyweek8

## Make a correlation chart
round(cor(corweek[,c(2:10)], use = 'pairwise.complete.obs'),2)

# Smaller, more readable subsets of that correlation chart
# Just all arthropods mean density
all.denswk = round(cor(corweek[,c(2:5)], use = 'pairwise.complete.obs'),2)
#write.csv(all.denswk, 'all.denswk.csv')
# Just caterpillar mean density
lepl.denswk = round(cor(corweek[,c(6:9,10)], use = 'pairwise.complete.obs'),2)
#write.csv(lepl.denswk, 'lepl.denswk.csv')

# Just selected arthropods biomass
#arth.bmwk = round(cor(corweek[,c(10:13,18)], use = 'pairwise.complete.obs'),2)
#write.csv(arth.bmwk, 'arth.bmwk.csv')
# Just caterpillar biomass
#lepl.bmwk = round(cor(corweek[,c(14:17,18)], use = 'pairwise.complete.obs'),2)
#write.csv(lepl.bmwk, 'lepl.bmwk.csv')


## Make an ellipse chart

ellipsedata2=round(cor(corweek[,c(2:10)], use = 'pairwise.complete.obs'),2)

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "BrBG")
my_colors=colorRampPalette(my_colors)(101)

# Plot ellipse chart
plotcorr(ellipsedata2, col=my_colors[ellipsedata2*50+51] , mar=c(1,1,1,1), cex.lab = 0.75)

## Bivariate plots
par(mfrow = c(1,1), mar = c(4,4,3,2), oma = c(1,1,0,0))
plot(corweek$`am all density`, corweek$`pm all density`, xlab = 'AM density', ylab = 'PM density', 
     main = "PM vs. AM All Arthropod Density By Week")
#abline(lm(corweek$`pm all density`~corweek$`am all density`))
abline(0,1)
plot(corweek$`am lepl density`, corweek$`pm lepl density`, xlab = 'AM density', ylab = 'PM density', 
     main = "PM vs. AM Caterpillar Density By Week")
abline(0,1)
plot(corweek$`am all density`, corweek$`bs all density`, xlab = 'AM density', ylab = 'Beat sheet density', 
     main = "Beat Sheet vs. AM All Arthropod Density By Week")
abline(0,1)
plot(corweek$`am lepl density`, corweek$`bs lepl density`, xlab = 'AM density', ylab = 'Beat sheet density', 
     main = "Beat Sheet vs. AM Caterpillar Density By Week")
abline(0,1)
plot(corweek$`pm all density`, corweek$`vol all density`, xlab = 'PM density', ylab = 'Volunteer density', 
     main = "Volunteer vs. PM All Arthropod Density By Week")
abline(0,1)
plot(corweek$`pm lepl density`, corweek$`vol lepl density`, xlab = 'PM density', ylab = 'Volunteer density', 
     main = "Volunteer vs. PM Caterpillar Density By Week")
abline(0,1)




# Fraction of surveys by week with caterpillars, min length of 0 (Lunchbunch plot)

amsurvey.pr0 <- surveySubset(cleandata.pr, subset = "visual am", minLength = 0)
pmsurvey.pr0 <- surveySubset(cleandata.pr, subset = "visual pm", minLength = 0)
beatsheet.pr0 <- surveySubset(cleandata.pr, subset = "beat sheet", minLength = 0)
volunteer.pr0 <- surveySubset(cleandata.pr, subset = "volunteer", minLength = 0)

amsurvey.bg0 <- surveySubset(cleandata.bg, subset = "visual am", minLength = 0)
beatsheet.bg0 <- surveySubset(cleandata.bg, subset = "beat sheet", minLength = 0)


par(mgp = c(3, 1, 0), mar = c(3, 5, 3, 4), cex.lab = 1.5, cex.axis = 1.2, mfrow = c(1,1))
plot(c(20,35), c(0, 0.25), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam0 = meanDensityByWeek(amsurvey.pr0, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs0 = meanDensityByWeek(beatsheet.pr0, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm0 = meanDensityByWeek(pmsurvey.pr0, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol0 = meanDensityByWeek(volunteer.pr0, "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

par(new = T)
plot(PRfrassW$week, PRfrassW$frass.mg.d, type = 'b', col = 'darkgreen', lwd = 1, xlim = c(20, 35), ylim = c(1,7),
     xlab = "", xaxt = "n", ylab = "", yaxt = "n")
axis(4, 1:7, cex.axis = 1.2)
mtext("Frass (mg/d)", 4, line = 2.5, cex = 1.5)
legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers', 'Frass'),
       lwd = c(3, 3, 3, 3, 1), lty = c(rep('solid', 3), 'dashed', 'solid'),
       col = c('blue', 'skyblue', 'red', 'red', 'darkgreen'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
title("Fraction of Surveys with Caterpillars", line = 1.5)


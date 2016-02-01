############################
## Laurence Hendry
## R version used: RStudio 0.99.489 
## Hertie School MPP Thesis 
## Thanks to NYU Stern V-Lab for Dataset
############################

############################
## Load packages
############################

# not used:
library(base)
library(rio) # swiss army knife for imports
library(plyr) # count occurences
library(tidyr) # data wrangling
library(ggplot2) # nice plots
library(stargazer) # nicer regression output which looks like a real publication
library(car) # scatterplots 
library(httr) # scraping from http sites
library(XML) # Tool for generating XML file
library(WDI) # Scraping Data from the World Bank
library(countrycode) # provides world bank country codes 
library(gplots)
library(knitr) 

# used:
library(dplyr) # data wrangling
library(readstata13) # helps import from Stata 13
library(DataCombine) # helps with dates
library(lubridate) # helps with dates
library(strucchange) # structural change test package 
library(plm) # package for handling panel data
library(sjPlot) # makes nice graphs, see: http://strengejacke.de/sjPlot/ 
library(foreign) # helps import from other statistical suites
library(fUnitRoots) # unit root tests (including Zivot-Andrews)
library(timeSeries)
library(timeDate)
library(ecp) # R package for nonparametric multiple change point analysis of multivariate data

############################
## Set the correct working directory
############################

setwd("/Users/laurencehendry/GoogleDrive/Master Thesis - Shared/Quantitative Sources/Special Folder/R Laurence Folder - 20.1.16")

############################
## .DTA from STATA as .CSV Import
############################

#import the .csv from STATA 13 (RHolyGrail.csv)

#remove incorrectly formatted data columns
RHolyGrail[9]<- NULL
RHolyGrail[10] <- NULL
RHolyGrail$edate1 <- NULL

#rename variables correctly 
names(RHolyGrail)[1] <-'days'
names(RHolyGrail)[2] <-'MES'
names(RHolyGrail)[3] <-'dailyvariance'
names(RHolyGrail)[4] <-'mscibeta'
names(RHolyGrail)[5] <-'mscicorrelation'
names(RHolyGrail)[6] <-'firmleverage'
names(RHolyGrail)[7] <-'srisk'
names(RHolyGrail)[8] <-'marketcap'

############################
## Get R to read dates correctly
############################

RHolyGrail$Date <- as.Date(RHolyGrail$days, origin = '1900-01-01')

#get a summary of the variables to check
summary(RHolyGrail)

############################
## Sub-set our data
############################
#unit is limited to AMC Aberdeen Asset Management 
RHolyGrail_subset2 <- subset(RHolyGrail, 
                             code_ordinal=="BARC_LN",
                             days >= 36679 | days < 41886)

#timeframe is limited to after 2002-08-10 
#DOESN'T WORK
RHolyGrail_subset2 <- subset(RHolyGrail_subset1, RHolyGrail_subset1$Date >= 2002-08-10,
                             select=c(days:Date))

View(RHolyGrail_subset2)

############################
## Panel regression with the 'plm' package
############################

RHolyGrail_sb <- pdata.frame(RHolyGrail_subset1)

#attempting to find an equivalent command to xtset 
#stumbled upon: http://www.rdocumentation.org/packages/qogdata/functions/xtset
#but 'xtset' does not appear supported in the plm package
#xtset(dataset = RHolyGrail,
#  data = c("code_ordinal", "Date"),
#  spec = c("tradingname_ordinal", "days"),
#  name = "srisk for companies, time series data")

############################
## Structural Break tests
############################

# For structural break tests, see p.56-7 https://cran.r-project.org/web/packages/strucchange/strucchange.pdf 
# and https://cran.r-project.org/web/packages/strucchange/vignettes/strucchange-intro.pdf 
# and p.57 'testing for breaks' http://www.wise.xmu.edu.cn/2007summerworkshop/download/Advanced%20Topics%20in%20Time%20Series%20Econometrics%20Using%20R1_ZongwuCAI.pdf

########## Chow test:

# test whether volatility for Aberdeen Asset Management remained constant over the years

sctest(RHolyGrail_sb$srisk ~ RHolyGrail_sb$Date + RHolyGrail_sb$EU_CRR, 
       type = "Chow", 
       point =7)

########## supF test:

fs.RHolyGrail_sb=Fstats(RHolyGrail_sb$srisk ~ 1)
print(sctest(fs.RHolyGrail_sb))

# set graph parameters
par(mfrow=c(2,1),
    mex = 0.8,
    ps = 9,
    pty = "m",
    bg = "white")

# Plot F-statistics with breakpoint line 
plot(fs.RHolyGrail_sb, main = "supF test")
lines(breakpoints(fs.RHolyGrail_sb))

# Plot RSS & BIC models
bp.RHolyGrail_sb <- breakpoints(RHolyGrail_sb$srisk ~ 1)
plot(bp.RHolyGrail_sb)

########## OLS-CUSUM test:

# CUSUM Plot tests the null hypothesis that srisk volatility remains constant over the years
# Here we compute OLS-based CUSUM process and plot with standard and alternative boundaries
# nb. efp = empirical fluctuation processes

sctest(RHolyGrail_sb$srisk ~ RHolyGrail_sb$Date + RHolyGrail_sb$EU_CRR, 
       type = "OLS-CUSUM", 
       point =7)


print(sctest(ocus.RHolyGrail_sb))
dev.off()

ocus.RHolyGrail_sb=efp(RHolyGrail_sb$srisk ~ 1, 
                       type = "OLS-CUSUM")

# set graph parameters
par(mfrow=c(2,1),
    mex = 0.8,
    ps = 9,
    pty = "m",
    bg = "white")

plot(ocus.RHolyGrail_sb)
plot(ocus.RHolyGrail_sb, alpha = 0.01, alt.boundary = TRUE)

########## Zivot & Andrews test:

# http://artax.karlin.mff.cuni.cz/r-help/library/urca/html/ur.za.html
# see p.9 https://cran.r-project.org/web/packages/fUnitRoots/fUnitRoots.pdf

# Doesn't work: 
za.RHolyGrail_za <- ur.za(RHolyGrail_sb, model="both", lag=2)



############################
## Write to .CSV file
############################

write.table(RHolyGrail, file = "RHolyGrail_worked.csv", row.names=TRUE, sep = ",")

############################
## Generate nice summary statistics
############################
#taking Aberdeen Asset Management as our example

sjp.setTheme(theme = "scatter",
             geom.label.size = 3.5,
             geom.label.color = "black",
             axis.textsize = .8,
             axis.title.size = .9)

sjp.frq(RHolyGrail$country_ordinal, 
        sort.frq = "asc",
        axisTitle.x = "Country",
        axisTitle.y = "Number of Observations",
        coord.flip = TRUE,
        labelPos = "outside")

sjp.frq(RHolyGrail_subset1$firmleverage,
        RHolyGrail_subset1$days,
        type = "dens",
        showNormalCurve = TRUE,
        normalCurveAlpha = .3,
        axisTitle.x = "Time",
        axisTitle.y = "Leverage",
        labelPos = "outside")

plot(RHolyGrail_subset1$firmleverage,
     RHolyGrail_subset1$days)

############################
## Generate nice chronological graphs
############################

########## For generic y over time (x)

# http://www.r-bloggers.com/how-to-plot-a-graph-in-r/
par(mfrow=c(2,1), #multiple graphs 
    mex = 0.8, #margin size
    ps = 9, #font size
    pty = "s", #or m
    bg = "white",
    tcl= -0.8)
axis(1, 
     at=seq(1, 
            445, 
            by=12), 
     labels=F, 
     lwd=1, 
     lwd.ticks=1)
abline(v=(12*(seq(2,32,by=5)))+1, col="lightgray",
       lty="dotted")
abline(h=(seq(0,150,25)), col="lightgray",
       lty="dotted")

plot(RHolyGrail_subset2$Date, 
     RHolyGrail_subset2$srisk, 
     ann=F,
     pch = ".",
     typ='l')
par(tcl= -0.2)
axis(1, at=seq(1, 445, by=12), labels=F, lwd=1, lwd.ticks=1)
par(tcl= -0.5)
axis(1, at=seq(1 + 12*2, 450, by=60), labels=seq(1975,2010,5), lwd=0, lwd.ticks=2)
par(tcl= -0.5)
axis(2)
abline(v=(12*(seq(2,32,by=5)))+1, col="lightgray", lty="dotted")
abline(h=(seq(0,150,25)), col="lightgray", lty="dotted")
title(main="SRISK over time for Barclays", 
      sub="test",
      ylab="SRISK",
      xlab="time")
linear.model = lm(RHolyGrail_subset2$srisk ~ RHolyGrail_subset2$Date)
abline(linear.model, 
       col="blue") #only using the first two of 3 regression coefficients


plot(RHolyGrail_subset1$Date, 
     RHolyGrail_subset1$srisk, 
     main = "SRISK over time for Aberdeen Asset Management",
     pch = ".",
     typ='l')


plot(ocus.RHolyGrail_sb)

########## For outputting leverage over time with sjp

sjp.setTheme(theme  = "scatter",
             geom.label.size = 10,
             axis.title.size = .85,
             legend.size = .8,
             legend.title.size = .8,
             legend.pos = "right")

sjp.scatter(RHolyGrail_subset1$Date, 
            RHolyGrail_subset1$firmleverage, 
            showRug = TRUE, 
            title = "Aberdeen Asset Management Leverage Ratio over time", 
            axisTitle.x = "Time", 
            axisTitle.y = "Leverage Ratio", 
            showGroupFitLine = TRUE, 
            fitmethod = "loess", 
            showTotalFitLine = TRUE)

########## Plotting all SRisk over time

srisk_ts <- ts(RHolyGrail$srisk)
plot.ts(srisk_ts)


############################
## Nonparametric multiple change point analysis of multivariate data
############################

#https://cran.r-project.org/web/packages/ecp/vignettes/ecp.pdf

########## Example to be converted: 

# Load ECP change point package
library(ecp)
library(dplyr)

# Create fake data -----------
fake <- data.frame() # create an empty data frame
for (i in 1:10) {
  base_num <- rnorm(1, mean = 5, sd = 2) 
  period1 <- rnorm(100)
  period2 <- rnorm(100, 0, base_num)
  period3 <- rnorm(100, base_num, 3)
  value <- matrix(c(period1, period2, period3), ncol = 1)
  temp <- data.frame(ID = i, value)
  fake <- rbind(fake, temp)
}

# Run change point model ---------------------------------
# Not data is assumed to be incorrect time series order
# use dplyr::arrange(GROUPING_VAR, TIME_VAR)

fake_with_change_points <- data.frame()
for (i in 1:length(unique(fake$ID))) {
  temp <- fake %>% filter(ID == i)
  temp_out <- e.divisive(X = as.matrix(temp['value']))
  # Extract position of the estimated change points
  estimated_cp <- temp_out$estimates[
    seq(from = 2, to = length(temp_out$estimates) - 1)]
  # Put change points in original data frame for indiv.
  est_changes <- rep(0, nrow(temp)) 
  est_changes[estimated_cp] <- 1
  
  # Combine into one data frame with all units
  temp$est_changes <- est_changes
  fake_with_change_points <- rbind(fake_with_change_points,
                                   temp)
}

## fake_with_change_points includes the original grouped time 
# series with a new dummy variable (est_changes) that is 1
# at the estimated change point and 0 otherwise.





#exploring panel data

coplot (y ~ year|tradingname.ordinal, type="b", data=RHolyGrail)



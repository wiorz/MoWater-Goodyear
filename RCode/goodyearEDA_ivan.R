

setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("clean/goodyearMoWater.rda" )
ls()
library( lubridate)
library( viridis)
library( scales)
library( tidyverse); theme_set(theme_minimal())
    theme_update(panel.grid.minor = element_blank())
suppressMessages(library( fields))

#head(goodyear)

# Prep for matching size first! Some bins have anextra line of NA, row num 
# should be 438.
# Bins with extra rows: bin1, bin2, bin3, bin6
Bin1 <- Bin1[1:nrow(Bin1) - 1, ]
Bin2 <- Bin2[1:nrow(Bin1) - 1, ]
Bin3 <- Bin3[1:nrow(Bin1) - 1, ]
Bin6 <- Bin6[1:nrow(Bin1) - 1, ]

dfData <- dfData[!(is.na(dfData$date)), ] # also clean goodyear here

dfData <- as_tibble(goodyear)

#Data with Selenium focus, removing all NA rows from Selenium
dfDataSel <- dfData[!is.na(dfData$Selenium), ]
#Data with Arsenic focus
dfDataAr <- dfData[!is.na(dfData$Arsenic), ]
#Data with Chronium focus
dfDataChr <- dfData[!is.na(dfData$Chromium), ]


#------------------------------------------------------

#train change date: relevant for bin 2 and 4. 
#Before the change, bin 2 is train 3,
#This can be our starting date since we will only be ignoring 8 months of data.
trainChangeDate <- ymd( "2011-06-15")

#last date for shortcut, the +1 is for adapting to use with functions due to the
#   comparison (< lastdate).
lastDate <- tail(dfDataSel$date, n = 1) + 1

#Note: 2015-04-01 may be set to 2015-01-01 because the data doesn't look right.
#There's a spike in data around Jan 2015 that should be grouped with the next 
# performance period,hence this choice.

#set periods: most bins have different perfmance periods!
#bin1, 5, 6, 7
bin1567Period1End <- ymd( "2012-03-01")
bin1567Period2End <- ymd( "2015-04-01")
bin1567Period3End <- ymd( "2017-04-01")
bin1567Periods <- c(bin1567Period1End, bin1567Period2End, bin1567Period3End)

#periods for bin2
bin2Period1End <- ymd( "2015-04-01")
bin2Period2End <- ymd( "2017-04-01")
bin2Periods <- c(bin2Period1End, bin2Period2End)

#periods for bin3, bin4
bin34Period1End <- ymd( "2015-04-01")
bin34Period2End <- ymd( "2016-12-01")
bin34Periods <- c(bin34Period1End, bin34Period2End)

#---------------------------------------------
#Set functions here

# Description: compare varX to varY for the targeted influent and effluent data, 
#   with specified start and stop period.
#   NOTE: These arguments should be strings: 
#       influent, effluent, interval_string, varX, varY
# Return: a ggplot. Note: still needs xlab, ylab and title.
#
# i.e.: drawInVsOutXYOfPeriod(dfDataSel, "Bin1", "Bin5", bin1567Period1End, 
#                           bin1567Period2End, "date", "Selenium", "4 months")
#       will produce Bin1 as influent vs Bin5 as effluent for Selenium over time
#       for period bin1567Period1End~bin1567Period2End, in 4 months interval.
#

drawInVSOutXYOfPeriod <- function(dataset, influent, effluent, startPeriod, 
                                     endPeriod, varX, varY, interval_string)
{
    resultPlot <- dataset %>% 
        filter(ID == influent | ID == effluent) %>% 
        filter(date >= startPeriod & date < endPeriod ) %>% 
        #aes_string because variable is not actual column factor term, but a name as string.
        ggplot(aes_string(x = varX, y = varY, group = "ID", color = "ID")) + 
        geom_point(size = 2) +  
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") + 
        scale_color_viridis_d()
    
    return(resultPlot) 
}

# Only looking at brine and train 1 (bin1, bin5, bin7) for after the train change.
drawTrain1VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin1" | ID == "Bin5" | 
                   ID == "Bin7" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>% 
        #aes_string because variable is not actual column factor term, but a name as string.
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b%Y") +
        scale_color_viridis_d() + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 1 for period after train change") + 
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        geom_vline(xintercept = bin1567Period1End) + 
        geom_vline(xintercept = bin1567Period2End) + 
        geom_vline(xintercept = bin1567Period3End) + 
        annotate("text", label = "A", x = bin1567Period1End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "B", x = bin1567Period2End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "C", x = bin1567Period3End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "D", x = lastDate - 60, y = 0.2, 
                 size = 8, colour = "red")
    
    return(resultPlot) 
}

# train 2 doesn't show periods because bin 2 and 6 have different periods.
drawTrain2VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin2" | ID == "Bin6" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_viridis_d() + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 2 for period after train change")
    
    return(resultPlot) 
}

drawTrain3VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin4" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_viridis_d() + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 3 for period after train change") + 
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        geom_vline(xintercept = bin34Period1End) + 
        geom_vline(xintercept = bin34Period2End) + 
        annotate("text", label = "A", x = bin34Period1End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "B", x = bin34Period2End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "C", x = lastDate - 60, y = 0.2, 
                 size = 8, colour = "red")
    
    return(resultPlot) 
}

drawTrain4VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin3" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_viridis_d() + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 4 for period after train change") + 
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        geom_vline(xintercept = bin34Period1End) + 
        geom_vline(xintercept = bin34Period2End) + 
        annotate("text", label = "A", x = bin34Period1End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "B", x = bin34Period2End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "C", x = lastDate - 60, y = 0.2, 
                 size = 8, colour = "red")
    
    return(resultPlot) 
}

#TODO: fix period 2 x labels overlapping each other. Maybe disable legends 
#   each plot and only show one on the right so that we have more drawing space.
# Or maybe draw at a larger scale, current the plot set at 1280 x 735. 

#draw grid function
drawGrid2x2 <- function(plot1, plot2, plot3, plot4)
{
    g1 <- ggplotGrob(plot1)
    g2 <- ggplotGrob(plot2)
    g3 <- ggplotGrob(plot3)
    g4 <- ggplotGrob(plot4)
    gr1 <- rbind(g1, g2)
    gr2 <- rbind(g3, g4)
    g <- cbind(gr1, gr2)
    grid.newpage()
    grid.draw(g)    
}

#--------------------------------

#Selenium contents for train 1 for different periods
plotT1vBrine <- drawTrain1VSBrine(dfDataSel, "4 months", "Selenium")
plotT1vBrine

#Train2
plotT2vBrine <- drawTrain2VSBrine(dfDataSel, "4 months", "Selenium")
plotT2vBrine

#Train3
plotT3vBrine <- drawTrain3VSBrine(dfDataSel, "4 months", "Selenium")
plotT3vBrine

plotT4vBrine <- drawTrain4VSBrine(dfDataSel, "4 months", "Selenium")
plotT4vBrine

#---------------------------------------------------

#Selenium contents for individual bins
#Focus: 
#Period 1


#--------------------------------------
#Netflow test

#cleaning inflow and outflow
dfDataSelFlow <- dfDataSel[!is.na(dfDataSel$Inflow), ]
dfDataSelFlow <- dfDataSelFlow[!is.na(dfDataSelFlow$Outflow), ]
#make the result of outflow-inflow as a new column in the data
dfDataSelFlow <- add_column(dfDataSelFlow, 
                            netflow = (dfDataSelFlow$Outflow - 
                                           dfDataSelFlow$Inflow) ) 
# glimpse(dfDataFlow)

# regression analysis of Netflow vs Selenium for Train 1
dfDataSelFlow %>% 
    filter(ID == "Bin1" | ID == "Bin5" | ID == "Bin7") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + # formula always look at the variable in terms of x
    scale_color_viridis_d("Selenium and netflow") +
    xlab("selenium") +  
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 1")

# regression analysis of Netflow vs Selenium for Train 4
dfDataSelFlow %>% 
    filter(ID == "Bin3") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_viridis_d("Selenium and netflow") +
    xlab("selenium") + 
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 4")

#----------------------------------------------



#-----------------------------------

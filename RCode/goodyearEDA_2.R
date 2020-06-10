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


dfData <- as_tibble(goodyear)
glimpse(dfData)

dfDataSel <- dfData[!is.na(dfData$Selenium), ]
glimpse(dfDataSel)

dfDataAr <- dfData[!is.na(dfData$Arsenic), ]


#------------------------------------------------------

#set periods
period1End <- ymd( "2012-03-01")
period2End <- ymd( "2015-01-01")
period3End <- ymd( "2017-04-01")
periods <- c(period1End, period2End, period3End)

# Only looking at brine and train 1 (bin1, bin5, bin7)
drawTrain1FromPeriod <- function(startPeriod, endPeriod, interval_string){
    resultPlot <- dfDataSel %>% 
        filter(dfDataSel$ID == "Bin1" | dfDataSel$ID == "Bin5" | 
                   dfDataSel$ID == "Bin7" | dfDataSel$ID == "brine") %>% 
        filter(date >= startPeriod & date < endPeriod ) %>% 
        ggplot(aes(x = date, y = Selenium, group = ID, color = ID)) + #aes_string because variable is not actual column factor term, but a name as string.+
        geom_line() + 
        geom_point(size = 2) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        xlab("Date") +
        ylab("Selenium Content (mg/L)")
    
    return(resultPlot) 
}

#Selenium contents for train 1 for different periods
#Period 1
plotT1P1 <- drawTrain1FromPeriod(dfDataSel$date[1], period1End, "4 months") + 
    labs(title= "Selenium content of Train 1 for period 1")
plotT1P1

#Period 2
plotT1P2 <- drawTrain1FromPeriod(period1End, period2End, "4 months") + 
    labs(title= "Selenium content of Train 1 for period 2")
plotT1P2

#Period 3
plotT1P3 <- drawTrain1FromPeriod(period2End, period3End, "4 months") + 
    labs(title= "Selenium content of Train 1 for period 3")
plotT1P3

#Period 4
plotT1P4 <- drawTrain1FromPeriod(period3End, 
                                 tail(dfDataSel$date, n = 1) + 1, "1 month") + #the tail() +1 is adding one day to the last day in the dataset
    labs(title= "Selenium content of Train 1 for period 4")
plotT1P4

#TODO: fix period 2 x labels overlapping each other. Maybe disable legends 
#   each plot and only show one on the right so that we have more drawing space.
# Or maybe draw at a larger scale, current the plot set at 1280 x 735. 

#draw grid function
drawGrid2x2 <- function(plot1, plot2, plot3, plot4){
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

drawGrid2x2(plotT1P1, plotT1P2, plotT1P3, plotT1P4)

#Selenium contents for train 4 for differnt periods
drawTrain4FromPeriod <- function(startPeriod, endPeriod, interval_string){
    resultPlot <- dfDataSel %>% 
        filter(dfDataSel$ID == "Bin3" | dfDataSel$ID == "brine") %>% 
        filter(date >= startPeriod & date < endPeriod ) %>% 
        ggplot(aes(x = date, y = Selenium, group = ID, color = ID)) + #aes_string because variable is not actual column factor term, but a name as string.
        geom_line() + 
        geom_point(size = 2) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %y") +
        xlab("Date") +
        ylab("Selenium Content (mg/L)")
    
    return(resultPlot) 
}

#Selenium contents for train 4 for differnt periods
#Period 1
plotT4P1 <- drawTrain4FromPeriod(dfDataSel$date[1], period1End, "4 months") + 
    labs(title= "Selenium content of Train 4 for period 1")
plotT4P1

#Period 2
plotT4P2 <- drawTrain4FromPeriod(period1End, period2End, "4 months") + 
    labs(title= "Selenium content of Train 4 for period 2")
plotT4P2

#Period 3
plotT4P3 <- drawTrain4FromPeriod(period2End, period3End, "4 months") + 
    labs(title= "Selenium content of Train 4 for period 3")
plotT4P3

#Period 4
plotT4P4 <- drawTrain4FromPeriod(period3End, 
                                 tail(dfDataSel$date, n = 1) + 1, "1 month") + 
    labs(title= "Selenium content of Train 4 for period 4")
plotT4P4

drawGrid2x2(plotT4P1, plotT4P2, plotT4P3, plotT4P4)

#--------------------------------------

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




#All variables to be excluded from subtraction:
#   date, Inflow, Outflow, Temp..Celsius, Color, COD, Conductivity.S.m, DO.mg.L, 
#   ORP, pH
tempBin1 <- subset(Bin5, select = -c(date, Inflow, Outflow, Temp..Celsius, 
                                     Color, COD, Conductivity.S.m, DO.mg.L, 
                                     ORP, pH) )
tempBin2 <- subset(Bin1, select = -c(date, Inflow, Outflow, Temp..Celsius, 
                                     Color, COD, Conductivity.S.m, DO.mg.L, 
                                     ORP, pH) )

binMinus1from5 <- (tempBin2 - tempBin1)
binMinus1from5 <- add_column(binMinus1from5, date = Bin1$date) 
binMinus1from5 <- as_tibble(binMinus1from5)

binMinus1from5C <- binMinus1from5[!(is.na(binMinus1from5$Selenium)), ]

binMinus1from5C %>% 
    ggplot(aes(x = date, y = Selenium, group = month(date))) +
    geom_line()

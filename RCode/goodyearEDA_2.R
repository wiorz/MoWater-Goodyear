setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("clean/goodyearMoWater.rda" )
ls()
library( lubridate)
library( viridis)
library( scales)
library( tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
suppressMessages(library( fields))

head(goodyear)

dfData <- as_tibble(goodyear)
glimpse(dfData)

dfDataC <- dfData[!is.na(dfData$Selenium), ]
glimpse(dfDataC)

#------------------------------------------------------

#set periods
period1End <- ymd( "2012-03-01")
period2End <- ymd( "2015-04-01")
period3End <- ymd( "2017-04-01" )
periods <- c(period1End, period2End, period3End)

# Only looking at bin1, bin5, bin7 and brine - same train
drawTrain1FromPeriod <- function(startPeriod, endPeriod, interval_string){
    resultPlot <- dfDataC %>% 
        filter(dfDataC$ID == "Bin1" | dfDataC$ID == "Bin5" | dfDataC$ID == "Bin7" | 
                   dfDataC$ID == "brine") %>% 
        filter(date >= startPeriod & date <= endPeriod ) %>% 
        ggplot(aes(x = date, y = Selenium, group = ID, color = ID)) + #aes_string because variable is not actual column factor term, but a name as string.+
        geom_point(size = 2) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_viridis_d() +
        xlab("Date") +
        ylab("Selenium Content (mg/L)")
    
    return(resultPlot) 
}

#Selenium contents for train 1 for differnt periods
#Period 1
plotT1P1 <- drawTrain1FromPeriod(dfDataC$date[1], period1End, "4 months") + 
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
plotT1P4 <- drawTrain1FromPeriod(period3End, tail(dfDataC$date, n = 1), "1 month") + 
    labs(title= "Selenium content of Train 1 for period 4")
plotT1P4


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
    resultPlot <- dfDataC %>% 
        filter(dfDataC$ID == "Bin3" | dfDataC$ID == "brine") %>% 
        filter(date >= startPeriod & date <= endPeriod ) %>% 
        ggplot(aes(x = date, y = Selenium, group = ID, color = ID)) + #aes_string because variable is not actual column factor term, but a name as string.+
        geom_point(size = 2) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %y") +
        scale_color_viridis_d() +
        xlab("Date") +
        ylab("Selenium Content (mg/L)")
    
    return(resultPlot) 
}

#Selenium contents for train 4 for differnt periods
#Period 1
plotT4P1 <- drawTrain4FromPeriod(dfDataC$date[1], period1End, "4 months") + 
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
plotT4P4 <- drawTrain4FromPeriod(period3End, tail(dfDataC$date, n = 1), "1 month") + 
    labs(title= "Selenium content of Train 4 for period 4")
plotT4P4

drawGrid2x2(plotT4P1, plotT4P2, plotT4P3, plotT4P4)

#--------------------------------------

#cleaning inflow and outflow
dfDataFlow <- dfDataC[!is.na(dfDataC$Inflow), ]
dfDataFlow <- dfDataFlow[!is.na(dfDataFlow$Outflow), ]

dfDataFlow <- add_column(dfDataFlow, netflow = (dfDataFlow$Outflow - dfDataFlow$Inflow) ) 
# glimpse(dfDataFlow)

# regression analysis of Netflow vs Selenium for Train 1
dfDataFlow %>% 
    filter(ID == "Bin1" | ID == "Bin5" | ID == "Bin7") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + # formula always look at the varaible in terms of x
    scale_color_viridis_d("Selenium and netflow") +
    xlab("selenium") +  
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 1")

# regression analysis of Netflow vs Selenium for Train 4
dfDataFlow %>% 
    filter(ID == "Bin3") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + # formula always look at the varaible in terms of x
    scale_color_viridis_d("Selenium and netflow") +
    xlab("selenium") + 
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 4")


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

#set periods
period1End <- ymd( "2012-03-01")
period2End <- ymd( "2015-04-01")
period3End <- ymd( "2017-04-01" )
periods <- c(period1End, period2End, period3End)


#Draw everything against each other
colnames(dfDataC)

for(i in colnames(dfDataC)){
    cur_plot <- ggplot(data = dfDataC, aes_string(x = i, y = "Selenium")) + #aes_string because variable is not actual column factor term, but a name as string.
        geom_line(color = ID ) + 
        geom_point(alpha = 0.3) + 
        geom_smooth(method = lm, color = "blue")
    print(cur_plot) # autoprint is turned off so we need to print it
    #Sys.sleep(2) #sleep so that we can see the output if needed
    
}



# Only looking at bin1, bin5, bin7 and brine - same train
drawTrain1FromPeriod <- function(startPeriod, endPeriod){
    resultPlot <- dfDataC %>% 
        filter(dfDataC$ID == "Bin1" | dfDataC$ID == "Bin5" | dfDataC$ID == "Bin7" | 
                   dfDataC$ID == "brine") %>% 
        filter(date >= startPeriod & date <= endPeriod ) %>% 
        ggplot(aes(x = date, y = Selenium, group = ID, color = ID)) + #aes_string because variable is not actual column factor term, but a name as string.+
        geom_line(size = 1.5) +
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
        scale_color_viridis_d() +
        xlab("Date") +
        ylab("Selenium Content (mg/L)")
    
    return(resultPlot) 
}

#Selenium contents for differnt periods
drawTrain1FromPeriod(dfDataC$date[1], period1End) + 
    labs(title= "Selenium content for Train 1 for period 1")

drawTrain1FromPeriod(period1End, period2End) + 
    labs(title= "Selenium content for Train 1 for period 2")

drawTrain1FromPeriod(period2End, period3End) + 
    labs(title= "Selenium content for Train 1 for period 3")

drawTrain1FromPeriod(period3End, tail(dfDataC$date, n = 1)) + #tail( , n = 1) get last row
    labs(title= "Selenium content for Train 1 for period 4")
    

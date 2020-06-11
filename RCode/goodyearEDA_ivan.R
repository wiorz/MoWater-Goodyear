

setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("clean/goodyearMoWater.rda" )
ls()
library( lubridate)
library( rcartocolor)
library( RColorBrewer)
library( scales)
library( ggpubr)
library( tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
suppressMessages(library( fields))

#head(goodyear)

# Prep for matching size if needed! Some bins have an extra line of NA, row num 
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
# i.e.: GGPInVsOutXYOfPeriod(dfDataSel, "Bin1", "Bin5", bin1567Period1End, 
#                           bin1567Period2End, "date", "Selenium", "4 months")
#       will produce Bin1 as influent vs Bin5 as effluent for Selenium over time
#       for period bin1567Period1End~bin1567Period2End, in 4 months interval.
#

GGPInVSOutXYOfPeriod <- function(dataset, influent, effluent, startPeriod, 
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
GGPTrain1VSBrine <- function(dataset, interval_string, chemical)
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
        scale_color_brewer(palette = "Dark2") + 
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
GGPTrain2VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin2" | ID == "Bin6" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") +  
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 2 for period after train change")
    
    return(resultPlot) 
}

GGPTrain3VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin4" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") + 
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

GGPTrain4VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin3" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(size = 2.5) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") + 
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

#Draw grid function
DrawGrid2x2 <- function(plot1, plot2, plot3, plot4)
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
plotT1vBrine <- GGPTrain1VSBrine(dfDataSel, "4 months", "Selenium")
plotT1vBrine
ggsave(filename = "Images/Train 1 Selenium vs Brine.png", 
       plotT1vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train2
plotT2vBrine <- GGPTrain2VSBrine(dfDataSel, "4 months", "Selenium")
plotT2vBrine
ggsave(filename = "Images/Train 2 Selenium vs Brine.png", 
       plotT2vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train3
plotT3vBrine <- GGPTrain3VSBrine(dfDataSel, "4 months", "Selenium")
plotT3vBrine
ggsave(filename = "Images/Train 3 Selenium vs Brine.png", 
       plotT3vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train 4
plotT4vBrine <- GGPTrain4VSBrine(dfDataSel, "4 months", "Selenium")
plotT4vBrine
ggsave(filename = "Images/Train 4 Selenium vs Brine.png", 
       plotT4vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')

#---------------------------------------------------
#Netflow vs Selenium linear regression check

#cleaning inflow and outflow
dfDataSelFlow <- dfDataSel[!is.na(dfDataSel$Inflow), ]
dfDataSelFlow <- dfDataSelFlow[!is.na(dfDataSelFlow$Outflow), ]
#make the result of outflow-inflow as a new column in the data
dfDataSelFlow <- add_column(dfDataSelFlow, 
                            netflow = (dfDataSelFlow$Outflow - 
                                           dfDataSelFlow$Inflow) ) 
# glimpse(dfDataFlow)

#Regression analysis of Netflow vs Selenium for Train 1
#can also use : scale_color_carto_d("Effluent Selenium") 
dfDataSelFlow %>% 
    filter(ID == "Bin1" | ID == "Bin5" | ID == "Bin7") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(size = 2, alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + # formula always look at the variable in terms of x
    scale_color_brewer(palette = "Dark2") + 
    xlab("selenium") +  
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 1")


# regression analysis of Netflow vs Selenium for Train 4
dfDataSelFlow %>% 
    filter(ID == "Bin3") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("selenium") + 
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 4")

#----------------------------------------------

#boxplot on bons to see which bins to use - 2 tone color
boxSel2t <- dfDataSel %>% 
    filter(year(date) <= 2014 | year(date) >= 2015) %>%
    ggplot(aes(x = ID , y = Selenium, group = ID, color = ID)) +
    geom_boxplot(fill = "grey") +
    scale_color_manual(values = c(rep("black", 7), "red3")) + 
    ylab("Selenium (mg/L)") +
    labs(title = "Selenium Boxplots per Effluent") + 
    theme(legend.position = "none")

boxSel2t

ggsave(filename = "Images/Selenium_Boxplot_two_tone.png", 
       boxSel2t,
       width = 42.3, height = 23.15, units = "cm", device='png')

#multiple color of boxplot base on train
boxSelTC <- dfDataSel %>% 
    filter(year(date) <= 2014 | year(date) >= 2015) %>%
    ggplot(aes(x = ID , y = Selenium, group = ID, color = ID)) +
    geom_boxplot(fill = c("goldenrod4", "darkgreen", "royalblue3", 
                          "red4", "goldenrod4", "darkgreen", 
                          "goldenrod4", "black")) +
    scale_color_manual(values = c("goldenrod4", "darkgreen", "royalblue3", 
                                  "red4", "goldenrod4", "darkgreen", 
                                  "goldenrod4", "black")) + 
    ylab("Selenium (mg/L)") +
    labs(title = "Selenium Boxplots per Effluent") + 
    theme(legend.position = "none")

boxSelTC

ggsave(filename = "Images/Selenium_Boxplot_train_color.png", 
       boxSelTC,
       width = 42.3, height = 23.15, units = "cm", device='png')

#conclusion: bin2, bin3 (baseline),bin4 and bin 6

#-----------------------------------------------

#Exploring DO and carbon linear regression with Selenium

#Bin1 period D
plotSelvCODB1P4 <- dfDataSel %>% 
    filter(ID == "Bin1" & date >= bin1567Period3End ) %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. of Bin1 Period D")
plotSelvCODB1P4
ggsave(filename = "Images/Selenium vs COD linear regression Bin1 Period D.png", 
       plotSelvCODB1P4,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin2 period C
plotSelvCODB2P3 <- dfDataSel %>% 
    filter(ID == "Bin2") %>% 
    filter(date >= bin2Period2End) %>% 
    ggplot(aes(COD, Selenium))+ 
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("COD") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. for Bin2 Period C")
plotSelvCODB2P3
ggsave(filename = "Images/Selenium vs COD linear regression Bin2 Period C.png", 
       plotSelvCODB2P3,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Selenium vs COD for Bin1 and Bin2 during carbon dosing
plotSelvCODB1B2 <- ggarrange(plotSelvCODB1P4, plotSelvCODB2P3, nrow=2, 
                             common.legend = FALSE)
plotSelvCODB1B2
ggsave(filename = "Images/Selenium vs COD linear regression Bin 1 vs Bin2.png", 
       plotSelcCODB1B2,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Focus look at Bin2 DO on last period when carbon dosing happens
plotSelvDOB2P3 <- dfDataSel %>% 
    filter(ID == "Bin2" & date >= bin2Period2End) %>% 
    ggplot(aes(DO.mg.L, Selenium))+ 
    geom_point(alpha = 1) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("DO mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs DO linear regression for Bin2 Period C")
plotSelvDOB2P3
#conclusion: not enough data to back up the claims of higher DO leads to more 
#selenium reduction

#Bin6
plotSelvCODB6P4 <- dfDataSel %>% 
    filter(ID == "Bin6") %>% 
    filter(date >= bin1567Period3End) %>% 
    ggplot(aes(COD, Selenium))+ 
    geom_point(alpha = 1) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("COD") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD linear regression for Bin6 Period D")
plotSelvCODB6P4
ggsave(filename = "Images/Selenium vs COD linear regression Bin6 Period D.png", 
       plotSelvCODB6P4,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin3
plotSelvCODB1P1 <- dfDataSel %>% 
    filter(date <= bin1567Period1End & ID == "Bin3") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD linear regression of Bin1 Period A")
plotSelvCODB1P1
ggsave(filename = "Images/Selenium vs COD linear regression Bin1 Period A.png", 
       plotSelvCODB1P1,
       width = 42.3, height = 23.15, units = "cm", device='png')

#----------------------------------------------

#Selenium contents for individual bins
#Focus: bin 3
#Reason: bin3 is our baseline
#Period 1











#--------------------------------------
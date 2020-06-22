#Code Review for Goodyear team

#Change folder if needed.
setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("clean/goodyearMoWater0.rda" )
load("clean/cleanedObjects.rda" )
ls()
library( tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library( lubridate)
library( rcartocolor)
library( RColorBrewer)
library( viridis)
library( scales)
library( rstatix)
library( dplyr)
library( ggpubr)
suppressMessages(library( fields))

#--- Linear Regression And Modeling ----

#Performing lm on all variables/columns besides ID,date and Selenium, and store result
mods <- lapply(dfDataSel[, c(3, 5:ncol(dfDataSel))], 
               function(x) summary(lm(dfDataSel$Selenium ~ x)))
mods

#store the coeffcient of lm results
coefMat <- lapply( mods, coef)
coefMat

#store the r-squaredvalues
rssMat <- lapply( mods, "[[", "r.squared")
rssMat

save(mods, coefMat, rssMat, file = "clean/lmResults.rda")

#---------------------------------------

#--- t-test ---
#uses library( rstatix)

binsTTest <- dfDataSel %>% 
    t_test(Selenium ~ ID) %>%
    add_significance()
binsTTest

save(binsTTest, file = "clean/ttestResults.rda")
#---------------------------------------

# --- Functions ---
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
        geom_point(alpha = 0.7, size = 3) +  
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
        geom_point(alpha = 0.7, size = 3) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") +  
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 2 for period after train change") + 
        geom_vline(xintercept = bin1567Period1End, color = "#D95F02", linetype = "dashed") + 
        geom_vline(xintercept = bin1567Period2End, color = "#222222" ) + 
        geom_vline(xintercept = bin1567Period3End, color = "#222222") + 
        annotate("text", label = "B6A", x = bin1567Period1End - 80, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B6B", x = bin1567Period2End - 80, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B6C", x = bin1567Period3End - 80, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B6D", x = lastDate, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B2A", x = bin2Period1End - 80, y = 0.145, 
                 size = 8, colour = "#1B9E77") + 
        annotate("text", label = "B2B", x = bin2Period2End - 80, y = 0.145, 
                 size = 8, colour = "#1B9E77") + 
        annotate("text", label = "B2C", x = lastDate, y = 0.145, 
                 size = 8, colour = "#1B9E77")
    
    return(resultPlot) 
}

GGPTrain3VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin4" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(alpha = 0.7, size = 3) +  
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
        geom_point(alpha = 0.7, size = 3) +  
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
#log ver
plotT1vBrineLog <- plotT1vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 1 for period after train change")
plotT1vBrineLog
ggsave(filename = "Images/Train 1 Selenium vs Brine in log.png", 
       plotT1vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train2
plotT2vBrine <- GGPTrain2VSBrine(dfDataSel, "4 months", "Selenium")
plotT2vBrine
ggsave(filename = "Images/Train 2 Selenium vs Brine.png", 
       plotT2vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')
#log ver
plotT2vBrineLog <- plotT2vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 2 for period after train change")
plotT2vBrineLog
ggsave(filename = "Images/Train 2 Selenium vs Brine in log.png", 
       plotT2vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train3
plotT3vBrine <- GGPTrain3VSBrine(dfDataSel, "4 months", "Selenium")
plotT3vBrine
ggsave(filename = "Images/Train 3 Selenium vs Brine.png", 
       plotT3vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')
#log ver
plotT3vBrineLog <- plotT3vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 3 for period after train change")
plotT3vBrineLog
ggsave(filename = "Images/Train 3 Selenium vs Brine in log.png", 
       plotT3vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train 4
plotT4vBrine <- GGPTrain4VSBrine(dfDataSel, "4 months", "Selenium")
plotT4vBrine
ggsave(filename = "Images/Train 4 Selenium vs Brine.png", 
       plotT4vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')

#log ver
plotT4vBrineLog <- plotT4vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 4 for period after train change")
plotT4vBrineLog
ggsave(filename = "Images/Train 4 Selenium vs Brine in log.png", 
       plotT4vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#----------------------------------------------

#--- Boxplot ---

# sample code for reordering boxplot
# data$names <- factor(data$names , levels=c("A", "D", "C", "B"))
# 
# #The plot is now ordered !
# boxplot(data$value ~ data$names , col=rgb(0.3,0.5,0.4,0.6) , ylab="value" , 
#        xlab="names in desired order")


#boxplot on bons to see which bins to use - 2 tone color
boxSel2t <- dfDataSel %>% 
    filter(year(date) <= 2014 | year(date) >= 2015) %>%
    ggplot(aes(x = ID , y = Selenium, group = ID, color = ID)) +
    geom_boxplot(fill = "grey") +
    scale_color_manual(values = c(rep("black", 7), "red3")) + 
    ylab("Selenium (mg/L)") +
    labs(title = "Selenium Boxplots per Effluent") + 
    theme(legend.position = "none", axis.text=element_text(size=14))

boxSel2t

ggsave(filename = "Images/Selenium_Boxplot_two_tone.png", 
       boxSel2t,
       width = 42.3, height = 23.15, units = "cm", device='png')

#multiple color of boxplot base on train
boxSelTC <- dfDataSel %>% 
    filter(year(date) <= 2014 | year(date) >= 2015) %>%
    ggplot(aes(x = ID , y = Selenium, group = ID, color = ID)) +
    geom_boxplot(fill = c("#D95F02", "darkgreen", "royalblue3", 
                          "red4", "#D95F02", "darkgreen", 
                          "#D95F02", "black")) +
    scale_color_manual(values = c(rep("black", 7), "darkgrey")) + 
    ylab("Selenium (mg/L)") +
    labs(title= "Boxplot: Selenium by Effluent") +
    theme(legend.position = "none", axis.text=element_text(size=14))

boxSelTC

ggsave(filename = "Images/Selenium_Boxplot_train_color.png", 
       boxSelTC,
       width = 42.3, height = 23.15, units = "cm", device='png')

#log ver
boxSelTCLog <- boxSelTC + scale_y_continuous(trans = 'log10') + 
    labs(title= "Boxplot: Log of Selenium by Effluent")
boxSelTCLog
ggsave(filename = "Images/Selenium_Boxplot_train_color_log.png", 
       boxSelTCLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#different boxplot grouped by train type
dfTest <- dfDataSel
dfTest$ID <- factor(dfTest$ID , levels=c("Bin1", "Bin5", "Bin7", "Bin2", 
                                         "Bin6", "Bin4", "Bin3", "brine"))

boxSelTCGroup <- dfTest %>%
    ggplot(aes(x = ID, y = Selenium)) +
    geom_boxplot(fill = c("#D95F02", "#D95F02", "#D95F02", 
                          "darkgreen", "darkgreen", "royalblue3", 
                          "red3", "black")) +
    xlab("Train Type") +
    ylab("Selenium Content (mg/L)") +
    labs(title= "Boxplot: Selenium by Effluent by Train") +
    theme(legend.position = "none", axis.text=element_text(size=14))

boxSelTCGroup

ggsave(filename = "Images/Selenium_Boxplot_train_color_group.png", 
       boxSelTCGroup,
       width = 42.3, height = 23.15, units = "cm", device='png')

#log ver
boxSelTCGroupLog <- boxSelTCGroup + scale_y_continuous(trans = 'log10') + 
    labs(title= "Boxplot: Log of Selenium by Effluent by train type")
boxSelTCGroupLog
ggsave(filename = "Images/Selenium_Boxplot_train_color_group_log.png", 
       boxSelTCGroupLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#---------------------------------------------

#--- graphs of time based facet models ---

#This shows if the correlations changes base on vegetation type or train type.

GGPSelVSVarByYear<- function(dataset, target){
    result <- dataset %>% 
        filter(ID != "brine") %>% 
        ggplot(aes_string(target, "Selenium")) +
        geom_point(alpha = 1, aes(color = month(date, label = TRUE))) + # plot factor by month
        facet_wrap(~year(date), 4) + # use wrap when faceting by one variable
        scale_color_manual(values = plasma(15)) + #value is 15 to avoid using the lighter colors
        theme(
            panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white")
        ) + 
        ylab("Selenium mg/L")
    
    return(result)
}

#nitrate is key
plotSelvNit <- GGPSelVSVarByYear(dfDataSel, "Nitrate")
plotSelvNit + xlab("Nitrate mg/L") +
    labs(title = "Nitrate vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs Nitrate.png", 
       plotSelvNit,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Arsenic
plotSelvArs <- GGPSelVSVarByYear(dfDataSel, "Arsenic")
plotSelvArs + xlab("Arsenic mg/L") +
    labs(title = "Arsenic vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs Arsenic.png", 
       plotSelvArs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#COD
plotSelvCOD <- GGPSelVSVarByYear(dfDataSel, "COD")
plotSelvCOD + xlab("COD mg/L") +
    labs(title = "COD vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs COD.png", 
       plotSelvCOD,
       width = 42.3, height = 23.15, units = "cm", device='png')
#pH
plotSelvpH <- GGPSelVSVarByYear(dfDataSel, "pH")
plotSelvpH + xlab("pH") +
    labs(title = "pH vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs pH.png", 
       plotSelvpH,
       width = 42.3, height = 23.15, units = "cm", device='png')

#DO
plotSelvDOmgl <- GGPSelVSVarByYear(dfDataSel, "DO.mg.L")
plotSelvDOmgl + xlab("DO mg/L") +
    labs(title = "DO vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs DO.png", 
       plotSelvDOmgl,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Temp
plotSelvT <- GGPSelVSVarByYear(dfDataSel, "Temp..Celsius")
plotSelvT + xlab("Temp..Celsius") +
    labs(title = "Temp vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs Temp.png", 
       plotSelvT,
       width = 42.3, height = 23.15, units = "cm", device='png')

#------------------------------------------------

# Mean and Standard Deviation Pointrange Plot

#Collect Means for Each bin

meanb1<-mean(na.omit(Bin1$Selenium)) 
meanb2<-mean(na.omit(Bin2$Selenium))
meanb3<-mean(na.omit(Bin3$Selenium))
meanb4<-mean(na.omit(Bin4$Selenium))
meanb5<-mean(na.omit(Bin5$Selenium))
meanb6<-mean(na.omit(Bin6$Selenium))
meanb7<-mean(na.omit(Bin7$Selenium))
meanbrine<-mean(na.omit(Brine$Selenium))

meanTb<-c(meanb1,meanb2,meanb3,meanb4,meanb5,meanb6,meanb7,meanbrine)

#Collect Standard Deviations for Each Bin

sdb1<-sd(na.omit(Bin1$Selenium)) 
sdb2<-sd(na.omit(Bin2$Selenium))
sdb3<-sd(na.omit(Bin3$Selenium))
sdb4<-sd(na.omit(Bin4$Selenium))
sdb5<-sd(na.omit(Bin5$Selenium))
sdb6<-sd(na.omit(Bin6$Selenium))
sdb7<-sd(na.omit(Bin7$Selenium))
sdbrine<-sd(na.omit(Brine$Selenium))

sdTb<-c(sdb1,sdb2,sdb3,sdb4,sdb5,sdb6,sdb7,sdbrine)

ID <- c("Bin 1", "Bin 2", "Bin 3", "Bin 4", "Bin 5", "Bin 6", "Bin 7", "Brine")

#Create a Data Frame That Contains all the necessary information

dfSummary <- as_data_frame(ID)

dfSummary <- add_column(dfSummary, meanTb)

dfSummary <- add_column(dfSummary, sdTb)

dfTest <- dfSummary

dfTest$value <- factor(dfTest$value, levels=c("Bin 1", "Bin 5", "Bin 7", "Bin 2", "Bin 6", "Bin 4", "Bin 3", "Brine"))

#Create the pointrange plot that shows both mean and standard deviation

dfTest %>%
    ggplot(aes(x = value, y = (meanTb))) +
    geom_pointrange(aes(ymin = meanTb - sdTb, 
                        ymax = meanTb + sdTb, 
                        color = value)) +
    geom_line(aes(y=0), color = "green") +
    theme_minimal() +
    scale_color_manual(values = c("#D95F02", "#D95F02", "#D95F02", 
                                  "darkgreen", "darkgreen", "royalblue3", 
                                  "red3", "black")) +
    xlab("Bin Type") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Mean and Standard Deviation Plot of Selenium Content")

# Regression Plots and Exploratory Methods Analysis

#Create Variable for Train Grouping

dfDataSel$TrainGroup <- ifelse(dfDataSel$ID == "Bin1"| dfDataSel$ID == "Bin5" |
                                   dfDataSel$ID == "Bin7", "Train 1",
                               ifelse(dfDataSel$ID == "Bin2" | 
                                          dfDataSel$ID == "Bin6", "Train 2", 
                                      ifelse(dfDataSel$ID == "Bin4", "Train 3", 
                                             ifelse(dfDataSel$ID == "Bin3", 
                                                    "Train 4", "Brine"))))

#---------------------------------------------------------------------------
#Create Two Regression Models for COD mapped onto Selenium and summary comparisons
#Note That Each variable has two graphs and models: one with brine and train 4 and one without brine and train 4
#Note that we chose this subset of 5 variables because they all had the lowest p-values when compared with Selenium

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    ggplot(aes(x = COD, y = Selenium, col = TrainGroup))+
    geom_point(alpha = 0.15) +
    geom_smooth(method = "lm") +
    scale_color_manual(values = c("#D95F02", 
                                  "darkgreen",
                                  "royalblue3", 
                                  "red3")) +
    xlab("COD") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping COD onto Selenium Content")

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    filter(TrainGroup != "Train 4") %>%
    ggplot(aes(x = COD, y = Selenium))+
    geom_point(alpha = 0.15) +
    geom_smooth(method = "lm") +
    xlab("COD") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping COD onto Selenium Content")

COD_model <- lm(Selenium ~ COD, data = dfDataSel)
COD_nmodel <- dfDataSel %>% filter(TrainGroup != "Brine") %>% filter(TrainGroup != "Train 4") %>% lm(Selenium ~ COD, data = .)

summary(COD_model)
summary(COD_nmodel)

dev.off()

#------------------------------------------------------------------------------------
#Create Two Regression Models for Phosphorus mapped onto Selenium and summary comparisons

pdf(file = "/Users/Blake Loosley/Documents/School/Data Science Internship/Images/Phosphorus_Regression.pdf",
    width = 12,
    height = 8)

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    ggplot(aes(x = log(Phosphorus), y = Selenium, col = TrainGroup))+
    geom_point(alpha = .15) +
    geom_smooth(method = "lm") +
    scale_color_manual(values = c("#D95F02", 
                                  "darkgreen",
                                  "royalblue3", 
                                  "red3")) +
    xlab("Phosphorus (mg/L)") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping Phosphorus onto Selenium Content")

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    filter(TrainGroup != "Train 4") %>%
    ggplot(aes(x = log(Phosphorus), y = Selenium))+
    geom_point(alpha = .15) +
    geom_smooth(method = "lm") +
    xlab("Phosphorus (mg/L)") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping Phosphorus onto Selenium Content")

Phos_model <- lm(Selenium ~ Phosphorus, data = dfDataSel)
Phos_nmodel <- dfDataSel %>% filter(TrainGroup != "Brine") %>% filter(TrainGroup != "Train 4") %>% lm(Selenium ~ Phosphorus, data = .)

summary(Phos_model)
summary(Phos_nmodel)

dev.off()

#----------------------------------------------------------------------------------
#Create Two Regression Models for TDS mapped onto Selenium and summary comparisons

pdf(file = "/Users/Blake Loosley/Documents/School/Data Science Internship/Images/TDS_Regression.pdf",
    width = 12,
    height = 8)

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    ggplot(aes(x = TDS, y = Selenium, col = TrainGroup))+
    geom_point(alpha = .15) +
    geom_smooth(method = "lm") +
    scale_color_manual(values = c("#D95F02", 
                                  "darkgreen",
                                  "royalblue3", 
                                  "red3")) +
    xlab("TDS") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping TDS onto Selenium Content")

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    filter(TrainGroup != "Train 4") %>%
    ggplot(aes(x = TDS, y = Selenium))+
    geom_point(alpha = .15) +
    geom_smooth(method = "lm") +
    xlab("TDS") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping TDS onto Selenium Content")

TDS_model <- lm(Selenium ~ TDS, data = dfDataSel)
TDS_nmodel <- dfDataSel %>% filter(TrainGroup != "Brine") %>% filter(TrainGroup != "Train 4") %>% lm(Selenium ~ TDS, data = .)

summary(TDS_model)
summary(TDS_nmodel)

dev.off()

#---------------------------------------------------------------------------------------
#Create Two Regression Models for Nitrate mapped onto Selenium and summary comparisons

pdf(file = "/Users/Blake Loosley/Documents/School/Data Science Internship/Images/Nitrate_Regression.pdf",
    width = 12,
    height = 8)

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    ggplot(aes(x = Nitrate, y = Selenium, col = TrainGroup))+
    geom_point(alpha = 0.15) +
    geom_smooth(method = "lm") +
    scale_color_manual(values = c("#D95F02", 
                                  "darkgreen",
                                  "royalblue3", 
                                  "red3")) +
    xlab("Nitrate") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping Nitrate onto Selenium Content")

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    filter(TrainGroup != "Train 4") %>%
    ggplot(aes(x = Nitrate, y = Selenium))+
    geom_point(alpha = 0.15) +
    geom_smooth(method = "lm") +
    xlab("Nitrate") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping Nitrate onto Selenium Content")

Nitrate_model <- lm(Selenium ~ Nitrate, data = dfDataSel)
Nitrate_nmodel <- dfDataSel %>% filter(TrainGroup != "Brine") %>% filter(TrainGroup != "Train 4") %>% lm(Selenium ~ Nitrate, data = .)

summary(Nitrate_model)
summary(Nitrate_nmodel)

dev.off()

#-------------------------------------------------------------------------------------
#Create Two Regression Models for pH mapped onto Selenium and summary comparisons

dfDataSel %>%
    filter(TrainGroup != "Brine") %>%
    ggplot(aes(x = pH, y = Selenium, col = TrainGroup))+
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    scale_color_manual(values = c("#D95F02", 
                                  "darkgreen",
                                  "royalblue3", 
                                  "red3")) +
    xlab("pH") +
    ylab("Selenium Content (mg/L)") +
    labs(title = "Linear Regression Model Mapping pH onto Selenium Content")

#-------------------------------------------------------------------------------------
#Calculated some models that contained all lowest p-value variables and played 
#around with combinations of variables to make a better model

mult_model <- lm(Selenium ~ pH + COD + Phosphorus + Nitrate + TDS, data=dfDataSel)
summary(mult_model)

mult_nmodel <- dfDataSel %>% filter(TrainGroup != "Brine") %>% filter(TrainGroup != "Train 4") %>% lm(Selenium ~ pH + COD + Phosphorus + Nitrate + TDS, data = .)
summary(mult_nmodel)

test_model <- lm(Selenium ~ COD + Phosphorus + Nitrate, data = dfDataSel)
summary(test_model)

#----------------------------------------------

#--- Hypothesis Testing ---

#Bin1 - Brine Comparison
t.test(Brine$Selenium, Bin1$Selenium)
#Not Significant

#Bin5 - Bin1 Comparison
t.test(Bin1$Selenium, Bin5$Selenium)
#Not Significant

#Bin7 - Bin5 Comparison
t.test(Bin5$Selenium, Bin7$Selenium)
#Not Significant

#Bin7 - Brine Comparison
t.test(Brine$Selenium, Bin7$Selenium)
#Not Significant

#Bin2 - Brine Comparison
t.test(Brine$Selenium, Bin2$Selenium)
#Significant

#Bin6 - Bin2 Comparison
t.test(Bin2$Selenium, Bin6$Selenium)
#Not Significant

#Bin6- Brine Comparison
t.test(Brine$Selenium, Bin6$Selenium)
#Significant

#Bin4 - Brine Comparison
t.test(Brine$Selenium, Bin4$Selenium)
#Significant

#Bin3 - Brine Comparison
t.test(Brine$Selenium, Bin3$Selenium)
#Not Significant

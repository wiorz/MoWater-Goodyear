load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedFactors.rda")

library(tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library(rcartocolor)
library(RColorBrewer)
library(viridis)
library(scales)
library(dplyr)
#library(ggpubr)
library(plotly) #for 3d plot
library(leaps) # for best subset regression

#-------------------------------
#train change date: relevant for bin 2 and 4. 
#Before the change, bin 2 is train 3,
#This can be our starting date since we will only be ignoring 8 months of data.
trainChangeDate <- ymd( "2011-06-15")

#unstable periods
unstablePeriodStart <- ymd( "2014-04-01")
unstablePeriodEnd <- ymd( "2016-01-01") #rough est. according to Kate (stakeholder)

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

#------------------------------------

#---Boxplot ---

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

#NOTE: If the following error occurs,check the file saving location:
# grDevices::png(..., res = dpi, units = "in") : 
#     unable to start png() device
#The size below is in inches, but the next plot is in cm.
#They are both same for 1920 x 1080 px.
ggsave(filename = "Baylor/MoWater/proj6/MoWater-Goodyear/Images/Selenium_Boxplot_train_color_group.png", 
       boxSelTCGroup,
       width = 25.6, height = 14.4, units = "in", device='png')

#log ver
boxSelTCGroupLog <- boxSelTCGroup + scale_y_continuous(trans = 'log10') + 
    labs(title= "Boxplot: Log of Selenium by Effluent by train type")
boxSelTCGroupLog
ggsave(filename = "Baylor/MoWater/proj6/MoWater-Goodyear/Images/Selenium_Boxplot_train_color_group_log.png", 
       boxSelTCGroupLog,
       width = 33.87, height = 19.05, units = "cm", device='png')

#--------------------------------------------------------

#---Best Subset Regression ---

GetLeapTable <- function(leapSummaryIn){
    result <- cbind(leapSummaryIn$adjr2, leapSummaryIn$cp, leapSummaryIn$bic)
    return(result)
}

GetMinMax <- function(leapSummaryIn){
    result <- data.frame(
        Adj.R2 = which.max(leapSummaryIn$adjr2),
        CP = which.min(leapSummaryIn$cp),
        BIC = which.min(leapSummaryIn$bic)
    )
    return(result)
}

#Just using veg, no media
leapsResultVeg <- regsubsets(Selenium ~ pH + DO.mg.L + Temp..Celsius + Nitrate + 
                                 COD + Phosphorus + Arsenic + Veg,
                             data = dfDataSel, nvmax = 10)
# view results
leapSummaryVeg <- summary(leapsResultVeg)
leapTableVeg <- GetLeapTable(leapSummaryVeg)

#Find the min and max
minMaxLeapVeg <- GetMinMax(leapSummaryVeg)
minMaxLeapVeg
#Adj.R2 CP BIC
#6      5  5
#---

leapTableVeg
#[5,] 0.953  2.235 -41.444
#[6,] 0.954  3.516 -40.389

leapSummaryVeg
#Temp, Nit, COD, Arsenic, Veg Type C
#pH, Temp, Nit, COD, Arsenic, Veg Type C

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultVeg, scale = "Cp", 
     main = "10 Best Subsets Regression on Selenium")


#--------------------------------------------------------
#--- 3D plot ---

colorsScale <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

figNCV <- plot_ly(dfT, x = ~Nitrate, y = ~COD, 
                  z = ~Selenium, color = ~Veg, colors = colorsScale)
figNCV <- figNCV %>% add_markers()
figNCV <- figNCV %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                         yaxis = list(title = 'COD'),
                                         zaxis = list(title = 'Selenium mg/L')))
figNCV

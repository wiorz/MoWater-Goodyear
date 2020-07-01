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

leapsResultL <- regsubsets(Selenium ~ Nitrate + COD + Phosphorus + Arsenic + 
                               Veg + MediaType,
                           data = dfCLong, nvmax = 5)
# view results
leapSummaryL <- summary(leapsResultL)
leapTableL <- GetLeapTable(leapSummaryL)
minMaxLeapL <- GetMinMax(leapSummaryL)

minMaxLeapL
#minMaxLeap result
#max adj.r2     min cp      min bic
#5              5           3
#---
leapTableL 
# [3,] 0.589 12.143 -205.610
# [5,] 0.602  5.601 -205.134
#---
leapSummaryL
#[3,] Nit, COD, Phosphorus
#[5,] Nit, COD, Phosphorus, Arsenic, Veg Type C

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultL, scale = "adjr2", cex.axis = 3, 
     main = "5 Best Subsets Regression on Selenium with 253 observ by Adjusted R-Square")


dfCLong$VegTypeCTrue <- dfCLong$Veg == "VegType_C"
FinalSubsetsModel <- lm(Selenium ~ Nitrate + COD + Phosphorus + Arsenic + VegTypeCTrue, data = dfCLong)
summary(FinalSubsetsModel)


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

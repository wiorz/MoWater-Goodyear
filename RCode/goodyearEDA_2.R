setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("clean/goodyearMoWater.rda" )
ls()
library( lubridate)
library( viridis)
library( scales)
library( tidyverse)
suppressMessages(library( fields))

head(goodyear)

dfData <- as_tibble(goodyear)
glimpse(dfData)

dfDataC <- dfData[!is.na(dfData$Selenium), ]
dfDataC

colnames(dfDataC)

for(i in colnames(dfDataC)){
    cur_plot <- ggplot(data = dfDataC, aes_string(x = i, y = "Selenium")) + #aes_string because variable is not actual column factor term, but a name as string.
        geom_line(color = ID ) + 
        geom_point(alpha = 0.3) + 
        geom_smooth(method = lm, color = "blue")
    print(cur_plot) # autoprint is turned off so we need to print it
    #Sys.sleep(2) #sleep so that we can see the output if needed
    
}

dfDataC %>% 
    filter(dfDataC$ID == "Bin1" | dfDataC$ID == "Bin5" | dfDataC$ID == "Bin7" | 
               dfDataC$ID == "brine") %>% 
    ggplot(aes(x = date, y = Arsenic, group = ID, color = ID)) + #aes_string because variable is not actual column factor term, but a name as string.
    geom_line(color=viridis(539))
    

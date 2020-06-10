setwd("~/Dropbox/Home/Projects/Goodyear")
source("createBinData.R")
source("createBrineData.R")
library( lubridate) 
library( stringr)

# the csv files read in here have been extracted from the master .xlsx file
# as part of this operation  #N/A  replaced by NA so the R missing value code is used. 
#

Brine<- createBrineData()
cat("created object ", " Brine", fill=TRUE)

Bin1<- createBinData1( 1)
Bin2<- createBinData1( 2)
Bin3<- createBinData1( 3)
Bin4<- createBinData1( 4)
Bin5<- createBinData1( 5)
Bin6<- createBinData1( 6)
Bin7<- createBinData1( 7)

cat("created objects ", " BinX  X= 1,2,...,7", fill=TRUE)

# Modify column names to be the same
# Many redundant date columns -- just keep "date"

  Brine$H2S.mg.L <- Brine$H2S.mg.l
  Brine$H2S.mg.l<- NULL
  
  Bin1$X.1<- NULL
  Bin1$X<- NULL
  Bin1$Date<- NULL
  Bin1$Date.1<- NULL
  Bin1$Date.2<- NULL
  
  Bin2$Date<- NULL
  Bin2$Date<- NULL
  Bin2$Date2<- NULL
  Bin2$X<- NULL
  
  Bin3$Date<- NULL
  Bin3$X<- NULL
  Bin3$Date.1<- NULL
  Bin3$Date.2<- NULL
  Bin3$ORP<- Bin3$ORP..mV.
  Bin3$ORP..mV.<- NULL
  
  Bin4$Date<- NULL
  Bin4$X<- NULL
  Bin4$Date.1<- NULL
  Bin4$Date.2<- NULL
  # Bin.5.pH ???
  Bin4$pH<- Bin4$Bin.5.pH
  Bin4$Bin.5.pH<- NULL
  # Bin.5.pH ????
  Bin4$Conductivity.S.m <- Bin4$Conductivity..S.m
  Bin4$Conductivity..S.m<- NULL

  Bin5$Date<- NULL
  Bin5$X<- NULL
  Bin5$Date.1<- NULL
  Bin5$Date.2<- NULL
  
  Bin5$Inflow<-Bin5$Inflow..Bin.1.
  Bin5$Inflow..Bin.1.<- NULL

  Bin6$Date<- NULL
  Bin6$X<- NULL
  Bin6$Date.1<- NULL
  Bin6$Date.2<- NULL
  
  Bin6$Inflow<- Bin6$Inflow..Bin.2.
  Bin6$Inflow..Bin.2.<- NULL
 
  
  Bin7$Date<- NULL
  Bin7$X<- NULL
  Bin7$Date.1<- NULL
  Bin7$Date.2<- NULL
  # rename Inflow variable
  Bin7$Inflow<- Bin7$Inflow..5.out.
  Bin7$Inflow..5.out.<- NULL
  Bin7$H2S.mg.L <- Bin7$H2S.mg.L2
  Bin7$H2S.mg.L2<- NULL
  

ID<-  c( 
  rep( "brine", nrow(Brine)),
  rep( "Bin1",  nrow(Bin1)),
  rep( "Bin2",  nrow(Bin2)),
  rep( "Bin3",  nrow(Bin3)),
  rep( "Bin4",  nrow(Bin4)),
  rep( "Bin5",  nrow(Bin5)),
  rep( "Bin6",  nrow(Bin6)),
  rep( "Bin7",  nrow(Bin7))
)
 ID<- factor(ID)
 
 goodyear<- cbind(ID,
             rbind(Brine,
                   Bin1,
                   Bin2,
                   Bin3,
                   Bin4, 
                   Bin5,
                   Bin6,
                   Bin7) 
             )

save( goodyear,
      Brine,
      Bin1, Bin2, Bin3, Bin4, Bin5, Bin6, Bin7, 
      file="goodyearMoWater.rda")






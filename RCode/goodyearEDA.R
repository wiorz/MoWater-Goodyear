
setwd("~/Dropbox/Home/Projects/Goodyear")
load("goodyearMoWater.rda" )
ls()
library( lubridate)
library( viridis)
library( scales)
suppressMessages(library( fields))

# primary data set
print( dim( goodyear))
# variable names
print(names( goodyear))
# observations by brine and bins
table( goodyear$ID)
# summary stats 
t( stats( goodyear))

# Selenium across different treatments
# could also use ggplot or lattice to make this 
colTab<- alpha( c(  "magenta", viridis(7)), .4)
plot(goodyear$date, 
     goodyear$Selenium,
     col=colTab[goodyear$ID], pch=16,
     xlab="Date", ylab="Selenium")

IDNames<- unique(goodyear$ID)
legend("topleft", 
       pch=16, col = colTab, 
       legend = IDNames 
      , inset=c( .05,.05) , ncol=2 )
title("Selenium by treatment")


periods<- ymd( c("2012-03-01", "2015-04-01", "2017-04-01" ) )

matplot( Bin1$date, Bin1[,c("Inflow", "Outflow") ],
         col=c("grey", "red"), type="p", pch=16,
         xlab="Date", ylab="Flow"
         )
abline( v= periods, col="grey", lwd=2)

legend("topleft", 
       pch=16, col =c("grey", "red") , 
       legend =  c("Inflow", "Outflow")
       , inset=c( .05,.05), bty="n" )
title( "Bin1 flow measurments")


matplot( Bin1$date, Bin1[,c("Inflow", "Outflow") ],
         col=c("grey", "red"), type="p", pch=16,
         xlab="Date", ylab="Flow", 
         ylim=c(0,.7)
)
legend("topleft", 
       pch=16, col =c("grey", "red") , 
       legend =  c("Inflow", "Outflow")
       , inset=c( .05,.05) )
title( "Bin1 flow measurments w/o outlier")
abline( v= periods, col="magenta", lwd=2)

 temp<- subset( goodyear, goodyear$ID=="Bin1")
 plot( temp$date, temp$TDS,xlab="date", ylab="TDS",pch=16)
 temp2<-  subset( goodyear, goodyear$ID=="brine")
 points( temp2$date, temp2$TDS, col="red", pch=16)
 legend("topleft", 
        pch=16, col =c("black", "red") , 
        legend =  c("Inflow", "Outflow")
        , inset=c( .05,.05) )
title( "Figure 17 --  Bin1 TDS ")
abline( v= periods, col="magenta", lwd=2)

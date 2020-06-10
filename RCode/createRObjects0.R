setwd("~/Dropbox/Home/Projects/Goodyear")
source("createBinData.R")
library( lubridate) 
library( stringr)

# the csv files read in here have been extracted from the master .xlsx file
# as part of this operation  #N/A  replaced by NA so the R missing value code is used. 
#
# skip first line
# data separated by commas
# any text strings preserve as a string

 look<- read.table( "rawFlow.csv",  skip=1, header=TRUE,
                    sep=",", stringsAsFactors = FALSE)
# omit second column
 look<- look[,-2]
 # clean up the column names omit periods and shorten
 colNames<- names( look)
 colNames[1]<- "date"
# replace names with cleaner versions
names( look)<- str_replace_all(colNames,coll("."),"")
# convert master date string to date object
# BTW second date column has missing dates. 
look$date<-as.Date(look$date, format="%m/%d/%y")
rawFlow<- look
cat("created object ", " rawFlow", fill=TRUE)
rm( look)

look<- read.table( "rawField.csv",  skip=3,header=TRUE,
                   sep=",", stringsAsFactors = FALSE)
# omit second column
look<- look[,-2]
colNames<- names( look)
colNames[1]<- "date"
names( look)<- str_replace_all(colNames,coll("."),"")
look$date<-as.Date(look$date, format="%m/%d/%y")
rawField<- look
cat("created object ", " rawField", fill=TRUE)
rm( look)


look<- read.table( "Brine.csv", header=TRUE,
                   sep=",", stringsAsFactors = FALSE)
look<- look[,-2]
colNames<- names( look)
colNames[1]<- "date"
names( look) <- str_replace_all(colNames,coll("Brine."),"")
dateObj<-as.Date(look$date, format="%m/%d/%y")
# coerce to numeric
for (  k in 1:ncol( look)){
  look[,k]<- as.numeric( look[,k])
}
look$date<- dateObj
look$Date.1<- NULL
look$Inflow<- rep( NA, nrow(look))
look$Outflow<- rep( NA, nrow(look))
Brine<- look

cat("created object ", " Brine", fill=TRUE)
rm( look)

Bin1<- createBinData( 1)
Bin2<- createBinData( 2)
Bin3<- createBinData( 3)
Bin4<- createBinData( 4)
Bin5<- createBinData( 5)
Bin6<- createBinData( 6)
Bin7<- createBinData( 7)

b<- names(Brine)
n1<- names( Bin1)  
n2<- names( Bin2)
n3<- names( Bin3)
n4<- names( Bin4)
n5<- names( Bin5)
n6<- names( Bin6)
n7<- names( Bin7)

allNames<- c(b,n1, n2,n3,n4,n5,n6,n7) 
# who has what
indBin<- c(
  rep( 0,length( b)),
  rep( 1,length( n1)),
  rep( 2,length( n2)),
  rep( 3,length( n3)),
  rep( 4,length( n4)),
  rep( 5,length( n5)),
  rep( 6,length( n6)),
  rep( 7,length( n7))
)

table( allNames, indBin)


# create data frames and modify column names to be the same
# Many redundantt date columns -- just keep "date"

Bin1<- createBinData( 1)
Bin2<- createBinData( 2)
Bin3<- createBinData( 3)
Bin4<- createBinData( 4)
Bin5<- createBinData( 5)
Bin6<- createBinData( 6)
Bin7<- createBinData( 7)

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
  
# recheck names 
b<- names(Brine)
n1<- names( Bin1)  
n2<- names( Bin2)
n3<- names( Bin3)
n4<- names( Bin4)
n5<- names( Bin5)
n6<- names( Bin6)
n7<- names( Bin7)

allNames<- c(b,n1, n2,n3,n4,n5,n6,n7) 
# who has what
indBin<- c(
   rep( 0,length( b)),
   rep( 1,length( n1)),
   rep( 2,length( n2)),
   rep( 3,length( n3)),
   rep( 4,length( n4)),
   rep( 5,length( n5)),
   rep( 6,length( n6)),
   rep( 7,length( n7))
)

table( allNames, indBin)

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
# coerce to numeric
 goodyear$Inflo
 
 
 
  


save( goodyear,
      rawFlow,rawField, 
      Brine,
      Bin1, Bin2, Bin3, Bin4, Bin5, Bin6, Bin7, 
      file="goodyearMoWater.rda")






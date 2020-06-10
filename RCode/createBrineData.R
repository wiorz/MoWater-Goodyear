createBrineData<- function(){
  
  
  lookRaw<- read.table( "Brine.csv", header=TRUE,
                     sep=",", stringsAsFactors = FALSE)
  look<- lookRaw
  look$Date<- NULL # extra Date column
  colNames<- names( look)
  colNames[1]<- "date"
  names( look) <- str_replace_all(colNames,coll("Brine."),"")
  dateObj<-as.Date(look$date, format="%m/%d/%y")
  # coerce to numeric
  for(  k in 1: ncol( look)){
    colData<- look[,k]
    # look for < sign in data 
    tempIndex<- which(substr(colData, 1, 1)=="<")
    indLessThan<- tempIndex[!is.na(tempIndex)]
    N<- length( indLessThan)
    cat( "Found ", length( indLessThan), 
         " '<'  in  column for ",k, colNames[k],
         fill=TRUE)
    # remove the < if found 
    if( N >0){
      colData[indLessThan]<- substring( colData[indLessThan], first=2 )
    }
    # now convert from character strings (representing numbers) to numeric  
   
    look[,k]<- as.numeric( colData[])
  }
  
  look$date<- dateObj
  look$Date.1<- NULL
  look$Inflow<- rep( NA, nrow(look))
  look$Outflow<- rep( NA, nrow(look))  
  
  print( length(names(look)))
  return( look)
}

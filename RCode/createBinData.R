createBinData1<- function( BinNumber){
# created DWN June 9, 2020  
  cat( "working on ", paste0("Bin", BinNumber,".csv") , fill=TRUE)
  #about<- scan(paste0("Bin", BinNumber,".csv"), what="a", sep="\n" )
  #cat( about)

  lookRaw<- read.table( paste0("Bin", BinNumber,".csv"), 
                     skip=2,
                     header=TRUE,
                     sep=",", stringsAsFactors = FALSE)
  look<- lookRaw
  colNames<- names( look)
  junkString<- paste0("Bin.",BinNumber,".")
  colNames<- str_replace( colNames,coll(junkString),"" )
  names(look) <- colNames
  # drop some columns
  dateObj<- as.Date(look$X, format="%m/%d/%y")
#coerce to numeric but remove the < leading character if present
  for(  k in 1: ncol( look)){
    colData<- look[,k]
# look for < sign in data 
     tempIndex<- which(substr(colData, 1, 1)=="<")
     indLessThan<- tempIndex[!is.na(tempIndex)]
    N<- length( indLessThan)
    cat( "Found ", length( indLessThan), 
           " '<'  in  column for ", colNames[k],
             fill=TRUE)
    # remove the < if found 
    if( N >0){
    colData[indLessThan]<- substring( colData[indLessThan], first=2 )
    }
   # now convert from character strings (representing numbers) to numeric  
    #ind<- !is.na( colData)
    look[,k]<-  as.numeric( colData)
    
  }
  look$date<- dateObj
  print( length(names(look)))
  return( look)
}

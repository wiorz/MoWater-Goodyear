createBinData<- function( BinNumber){
  cat( "working on ", paste0("Bin", BinNumber,".csv") , fill=TRUE)
  #about<- scan(paste0("Bin", BinNumber,".csv"), what="a", sep="\n" )
  #cat( about)

  look<- read.table( paste0("Bin", BinNumber,".csv"), 
                     skip=2,
                     header=TRUE,
                     sep=",", stringsAsFactors = FALSE)
  colNames<- names( look)
  junkString<- paste0("Bin.",BinNumber,".")
  colNames<- str_replace( colNames,coll(junkString),"" )
  names(look) <- colNames
  # drop some columns
  dateObj<- as.Date(look$X, format="%m/%d/%y")
  #coerce to numeric
  for(  k in 1: ncol( look)){
    look[,k]<- as.numeric( look[,k])
  }
  look$date<- dateObj
  print( length(names(look)))
  return( look)
}

library(RCurl)
library(RJSONIO)
library(RCurl)

#preapre the data to send to google for geocoding
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

#funciton that takes the address, sends to google and saves results, looking at whether the returned codes are accurate or if there was a problem
gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
#     print(paste("exit code:",x$status))
    return(c(NA,NA))
    
  }
}

houses <- read.csv(file='Charleston.csv',stringsAsFactors=F)


hcoord <- t(apply(as.data.frame(houses$Address), 1, gGeoCode))
colnames(hcoord) <- c('Latitude','Longitude')

fin <- cbind(houses,hcoord)

good <- subset(fin,!is.na(fin$Latitude))
bad  <- subset(fin,is.na(fin$Latitude))
print(paste("There were ",length(bad[,1])," bad geocodes..."))
i <- 0
while(length(bad[,1]>0)){
  i <- i+1
 
  crd <- t(apply(as.data.frame(bad$Address), 1, gGeoCode))
  colnames(crd) <- c('Latitude','Longitude')
  f <- cbind(bad[,c(1:6)],crd)
  
  good <- rbind(good,subset(f,!is.na(f$Latitude)))
  bad <- subset(f,is.na(f$Latitude))
  print(paste("Iteration:",i," NumBad:",length(bad[,1])))
}  
print(paste("Original:",length(houses[,1])," Final",length(good[,1])))

# write out the results to be grabbed by Tableau
write.csv(good,file='C:\\Users\\scroker\\Documents\\R\\Geo\\CharlestonGeo.csv')


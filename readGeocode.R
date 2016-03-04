library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

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

houses <- read.csv(file='C:\\Users\\scroker\\Documents\\R\\Geo\\Charleston.csv',stringsAsFactors=F)


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


# 
# hcoord2 <- t(apply(as.data.frame(bad$Address), 1, gGeoCode))
# colnames(hcoord2) <- c('Latitude','Longitude')
# fin2 <- cbind(bad[,c(1:5)],hcoord2)
# 
# good2 <- subset(fin2,!is.na(fin2$Latitude))
# bad2 <- subset(fin2,is.na(fin2$Latitude))
# 
# hcoord3 <- t(apply(as.data.frame(bad2$Address), 1, gGeoCode))
# colnames(hcoord3) <- c('Latitude','Longitude')
# fin3 <- cbind(bad2[,c(1:5)],hcoord3)
# 
# final <- rbind(good,good2,fin3)

# write out the results to be grabbed by Tableau
write.csv(good,file='C:\\Users\\scroker\\Documents\\R\\Geo\\CharlestonGeo.csv')


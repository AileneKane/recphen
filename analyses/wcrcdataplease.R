## Started April 2019 ##
## by Ailene ##

## Source file for reading in the data and
## formatting datafile for stan

#add week to dataset
d$week<-strftime(strptime(d$date,format= "%Y-%m-%d"), format = "%V")#new weeks start on mondays
d<-d[which(is.na(d$CRCArea)==FALSE),]
d<-d[d$date!="NANA",]

# separate fish by species to put the fish caught 
anglers = aggregate(d$Anglers, list(paste(d$year,d$week,d$CRCArea, sep="")),sum, na.rm=TRUE)
chinook = aggregate(d$ChinCaught, list(paste(d$year,d$week,d$CRCArea, sep="")),sum, na.rm=TRUE)
coho= aggregate(d$CohoCaught, list(paste(d$year,d$week,d$CRCArea, sep="")),sum, na.rm=TRUE)
#add other species from d$OtherSpCaught
fishsum<-cbind(anglers,chinook$x,coho$x)
colnames(fishsum)<-c("date","anglers","chin","coho")
fishsum$year<-substr(fishsum$date,1,4)
fishsum$week<-substr(fishsum$date,5,6)
fishsum$area<-substr(fishsum$date,7,nchar(fishsum$date))
dim(fishsum)
#fishsum<- fishsum [apply(fishsum , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#for an annual sum, across all fishing areas
# separate fish by species to put the fish caught 
anglers.yr = aggregate(d$Anglers, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
chinook.yr = aggregate(d$ChinCaught, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
coho.yr= aggregate(d$CohoCaught, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
#add other species from d$OtherSpCaught
fishsum.yr<-cbind(anglers.yr,chinook.yr$x,coho.yr$x)
colnames(fishsum.yr)<-c("date","anglers","chin","coho")
fishsum.yr$year<-substr(fishsum.yr$date,1,4)
fishsum.yr$week<-substr(fishsum.yr$date,5,6)
dim(fishsum.yr)
#fishsum<- fishsum [apply(fishsum , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
